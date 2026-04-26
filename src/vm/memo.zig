/// Shared packrat-memoization state used by the JIT path.
///
/// Mirrors the VM's `memo_table` / memo-frame machinery (`Vm.MemoEntry`,
/// the `.memo` `Entry` variant, etc.) in a non-generic, extern-friendly
/// shape so the JIT-emitted code can read and write it via the C ABI.
const std = @import("std");
const events_mod = @import("events.zig");
const CaptureTree = @import("CaptureTree.zig");
const jit_abi = @import("jit_abi.zig");

pub const State = enum(u8) {
    empty = 0,
    /// In-progress evaluation; produced and consumed by Warth's
    /// left-recursion handling (`setupLrVm`, recall, grow).
    lr = 1,
    success = 2,
    fail = 3,
};

/// One memoized (rule_id, position) entry. Layout is fixed at 16 bytes
/// so the JIT can index into the table with a constant stride. The
/// `events_*` fields are unused when `capture_events` is off.
pub const Entry = extern struct {
    state: State,
    _pad: [3]u8 = .{ 0, 0, 0 },
    /// Meaning depends on `state`:
    ///   .success -> end position of the cached match
    ///   .lr      -> stack index of the live memo frame
    ///   other    -> unused
    next_pos_or_frame: u32,
    /// Slice of `events_buf` holding the events the memoized body
    /// produced. Replayed on a `.success` cache hit so the live event
    /// log matches what re-running the body would produce. Zero when
    /// capture_events is off.
    events_start: u32 = 0,
    events_count: u32 = 0,
};

comptime {
    if (@sizeOf(Entry) != 16) @compileError("memo.Entry must be 16 bytes");
}

/// Side-table memo frame data. Lives in a parallel
/// `[max_stack]Frame` indexed by stack depth at push time, so the
/// JIT's 32-byte StackEntry can stay unchanged across opcode push
/// sites: a memo stack entry just stores `tag=5` and the side index
/// (= the entry's own depth) in `val1`.
///
/// Sized to 16 bytes so the JIT can index the side table with a
/// `shift-by-4` (x16) rather than an integer multiply.
pub const Frame = extern struct {
    rule_id: u32,
    start_pos: u32,
    return_pc: u32, // bytecode PC to resume at after the rule completes
    events_len_at_entry: u32,
};

comptime {
    if (@sizeOf(Frame) != 16) @compileError("memo.Frame must be 16 bytes");
}

/// `helperMemoLookup` action codes.
pub const lookup_miss: u64 = 0;
pub const lookup_fail: u64 = 1;
pub const lookup_success: u64 = 2;
/// Returned when an in-progress `.lr` entry is hit. The caller invokes
/// the LR setup path (attach the frame to a head, return its current
/// seed) instead of treating the lookup like a normal cache hit.
pub const lookup_lr: u64 = 3;

/// Append-only buffer of events captured by memoized rule completions.
/// A `.success` `Entry` indexes into this via `events_start /
/// events_count`. Append-only because earlier slices stay live as
/// later memo entries are added.
pub const EventsBuf = struct {
    list: std.ArrayListUnmanaged(CaptureTree.Event) = .empty,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) EventsBuf {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *EventsBuf) void {
        self.list.deinit(self.allocator);
    }
};

/// JIT-side memo lookup. Reads `table[idx]` and dispatches:
///   .empty   -> returns `lookup_miss`. Caller pushes a memo frame and
///               jumps to the rule body.
///   .success -> writes the cached end position to `*end_pos_out` and
///               returns `lookup_success`. The caller separately
///               replays any cached event range via `helperMemoCachedSlice`
///               + `helperMemoReplayEvents`.
///   .fail    -> returns `lookup_fail`. Caller routes to the backtrack
///               handler.
///   .lr      -> returns `lookup_lr`. Caller invokes the LR setup
///               path with the in-progress frame's stack index.
///
/// `idx` is precomputed by the JIT as `rule_id * stride + pos`.
pub fn helperMemoLookup(
    table_ptr: [*]const Entry,
    idx: u64,
    end_pos_out: *u64,
) callconv(.c) u64 {
    const entry = table_ptr[@intCast(idx)];
    switch (entry.state) {
        .empty => return lookup_miss,
        .fail => return lookup_fail,
        .success => {
            end_pos_out.* = entry.next_pos_or_frame;
            return lookup_success;
        },
        .lr => return lookup_lr,
    }
}

/// Read a `.success` entry's cached event range. Returns the slice
/// length, or zero when capture_events is off / nothing was cached.
/// `out_start` receives the index into `events_buf` of the first
/// cached event. The JIT calls this after `helperMemoLookup` returns
/// `lookup_success`, then forwards the slice to `helperMemoReplayEvents`.
pub fn helperMemoCachedSlice(
    table_ptr: [*]const Entry,
    idx: u64,
    out_start: *u64,
) callconv(.c) u64 {
    const entry = table_ptr[@intCast(idx)];
    out_start.* = entry.events_start;
    return entry.events_count;
}

/// Replay every event in `events_buf[start..start+count]` against
/// `state` (the live event log) and `captures_ptr` (the JIT's
/// capture-slot array), pushing matching undo frames onto the JIT
/// stack so a later backtrack truncates correctly. Mirrors
/// `Vm.replayCachedEvents` for the JIT runtime.
///
/// Returns 0 on success, `events_mod.oom_sentinel` on allocator
/// failure or stack overflow. The JIT routes the OOM/overflow path
/// to its fail handler.
///
/// `sp_in_out` is read for the starting stack depth and written with
/// the final depth. The caller must reserve enough headroom on the
/// JIT stack — replay never grows the stack beyond `count` entries.
///
/// `max_stack` lives in `jit_abi`; we don't take it as a parameter so
/// the helper's ABI footprint fits in registers on x86_64 SysV (6 reg
/// args plus one stack arg `captures`).
pub fn helperMemoReplayEvents(
    state_ptr: *events_mod.State,
    events_buf_ptr: *EventsBuf,
    start: u64,
    count: u64,
    stack_ptr: [*]jit_abi.StackEntry,
    sp_in_out: *u64,
    captures_ptr: [*]u64,
) callconv(.c) u64 {
    if (count == 0) return 0;
    const cached = events_buf_ptr.list.items[@intCast(start)..][0..@intCast(count)];
    var sp = sp_in_out.*;
    for (cached) |ev| {
        if (sp >= jit_abi.max_stack) return events_mod.oom_sentinel;
        switch (ev) {
            .open => |m| {
                const slot: u16 = m.group_id << 1;
                const event_len = events_mod.appendSave(state_ptr, slot, m.pos) catch return events_mod.oom_sentinel;
                stack_ptr[@intCast(sp)] = .{
                    .tag = 2,
                    .val1 = slot,
                    .val2 = captures_ptr[slot],
                    .event_len = event_len,
                };
                sp += 1;
                captures_ptr[slot] = m.pos;
            },
            .close => |m| {
                const slot: u16 = (m.group_id << 1) | 1;
                const event_len = events_mod.appendSave(state_ptr, slot, m.pos) catch return events_mod.oom_sentinel;
                stack_ptr[@intCast(sp)] = .{
                    .tag = 2,
                    .val1 = slot,
                    .val2 = captures_ptr[slot],
                    .event_len = event_len,
                };
                sp += 1;
                captures_ptr[slot] = m.pos;
            },
            .partial_close => |m| {
                const event_len = events_mod.appendPartialClose(state_ptr, m.group_id, m.pos) catch return events_mod.oom_sentinel;
                stack_ptr[@intCast(sp)] = .{ .tag = 3, .val1 = 0, .val2 = 0, .event_len = event_len };
                sp += 1;
            },
            .error_open => |m| {
                const event_len = events_mod.appendErrorOpen(state_ptr, m.group_id, m.pos) catch return events_mod.oom_sentinel;
                stack_ptr[@intCast(sp)] = .{ .tag = 3, .val1 = 0, .val2 = 0, .event_len = event_len };
                sp += 1;
            },
            .error_close => |m| {
                const event_len = events_mod.appendErrorClose(state_ptr, m.group_id, m.pos) catch return events_mod.oom_sentinel;
                stack_ptr[@intCast(sp)] = .{ .tag = 3, .val1 = 0, .val2 = 0, .event_len = event_len };
                sp += 1;
            },
            .missing => |m| {
                const event_len = events_mod.appendMissing(state_ptr, m.group_id, m.pos) catch return events_mod.oom_sentinel;
                stack_ptr[@intCast(sp)] = .{ .tag = 3, .val1 = 0, .val2 = 0, .event_len = event_len };
                sp += 1;
            },
            .token => |t| {
                const event_len = events_mod.appendToken(state_ptr, t.start, t.end) catch return events_mod.oom_sentinel;
                stack_ptr[@intCast(sp)] = .{ .tag = 3, .val1 = 0, .val2 = 0, .event_len = event_len };
                sp += 1;
            },
            .field_marker => |fm| {
                const event_len = events_mod.appendField(state_ptr, fm.field_id, fm.pos) catch return events_mod.oom_sentinel;
                stack_ptr[@intCast(sp)] = .{ .tag = 3, .val1 = 0, .val2 = 0, .event_len = event_len };
                sp += 1;
            },
        }
    }
    sp_in_out.* = sp;
    return 0;
}

/// JIT-side memo-frame return handler. Called from the JIT's `ret`
/// lowering when it pops a tag=5 marker (memo frame). Reads the
/// side-table frame, marks `table[idx] = .success` (caching the event
/// range when capture_events is on), and returns the *native* code
/// address of the bytecode return PC so the JIT can branch to it
/// directly. Returns `events_mod.oom_sentinel` on allocator failure
/// (the JIT routes that into its fail handler).
pub fn helperMemoRetSuccess(
    memo_ctx: *const jit_abi.MemoCtx,
    side_idx: u64,
    end_pos: u64,
    state_ptr: ?*events_mod.State,
    jump_table: [*]const u64,
    code_base: u64,
) callconv(.c) u64 {
    const side: [*]Frame = @ptrFromInt(@as(usize, @intCast(memo_ctx.side_ptr)));
    const frame = side[@intCast(side_idx)];
    const idx: u64 = @as(u64, frame.rule_id) * memo_ctx.stride + frame.start_pos;
    const table: [*]Entry = @ptrFromInt(@as(usize, @intCast(memo_ctx.table_ptr)));

    var entry: Entry = .{ .state = .success, .next_pos_or_frame = @intCast(end_pos) };
    if (state_ptr) |state| {
        if (memo_ctx.events_buf_ptr != 0) {
            const buf: *EventsBuf = @ptrFromInt(@as(usize, @intCast(memo_ctx.events_buf_ptr)));
            const live = state.list.items;
            if (live.len > frame.events_len_at_entry) {
                const slice = live[frame.events_len_at_entry..];
                const start: u32 = @intCast(buf.list.items.len);
                buf.list.appendSlice(buf.allocator, slice) catch return events_mod.oom_sentinel;
                entry.events_start = start;
                entry.events_count = @intCast(slice.len);
            }
        }
    }
    table[@intCast(idx)] = entry;

    return code_base + jump_table[frame.return_pc];
}

/// JIT-side memo-frame backtrack handler. Called from the JIT's
/// backtrack loop when it crosses a tag=5 marker. Marks the table
/// entry `.fail` so the next memo lookup at the same `(rule_id, pos)`
/// short-circuits.
pub fn helperMemoRetFail(
    memo_ctx: *const jit_abi.MemoCtx,
    side_idx: u64,
) callconv(.c) void {
    const side: [*]Frame = @ptrFromInt(@as(usize, @intCast(memo_ctx.side_ptr)));
    const frame = side[@intCast(side_idx)];
    const idx: u64 = @as(u64, frame.rule_id) * memo_ctx.stride + frame.start_pos;
    const table: [*]Entry = @ptrFromInt(@as(usize, @intCast(memo_ctx.table_ptr)));
    table[@intCast(idx)] = .{ .state = .fail, .next_pos_or_frame = 0 };
}

const testing = std.testing;

test "memo.helperMemoLookup: empty entry returns miss" {
    var table: [4]Entry = .{Entry{ .state = .empty, .next_pos_or_frame = 0 }} ** 4;
    var end_pos: u64 = undefined;
    try testing.expectEqual(lookup_miss, helperMemoLookup(&table, 2, &end_pos));
}

test "memo.helperMemoLookup: success entry returns end pos" {
    var table: [4]Entry = .{Entry{ .state = .empty, .next_pos_or_frame = 0 }} ** 4;
    table[1] = .{ .state = .success, .next_pos_or_frame = 7 };
    var end_pos: u64 = undefined;
    try testing.expectEqual(lookup_success, helperMemoLookup(&table, 1, &end_pos));
    try testing.expectEqual(@as(u64, 7), end_pos);
}

test "memo.helperMemoLookup: fail entry returns fail" {
    var table: [4]Entry = .{Entry{ .state = .empty, .next_pos_or_frame = 0 }} ** 4;
    table[3] = .{ .state = .fail, .next_pos_or_frame = 0 };
    var end_pos: u64 = undefined;
    try testing.expectEqual(lookup_fail, helperMemoLookup(&table, 3, &end_pos));
}

