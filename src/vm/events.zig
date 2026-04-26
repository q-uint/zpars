/// Shared capture-event state used by both VM and JIT paths.
///
/// A `save` instruction carries a slot id; even slots open a group, odd
/// slots close one. Each successful `save` appends an `Event` describing
/// the transition and returns the pre-append length so the caller can
/// truncate the log on backtrack (keeping it in lockstep with the
/// capture-slot undo).
///
/// Consolidating the append/truncate semantics here keeps VM and JIT from
/// drifting: the VM calls these functions directly, the JIT emits calls
/// to the C-ABI wrappers at the bottom of this file.
const std = @import("std");
const CaptureTree = @import("CaptureTree.zig");
const jit_abi = @import("jit_abi.zig");

pub const State = struct {
    list: std.ArrayListUnmanaged(CaptureTree.Event),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) State {
        return .{ .list = .empty, .allocator = allocator };
    }

    pub fn deinit(self: *State) void {
        self.list.deinit(self.allocator);
    }

    pub fn clear(self: *State) void {
        self.list.clearRetainingCapacity();
    }

    pub fn items(self: *const State) []const CaptureTree.Event {
        return self.list.items;
    }
};

/// Append the open/close event for a successful `save` at `pos` and
/// return the length the log had *before* the append. Callers stash the
/// returned value on their undo stack so `truncate` can restore it on
/// backtrack.
pub fn appendSave(state: *State, slot: u16, pos: u32) std.mem.Allocator.Error!u32 {
    const pre_len: u32 = @intCast(state.list.items.len);
    const marker: CaptureTree.Event.Marker = .{
        .group_id = slot >> 1,
        .pos = pos,
    };
    const ev: CaptureTree.Event = if (slot & 1 == 0)
        .{ .open = marker }
    else
        .{ .close = marker };
    try state.list.append(state.allocator, ev);
    return pre_len;
}

/// Append a recovery-era event of one of the labeled variants. Same
/// shape as `appendSave`: returns the pre-append length so the caller
/// can stash it in an undo frame. `error_open` / `error_close` /
/// `missing` are emitted explicitly by recovery handlers in compiled
/// grammars; `partial_close` is synthesized by the throw unwinder when
/// it walks past unclosed `open`s on its way to the matching `lcatch`.
pub fn appendErrorOpen(state: *State, label: u16, pos: u32) std.mem.Allocator.Error!u32 {
    const pre_len: u32 = @intCast(state.list.items.len);
    try state.list.append(state.allocator, .{ .error_open = .{ .group_id = label, .pos = pos } });
    return pre_len;
}

pub fn appendErrorClose(state: *State, label: u16, pos: u32) std.mem.Allocator.Error!u32 {
    const pre_len: u32 = @intCast(state.list.items.len);
    try state.list.append(state.allocator, .{ .error_close = .{ .group_id = label, .pos = pos } });
    return pre_len;
}

pub fn appendMissing(state: *State, label: u16, pos: u32) std.mem.Allocator.Error!u32 {
    const pre_len: u32 = @intCast(state.list.items.len);
    try state.list.append(state.allocator, .{ .missing = .{ .group_id = label, .pos = pos } });
    return pre_len;
}

pub fn appendPartialClose(state: *State, group_id: u16, pos: u32) std.mem.Allocator.Error!u32 {
    const pre_len: u32 = @intCast(state.list.items.len);
    try state.list.append(state.allocator, .{ .partial_close = .{ .group_id = group_id, .pos = pos } });
    return pre_len;
}

/// Append an anonymous-token event for a literal that just matched
/// `input[start..end]`. The compiler emits this after every `char` /
/// `string` opcode under `token_events = .all` (or only for tagged
/// literals under `.tagged`).
pub fn appendToken(state: *State, start: u32, end: u32) std.mem.Allocator.Error!u32 {
    const pre_len: u32 = @intCast(state.list.items.len);
    try state.list.append(state.allocator, .{ .token = .{ .start = start, .end = end } });
    return pre_len;
}

/// Append a field-name "stamp" event. The compiler emits this just
/// before a rule call or literal that the grammar tagged with a field
/// name (e.g. `name:Identifier`); `buildFromEvents` attaches `field_id`
/// to the next open/token node it produces.
pub fn appendField(state: *State, field_id: u16, pos: u32) std.mem.Allocator.Error!u32 {
    const pre_len: u32 = @intCast(state.list.items.len);
    try state.list.append(state.allocator, .{ .field_marker = .{ .field_id = field_id, .pos = pos } });
    return pre_len;
}

/// Truncate the event log to `new_len`, dropping any events appended
/// after the matching `save`. Called from the backtrack unwind.
pub fn truncate(state: *State, new_len: u32) void {
    state.list.shrinkRetainingCapacity(new_len);
}

/// Sentinel the JIT helpers return on OOM. Matches `Jit.null_cap` so the
/// JIT save epilogue can reuse the same compare for match-failure and
/// append-failure paths.
pub const oom_sentinel: u64 = std.math.maxInt(u64);

/// C-ABI wrapper for `appendSave`. Returns `oom_sentinel` on allocator
/// failure; the JIT jumps to the fail handler on that value.
pub fn helperAppendSave(
    state_ptr: *State,
    slot: u64,
    pos: u64,
) callconv(.c) u64 {
    const pre_len = appendSave(state_ptr, @intCast(slot), @intCast(pos)) catch return oom_sentinel;
    return pre_len;
}

/// C-ABI wrapper for `appendToken`. Same OOM sentinel convention as
/// `helperAppendSave`. The JIT computes `start = pos - len` itself so
/// the helper just records the span.
pub fn helperAppendToken(
    state_ptr: *State,
    start: u64,
    end: u64,
) callconv(.c) u64 {
    const pre_len = appendToken(state_ptr, @intCast(start), @intCast(end)) catch return oom_sentinel;
    return pre_len;
}

/// C-ABI wrapper for `appendField`. Same OOM sentinel convention as
/// `helperAppendSave`.
pub fn helperAppendField(
    state_ptr: *State,
    field_id: u64,
    pos: u64,
) callconv(.c) u64 {
    const pre_len = appendField(state_ptr, @intCast(field_id), @intCast(pos)) catch return oom_sentinel;
    return pre_len;
}

/// C-ABI wrapper for `appendErrorOpen`. Same OOM sentinel convention.
pub fn helperAppendErrorOpen(
    state_ptr: *State,
    label: u64,
    pos: u64,
) callconv(.c) u64 {
    const pre_len = appendErrorOpen(state_ptr, @intCast(label), @intCast(pos)) catch return oom_sentinel;
    return pre_len;
}

/// C-ABI wrapper for `appendErrorClose`. Same OOM sentinel convention.
pub fn helperAppendErrorClose(
    state_ptr: *State,
    label: u64,
    pos: u64,
) callconv(.c) u64 {
    const pre_len = appendErrorClose(state_ptr, @intCast(label), @intCast(pos)) catch return oom_sentinel;
    return pre_len;
}

/// C-ABI wrapper for `appendMissing`. Same OOM sentinel convention.
pub fn helperAppendMissing(
    state_ptr: *State,
    label: u64,
    pos: u64,
) callconv(.c) u64 {
    const pre_len = appendMissing(state_ptr, @intCast(label), @intCast(pos)) catch return oom_sentinel;
    return pre_len;
}

/// C-ABI wrapper for `truncate`. Always succeeds.
pub fn helperTruncate(
    state_ptr: *State,
    new_len: u64,
) callconv(.c) void {
    truncate(state_ptr, @intCast(new_len));
}

/// Returns the current events-log length. Used by `lcatch` codegen to
/// snapshot the log length on its catch frame so a later `throw` can
/// synthesize partial-close events for the right window. Done via a
/// helper rather than an inline load so the JIT does not bake in
/// `std.ArrayListUnmanaged`'s field layout.
pub fn helperEventsLen(state_ptr: *State) callconv(.c) u64 {
    return @intCast(state_ptr.list.items.len);
}

/// Sentinel returned by `helperThrow` when no matching `lcatch` is on
/// the stack. The JIT branches to the fail handler on this value.
pub const throw_miss: u64 = std.math.maxInt(u64);

/// Wildcard label: an `lcatch` frame stamped with this value catches
/// throws of any label. Mirrors `Vm.wildcard_label`.
pub const wildcard_label: u64 = std.math.maxInt(u16);

/// JIT-side throw unwinder. Walks `stack[0..sp_in_out.*]` downward
/// looking for an `lcatch` frame (tag=4) with a matching label (or
/// the wildcard sentinel). On hit:
///   - synthesizes `partial_close` events for any unclosed `open`s
///     above the catch frame (mirrors `Vm.synthesizePartialCloses`);
///   - writes the new stack depth (just below the matched frame) into
///     `sp_in_out`;
///   - returns the bytecode PC of the catch's handler so the JIT can
///     index its jump table.
/// On miss: writes 0 to `sp_in_out` and returns `throw_miss`.
///
/// `state_ptr` may be null when capture_events is off; partial-close
/// synthesis is skipped in that case.
///
/// Note: unlike the JIT's regular backtrack handler, this helper does
/// NOT roll back capture slots or events stored on `.save`/`.event`
/// frames it walks past. That preserves throw semantics — the matching
/// catch decides what stays and what becomes a partial_close.
pub fn helperThrow(
    state_ptr: ?*State,
    stack_ptr: [*]jit_abi.StackEntry,
    sp_in_out: *u64,
    label: u64,
    throw_pos: u64,
) callconv(.c) u64 {
    const lcatch_tag: u64 = 4;
    var sp = sp_in_out.*;
    while (sp > 0) {
        sp -= 1;
        const entry = stack_ptr[@intCast(sp)];
        if (entry.tag != lcatch_tag) continue;
        const frame_label = entry.val1;
        const matches = frame_label == label or frame_label == wildcard_label;
        if (!matches) continue;

        const handler_pc = entry.val2;
        const event_len: u32 = @intCast(entry.event_len);
        if (state_ptr) |state| {
            synthesizePartialCloses(state, event_len, @intCast(throw_pos)) catch {
                sp_in_out.* = sp;
                return throw_miss;
            };
        }
        sp_in_out.* = sp;
        return handler_pc;
    }
    sp_in_out.* = 0;
    return throw_miss;
}

/// Mirror of `Vm.synthesizePartialCloses`: walks the live event window
/// `[catch_event_len..]` to find still-open captures (no matching
/// close/partial_close), then appends a `partial_close` for each at
/// `throw_pos`, innermost-first.
fn synthesizePartialCloses(
    state: *State,
    catch_event_len: u32,
    throw_pos: u32,
) std.mem.Allocator.Error!void {
    // Stack-allocated open-id stack. Bound matches the JIT's
    // `Jit.max_stack`; capture nesting can't exceed call-stack depth.
    var open_stack: [1024]u16 = undefined;
    var open_sp: usize = 0;

    const live = state.list.items[catch_event_len..];
    for (live) |ev| switch (ev) {
        .open => |m| {
            std.debug.assert(open_sp < open_stack.len);
            open_stack[open_sp] = m.group_id;
            open_sp += 1;
        },
        .close => |c| {
            std.debug.assert(open_sp > 0 and open_stack[open_sp - 1] == c.group_id);
            open_sp -= 1;
        },
        .partial_close => |c| {
            std.debug.assert(open_sp > 0 and open_stack[open_sp - 1] == c.group_id);
            open_sp -= 1;
        },
        // error_open/close, missing, token, field_marker have no
        // nesting effect on the open stack.
        .error_open, .error_close, .missing, .token, .field_marker => {},
    };

    while (open_sp > 0) {
        open_sp -= 1;
        _ = try appendPartialClose(state, open_stack[open_sp], throw_pos);
    }
}

const testing = std.testing;

test "appendSave: alternating slots yield open/close pair" {
    var state = State.init(testing.allocator);
    defer state.deinit();

    const pre0 = try appendSave(&state, 0, 3);
    const pre1 = try appendSave(&state, 1, 7);
    try testing.expectEqual(@as(u32, 0), pre0);
    try testing.expectEqual(@as(u32, 1), pre1);

    const evs = state.items();
    try testing.expectEqual(@as(usize, 2), evs.len);
    try testing.expectEqual(@as(u16, 0), evs[0].open.group_id);
    try testing.expectEqual(@as(u32, 3), evs[0].open.pos);
    try testing.expectEqual(@as(u16, 0), evs[1].close.group_id);
    try testing.expectEqual(@as(u32, 7), evs[1].close.pos);
}

test "truncate: restores the length returned from an earlier append" {
    var state = State.init(testing.allocator);
    defer state.deinit();

    _ = try appendSave(&state, 0, 1);
    const pre = try appendSave(&state, 2, 2);
    _ = try appendSave(&state, 3, 3);
    truncate(&state, pre);

    try testing.expectEqual(@as(usize, 1), state.items().len);
}

test "appendToken: records [start, end) span" {
    var state = State.init(testing.allocator);
    defer state.deinit();

    const pre = try appendToken(&state, 3, 7);
    try testing.expectEqual(@as(u32, 0), pre);
    const evs = state.items();
    try testing.expectEqual(@as(usize, 1), evs.len);
    try testing.expectEqual(@as(u32, 3), evs[0].token.start);
    try testing.expectEqual(@as(u32, 7), evs[0].token.end);
}

test "appendField: records field id and pos" {
    var state = State.init(testing.allocator);
    defer state.deinit();

    const pre = try appendField(&state, 5, 10);
    try testing.expectEqual(@as(u32, 0), pre);
    const evs = state.items();
    try testing.expectEqual(@as(usize, 1), evs.len);
    try testing.expectEqual(@as(u16, 5), evs[0].field_marker.field_id);
    try testing.expectEqual(@as(u32, 10), evs[0].field_marker.pos);
}

test "recovery helpers: each variant appends correctly and returns pre-len" {
    var state = State.init(testing.allocator);
    defer state.deinit();

    const pre_eo = try appendErrorOpen(&state, 7, 3);
    const pre_ec = try appendErrorClose(&state, 7, 9);
    const pre_pc = try appendPartialClose(&state, 1, 9);
    const pre_m = try appendMissing(&state, 4, 9);

    try testing.expectEqual(@as(u32, 0), pre_eo);
    try testing.expectEqual(@as(u32, 1), pre_ec);
    try testing.expectEqual(@as(u32, 2), pre_pc);
    try testing.expectEqual(@as(u32, 3), pre_m);

    const evs = state.items();
    try testing.expectEqual(@as(usize, 4), evs.len);
    try testing.expectEqual(@as(u16, 7), evs[0].error_open.group_id);
    try testing.expectEqual(@as(u32, 3), evs[0].error_open.pos);
    try testing.expectEqual(@as(u16, 7), evs[1].error_close.group_id);
    try testing.expectEqual(@as(u16, 1), evs[2].partial_close.group_id);
    try testing.expectEqual(@as(u16, 4), evs[3].missing.group_id);
    try testing.expectEqual(@as(u32, 9), evs[3].missing.pos);
}
