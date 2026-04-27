/// Shared packrat-memoization state used by the JIT path.
///
/// Mirrors the VM's `memo_table` / memo-frame machinery (`Vm.MemoEntry`,
/// the `.memo` `Entry` variant, etc.) in a non-generic, extern-friendly
/// shape so the JIT-emitted code can read and write it via the C ABI.
const std = @import("std");
const events_mod = @import("events.zig");
const CaptureTree = @import("CaptureTree.zig");
const jit_abi = @import("jit_abi.zig");

/// Distinct integer types so a swapped `(rule_id, pos)` arg becomes a
/// compile error inside the helpers rather than a silent miscompile.
/// Non-exhaustive (`_`) so any underlying integer is a valid value.
/// All have the same in-memory size as their tag, so `Frame` keeps its
/// 32-byte layout and the C-ABI surface is unaffected.
pub const RuleId = enum(u32) { _ };
pub const InputPos = enum(u32) { _ };
pub const SideIdx = enum(u32) { _ };
/// Flat index into the memo table: `rule_id * stride + pos`. u64 because
/// large grammars * long inputs can exceed u32.
pub const MemoIdx = enum(u64) { _ };

inline fn ridFromU64(x: u64) RuleId {
    return @enumFromInt(@as(u32, @intCast(x)));
}

inline fn posFromU64(x: u64) InputPos {
    return @enumFromInt(@as(u32, @intCast(x)));
}

inline fn sideFromU64(x: u64) SideIdx {
    return @enumFromInt(@as(u32, @intCast(x)));
}

/// Single source of truth for the table-index arithmetic. The arg
/// types make `(pos, rule_id)` a compile error.
inline fn computeMemoIdx(rule_id: RuleId, pos: InputPos, stride: u64) MemoIdx {
    return @enumFromInt(@as(u64, @intFromEnum(rule_id)) * stride + @intFromEnum(pos));
}

inline fn ctxTable(ctx: *const jit_abi.MemoCtx) [*]Entry {
    return @ptrFromInt(@as(usize, @intCast(ctx.table_ptr)));
}

inline fn ctxSide(ctx: *const jit_abi.MemoCtx) *Side {
    return @ptrFromInt(@as(usize, @intCast(ctx.side_ptr)));
}

inline fn ctxHeads(ctx: *const jit_abi.MemoCtx) *Heads {
    return @ptrFromInt(@as(usize, @intCast(ctx.heads_ptr)));
}

inline fn ctxStack(ctx: *const jit_abi.MemoCtx) [*]jit_abi.StackEntry {
    return @ptrFromInt(@as(usize, @intCast(ctx.stack_ptr)));
}

inline fn ctxJumpTable(ctx: *const jit_abi.MemoCtx) [*]const u64 {
    return @ptrFromInt(@as(usize, @intCast(ctx.jump_table_ptr)));
}

inline fn ctxEventsState(ctx: *const jit_abi.MemoCtx) ?*events_mod.State {
    if (ctx.events_state_ptr == 0) return null;
    return @ptrFromInt(@as(usize, @intCast(ctx.events_state_ptr)));
}

inline fn ctxEventsBuf(ctx: *const jit_abi.MemoCtx) ?*EventsBuf {
    if (ctx.events_buf_ptr == 0) return null;
    return @ptrFromInt(@as(usize, @intCast(ctx.events_buf_ptr)));
}

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

/// Side-table memo frame data. Lives in `Side.frames` (a growable
/// list, *not* indexed by stack depth) -- the JIT stores a fresh
/// side index in each memo `StackEntry.val1`, and the index never
/// collides with prior pushes within an `execute()`. (Naively
/// reusing `bsp` as the index breaks under Warth's grow-and-recall:
/// the grow re-push lands at the same `bsp` that an earlier memo
/// frame's data lived at, and clobbers it.)
///
/// Sized to 32 bytes so the JIT can index `Side.frames` with a
/// `shift-by-5` (x32) rather than an integer multiply. The trailing
/// fields are only read on the LR (Warth) paths; non-LR runs leave
/// them at their defaults.
pub const Frame = extern struct {
    rule_id: RuleId,
    start_pos: InputPos,
    return_pc: u32, // bytecode PC to resume at after the rule completes
    rule_entry_pc: u32, // bytecode PC of the rule body's first instruction
    events_len_at_entry: u32,
    /// 0 for first-evaluation frames, 1 for recall re-eval frames
    /// pushed from inside a grow iteration.
    is_recall: u32 = 0,
    /// `grow_sentinel` (= maxInt(u32)) means "no grow seed yet";
    /// otherwise the current grow-iteration seed.
    best_end: u32 = grow_sentinel,
    /// `no_head` (= maxInt(u32)) means "not attached to any head";
    /// otherwise an index into the heads pool.
    head_idx: u32 = no_head,
};

comptime {
    if (@sizeOf(Frame) != 32) @compileError("memo.Frame must be 32 bytes");
}

pub const grow_sentinel: u32 = std.math.maxInt(u32);
pub const no_head: u32 = std.math.maxInt(u32);

/// Container for the side-table frames. Grown on every memo-frame
/// push; never shrunk during a single `execute()`. The JIT stores a
/// fresh `side_idx` (= the post-push length minus one) in each memo
/// `StackEntry.val1`. Helpers index into `frames.items` via that
/// `side_idx`. Sharing the same allocator as `Heads` keeps lifetime
/// bookkeeping simple.
pub const Side = struct {
    frames: std.ArrayListUnmanaged(Frame) = .empty,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Side {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Side) void {
        self.frames.deinit(self.allocator);
    }

    pub fn clear(self: *Side) void {
        self.frames.clearRetainingCapacity();
    }

    /// Append a frame and return its `SideIdx`. Indices are monotonic
    /// within an `execute()` and never collide with prior pushes.
    /// Helpers propagate allocator failure as `events_mod.oom_sentinel`.
    pub fn push(self: *Side, frame: Frame) !SideIdx {
        const idx: SideIdx = @enumFromInt(@as(u32, @intCast(self.frames.items.len)));
        try self.frames.append(self.allocator, frame);
        return idx;
    }

    pub fn at(self: *Side, idx: SideIdx) *Frame {
        return &self.frames.items[@intFromEnum(idx)];
    }
};

/// One Warth head: rule whose left-recursion is currently being
/// "grown" at some position, plus the involved/eval bitsets that
/// govern which other rules participate in the cycle.
pub const Head = struct {
    rule_id: RuleId,
    involved: std.DynamicBitSetUnmanaged,
    eval: std.DynamicBitSetUnmanaged,
};

/// Container for the heads array + heads pool. The JIT allocates one
/// of these inside `Jit.Self` and exposes a pointer to the helpers
/// via `MemoCtx`.
pub const Heads = struct {
    /// Per-position active head index. `no_head` means "no head
    /// active here". Sized to `stride` (input.len + 1).
    arr: []u32,
    /// Pool of `Head`s; `arr` entries index into this slice.
    pool: std.ArrayListUnmanaged(Head) = .empty,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, stride: usize) !Heads {
        const arr = try allocator.alloc(u32, stride);
        @memset(arr, no_head);
        return .{ .arr = arr, .allocator = allocator };
    }

    pub fn deinit(self: *Heads) void {
        for (self.pool.items) |*h| {
            h.involved.deinit(self.allocator);
            h.eval.deinit(self.allocator);
        }
        self.pool.deinit(self.allocator);
        self.allocator.free(self.arr);
    }

    /// Reset between executes: clear the per-position arr (so old
    /// heads are forgotten) and free the pool's bitsets.
    pub fn clear(self: *Heads) void {
        @memset(self.arr, no_head);
        for (self.pool.items) |*h| {
            h.involved.deinit(self.allocator);
            h.eval.deinit(self.allocator);
        }
        self.pool.clearRetainingCapacity();
    }
};

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

/// JIT-side memo_call dispatcher. Encapsulates the full
/// "lookup + RECALL + setupLr" decision tree that the VM does inline
/// in its `memo_call` handler. The caller (JIT-emitted code) just
/// branches on the returned action code.
///
/// Pre-RECALL: when an LR head is active at `pos`, the helper either
/// short-circuits with `cut` (rule isn't in the head's involved set)
/// or returns `recall` (the helper has already pushed the new recall
/// frame onto `Side` and reports its index via `side_idx_out`). The
/// recall path also flips the rule's `eval` bit off so the same body
/// isn't re-recurred this iteration.
///
/// Lookup: on `.lr` (in-progress evaluation), the helper runs setupLr
/// internally, which walks the JIT backtrack stack to attach memo
/// frames to a shared head. It then returns either `lr_seed` (with
/// the head's current best_end written to `end_pos_out`) or `fail`
/// (no seed yet).
///
/// For `miss` and `recall`, the helper appends the new frame to
/// `Side` and writes the new `side_idx` to `side_idx_out`; the JIT
/// stores that into the new memo `StackEntry.val1`.
///
/// `pc_pair` packs `return_pc` (low 32) and `rule_entry_pc` (high 32)
/// into one register-sized arg. `sp_top` is the JIT's `bsp` at the
/// memo_call site (used by `setupLr` and as the in-progress frame
/// pointer for the `.lr` table marker).
pub fn helperMemoCallBegin(
    memo_ctx: *const jit_abi.MemoCtx,
    sp_top: u64,
    rule_id: u64,
    pos: u64,
    pc_pair: u64,
    end_pos_out: *u64,
    side_idx_out: *u64,
) callconv(.c) u64 {
    std.debug.assert(rule_id < memo_ctx.memo_rule_count);
    std.debug.assert(pos < memo_ctx.stride);
    std.debug.assert(sp_top < jit_abi.max_stack);

    const heads = ctxHeads(memo_ctx);
    const table = ctxTable(memo_ctx);
    const side = ctxSide(memo_ctx);
    const rid = ridFromU64(rule_id);
    const ipos = posFromU64(pos);
    const idx = computeMemoIdx(rid, ipos, memo_ctx.stride);
    const idx_int: usize = @intCast(@intFromEnum(idx));
    const return_pc: u32 = @truncate(pc_pair);
    const rule_entry_pc: u32 = @truncate(pc_pair >> 32);
    const events_len = currentEventsLen(memo_ctx);

    // RECALL fast-path: a head is active at `pos`.
    const active_head = heads.arr[@intFromEnum(ipos)];
    if (active_head != no_head) {
        const h = &heads.pool.items[active_head];
        const rid_int: usize = @intFromEnum(rid);
        // CUT: empty entry, not the head's seed rule, not in
        // `involved`. The grammar rule cannot participate in this
        // grow cycle, so backtrack rather than re-evaluate.
        if (table[idx_int].state == .empty and
            rid != h.rule_id and
            !h.involved.isSet(rid_int))
        {
            return call_cut;
        }
        // RECALL: rule is still in the eval set, so this iteration
        // must re-evaluate it once (with the current seed visible).
        if (h.eval.isSet(rid_int)) {
            h.eval.unset(rid_int);
            const new_idx = side.push(.{
                .rule_id = rid,
                .start_pos = ipos,
                .return_pc = return_pc,
                .rule_entry_pc = rule_entry_pc,
                .events_len_at_entry = events_len,
                .is_recall = 1,
                .head_idx = active_head,
            }) catch return events_mod.oom_sentinel;
            side_idx_out.* = @intFromEnum(new_idx);
            return call_recall;
        }
        // Otherwise fall through to normal table lookup.
    }

    const entry = table[idx_int];
    switch (entry.state) {
        .empty => {
            // Mark the entry as in-progress so a re-entrant call to
            // (rule, pos) can detect left recursion via the `.lr`
            // arm below. `sp_top` is the JIT's bsp here -- after the
            // caller pushes the marker, `stack[sp_top].val1` will
            // resolve to the side frame we're about to allocate.
            table[idx_int] = .{ .state = .lr, .next_pos_or_frame = @intCast(sp_top) };
            const new_idx = side.push(.{
                .rule_id = rid,
                .start_pos = ipos,
                .return_pc = return_pc,
                .rule_entry_pc = rule_entry_pc,
                .events_len_at_entry = events_len,
            }) catch return events_mod.oom_sentinel;
            side_idx_out.* = @intFromEnum(new_idx);
            return call_miss;
        },
        .fail => return call_fail,
        .success => {
            end_pos_out.* = entry.next_pos_or_frame;
            return call_success;
        },
        .lr => {
            // SETUP-LR: attach all memo frames between the in-progress
            // frame and `sp_top` to a shared head. Returns the head's
            // current best_end as the seed, or `call_fail` if there's
            // no seed yet (pre-grow first eval still in progress).
            // The in-progress frame is found via the table's stored
            // `next_pos_or_frame`, which the .empty arm above set to
            // the bsp at first-call time. We need that frame's depth
            // in the JIT stack -- look it up via val1.
            const stack = ctxStack(memo_ctx);
            const owner_sp_top: u32 = entry.next_pos_or_frame;
            // Fetch the side index from the stack entry at depth
            // `owner_sp_top` (where the in-progress memo frame
            // marker lives). It's a tag=5 entry with val1 = side_idx.
            const owner_side_idx = sideFromU64(stack[owner_sp_top].val1);
            setupLr(heads, side, stack, sp_top, owner_side_idx, rid, @intCast(memo_ctx.memo_rule_count)) catch {
                return events_mod.oom_sentinel;
            };
            const fr = side.at(owner_side_idx);
            if (fr.best_end != grow_sentinel) {
                end_pos_out.* = fr.best_end;
                return call_lr_seed;
            }
            return call_fail;
        },
    }
}

/// Snapshot of the live event log length, or 0 when capture_events
/// is off. Read once per memo-frame push so a future grow / recall
/// completion can cache only the events the body produced.
fn currentEventsLen(memo_ctx: *const jit_abi.MemoCtx) u32 {
    const state = ctxEventsState(memo_ctx) orelse return 0;
    return @intCast(state.list.items.len);
}

/// Action codes returned by `helperMemoCallBegin`. The JIT codegen
/// dispatches on these.
pub const call_miss: u64 = 0;
pub const call_fail: u64 = 1;
pub const call_success: u64 = 2;
pub const call_lr_seed: u64 = 3;
pub const call_recall: u64 = 4;
pub const call_cut: u64 = 5;

/// SETUP-LR: walk the JIT backtrack stack from `sp_top - 1` down,
/// marking every memo frame's side data with `target_head_idx` and
/// adding their rule_ids to the head's involved set. Allocates a new
/// head if the in-progress frame (at `owner_side_idx`) doesn't have
/// one yet. Mirrors `Vm.setupLrVm`. The walk stops once we reach the
/// in-progress frame's stack entry (or a frame already attached to
/// the same head).
fn setupLr(
    heads: *Heads,
    side: *Side,
    stack: [*]jit_abi.StackEntry,
    sp_top: u64,
    owner_side_idx: SideIdx,
    recur_rule_id: RuleId,
    num_rules: usize,
) std.mem.Allocator.Error!void {
    var target_head_idx = side.at(owner_side_idx).head_idx;
    if (target_head_idx == no_head) {
        var involved = try std.DynamicBitSetUnmanaged.initEmpty(heads.allocator, num_rules);
        const eval = try std.DynamicBitSetUnmanaged.initEmpty(heads.allocator, num_rules);
        const seed_rule_id = side.at(owner_side_idx).rule_id;
        involved.set(@intFromEnum(seed_rule_id));
        try heads.pool.append(heads.allocator, .{
            .rule_id = seed_rule_id,
            .involved = involved,
            .eval = eval,
        });
        target_head_idx = @intCast(heads.pool.items.len - 1);
        side.at(owner_side_idx).head_idx = target_head_idx;
    }
    heads.pool.items[target_head_idx].involved.set(@intFromEnum(recur_rule_id));

    // Walk down looking for tag=5 stack entries; for each, mark its
    // side frame with this head and add its rule_id to involved. Stop
    // when we hit the in-progress frame or a frame that's already
    // attached to this head.
    var i: u64 = sp_top;
    while (i > 0) {
        i -= 1;
        if (stack[@intCast(i)].tag != 5) continue;
        const fr_idx = sideFromU64(stack[@intCast(i)].val1);
        const fr = side.at(fr_idx);
        if (fr.head_idx == target_head_idx) break;
        fr.head_idx = target_head_idx;
        heads.pool.items[target_head_idx].involved.set(@intFromEnum(fr.rule_id));
        if (@intFromEnum(fr_idx) == @intFromEnum(owner_side_idx)) break;
    }
}

/// Reset eval = involved at the start of each grow iteration.
fn resetEvalSet(heads: *Heads, head_idx: u32) void {
    const h = &heads.pool.items[head_idx];
    h.eval.setRangeValue(.{ .start = 0, .end = h.eval.bit_length }, false);
    var it = h.involved.iterator(.{});
    while (it.next()) |bit| h.eval.set(bit);
}

/// Read a `.success` entry's cached event range. Returns the slice
/// length, or zero when capture_events is off / nothing was cached.
/// `out_start` receives the index into `events_buf` of the first
/// cached event. The JIT calls this after `helperMemoCallBegin`
/// returns `call_success`, then forwards the slice to
/// `helperMemoReplayEvents`.
pub fn helperMemoCachedSlice(
    table_ptr: [*]const Entry,
    idx: u64,
    out_start: *u64,
) callconv(.c) u64 {
    const entry = table_ptr[@intCast(idx)];
    // Contract: only called immediately after `helperMemoCallBegin`
    // returned `call_success`, so the entry is `.success`.
    std.debug.assert(entry.state == .success);
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
    std.debug.assert(start <= events_buf_ptr.list.items.len);
    std.debug.assert(count <= events_buf_ptr.list.items.len - start);
    std.debug.assert(sp_in_out.* <= jit_abi.max_stack);
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

/// Output struct for `helperMemoRet` / `helperMemoBacktrack`. Lets the
/// helpers return a (native target, pos override) pair without having
/// to pack into `u64` or use multiple out-pointers.
pub const RetResult = extern struct {
    /// Native code address to branch to when `action != ret_continue`.
    native_target: u64 = 0,
    /// `pos` value to set when redirecting (ignored for `ret_done`).
    new_pos: u64 = 0,
};

/// `helperMemoRet` action codes.
pub const ret_done: u64 = 0;
/// Helper has re-pushed a memo frame for a grow iteration; bsp has
/// been incremented in place. Caller branches to `out.native_target`
/// (the rule-entry native PC) and sets `pos = out.new_pos` (the
/// frame's start_pos seed).
pub const ret_regrow: u64 = 1;
/// Allocator failure. Caller routes to its fail handler.
pub const ret_oom: u64 = 2;

/// `helperMemoBacktrack` action codes.
pub const bt_continue: u64 = 0;
/// Caller branches to `out.native_target` (the original return_pc's
/// native address) with `pos = out.new_pos`. Used both for a recall
/// frame redirecting to a previously-cached success and for a grow
/// iteration that just failed to grow further (seed remains).
pub const bt_redirect: u64 = 1;

/// JIT-side memo-frame return handler. Called from the JIT's `ret`
/// lowering when it pops a tag=5 marker. Mirrors the VM's `.memo` arm
/// of `ret`: distinguishes recall completion / first-eval completion
/// (with or without an attached head) / grow-iter completion, and
/// either finalizes the memo entry or re-pushes a frame to grow.
///
/// `bsp_ptr` lets the helper push the new memo frame for the grow
/// case. Caller spills `bsp` to a known scratch slot before the call
/// and reloads it after.
pub fn helperMemoRet(
    memo_ctx: *const jit_abi.MemoCtx,
    side_idx: u64,
    end_pos: u64,
    bsp_ptr: *u64,
    out: *RetResult,
) callconv(.c) u64 {
    const side = ctxSide(memo_ctx);
    std.debug.assert(side_idx < side.frames.items.len);
    std.debug.assert(bsp_ptr.* < jit_abi.max_stack);

    const sidx = sideFromU64(side_idx);
    const frame = side.at(sidx).*;
    const idx = computeMemoIdx(frame.rule_id, frame.start_pos, memo_ctx.stride);
    const idx_int: usize = @intCast(@intFromEnum(idx));
    const table = ctxTable(memo_ctx);
    const jt = ctxJumpTable(memo_ctx);
    const code_base = memo_ctx.code_base;

    const cur_end: u32 = @intCast(end_pos);
    const start_pos_u32: u32 = @intFromEnum(frame.start_pos);

    if (frame.is_recall != 0) {
        // RECALL re-eval completed. Only update the table if the
        // answer strictly grew, so a late recall can't clobber a
        // better seed written by an earlier iteration.
        const prev = table[idx_int];
        const prev_end: u32 = if (prev.state == .success) prev.next_pos_or_frame else start_pos_u32;
        const report_end = if (cur_end > prev_end) cur_end else prev_end;
        if (cur_end > prev_end) {
            writeMemoSuccess(table, idx, cur_end, frame, memo_ctx) catch return ret_oom;
        }
        out.native_target = code_base + jt[frame.return_pc];
        out.new_pos = report_end;
        return ret_done;
    }

    if (frame.best_end == grow_sentinel) {
        // First evaluation of this rule@pos completed.
        if (frame.head_idx != no_head) {
            const heads = ctxHeads(memo_ctx);
            if (heads.pool.items[frame.head_idx].rule_id == frame.rule_id) {
                // Enter GROW: write success, register head at pos,
                // reset eval set, re-push frame with best_end seeded.
                writeMemoSuccess(table, idx, cur_end, frame, memo_ctx) catch return ret_oom;
                heads.arr[start_pos_u32] = frame.head_idx;
                resetEvalSet(heads, frame.head_idx);
                pushGrowFrame(memo_ctx, bsp_ptr, frame, cur_end) catch return ret_oom;
                out.native_target = code_base + jt[frame.rule_entry_pc];
                out.new_pos = start_pos_u32;
                return ret_regrow;
            }
            // Participant in someone else's cycle: hand answer up.
            writeMemoSuccess(table, idx, cur_end, frame, memo_ctx) catch return ret_oom;
            out.native_target = code_base + jt[frame.return_pc];
            out.new_pos = end_pos;
            return ret_done;
        }
        // Plain (non-LR) success.
        writeMemoSuccess(table, idx, cur_end, frame, memo_ctx) catch return ret_oom;
        out.native_target = code_base + jt[frame.return_pc];
        out.new_pos = end_pos;
        return ret_done;
    }

    // Grow iteration just completed. During the iteration, recall
    // re-evals may have written a better answer into the table even
    // if this iteration produced a shorter match (via alternation
    // fallback). The memo entry holds the true current best.
    const memo_end: u32 = if (table[idx_int].state == .success)
        table[idx_int].next_pos_or_frame
    else
        frame.best_end;
    const new_best = if (cur_end > memo_end) cur_end else memo_end;
    if (new_best > frame.best_end) {
        writeMemoSuccess(table, idx, new_best, frame, memo_ctx) catch return ret_oom;
        const heads = ctxHeads(memo_ctx);
        resetEvalSet(heads, frame.head_idx);
        pushGrowFrame(memo_ctx, bsp_ptr, frame, new_best) catch return ret_oom;
        out.native_target = code_base + jt[frame.rule_entry_pc];
        out.new_pos = start_pos_u32;
        return ret_regrow;
    }
    // Done growing. Drop head from this position; resume at best seed.
    const heads = ctxHeads(memo_ctx);
    heads.arr[start_pos_u32] = no_head;
    out.native_target = code_base + jt[frame.return_pc];
    out.new_pos = frame.best_end;
    return ret_done;
}

/// JIT-side memo-frame backtrack handler. Called from the JIT's
/// backtrack loop when it crosses a tag=5 marker. Mirrors the VM's
/// `.memo` arm of `backtrack`.
pub fn helperMemoBacktrack(
    memo_ctx: *const jit_abi.MemoCtx,
    side_idx: u64,
    out: *RetResult,
) callconv(.c) u64 {
    const side = ctxSide(memo_ctx);
    std.debug.assert(side_idx < side.frames.items.len);

    const frame = side.at(sideFromU64(side_idx)).*;
    const idx = computeMemoIdx(frame.rule_id, frame.start_pos, memo_ctx.stride);
    const idx_int: usize = @intCast(@intFromEnum(idx));
    const table = ctxTable(memo_ctx);
    const jt = ctxJumpTable(memo_ctx);

    if (frame.is_recall != 0) {
        // RECALL re-eval failed. If a prior iteration already cached
        // a success seed, redirect to that rather than overwrite.
        const prev = table[idx_int];
        if (prev.state == .success) {
            out.native_target = memo_ctx.code_base + jt[frame.return_pc];
            out.new_pos = prev.next_pos_or_frame;
            return bt_redirect;
        }
        table[idx_int] = .{ .state = .fail, .next_pos_or_frame = 0 };
        return bt_continue;
    }

    if (frame.best_end != grow_sentinel) {
        // Grow iteration failed. Stop growing, resume at best seed.
        const heads = ctxHeads(memo_ctx);
        heads.arr[@intFromEnum(frame.start_pos)] = no_head;
        out.native_target = memo_ctx.code_base + jt[frame.return_pc];
        out.new_pos = frame.best_end;
        return bt_redirect;
    }

    // First-eval failure: cache it.
    table[idx_int] = .{ .state = .fail, .next_pos_or_frame = 0 };
    return bt_continue;
}

/// Write `table[idx] = .success`, optionally caching the event range
/// `events[entry_events_len..now_len]` into `events_buf` so a future
/// `call_success` can replay them. Used by `helperMemoRet` for both
/// the plain and grow paths.
fn writeMemoSuccess(
    table: [*]Entry,
    idx: MemoIdx,
    end_pos: u32,
    frame: Frame,
    memo_ctx: *const jit_abi.MemoCtx,
) std.mem.Allocator.Error!void {
    var entry: Entry = .{ .state = .success, .next_pos_or_frame = end_pos };
    if (ctxEventsState(memo_ctx)) |state| {
        if (ctxEventsBuf(memo_ctx)) |buf| {
            const live = state.list.items;
            if (live.len > frame.events_len_at_entry) {
                const slice = live[frame.events_len_at_entry..];
                const start: u32 = @intCast(buf.list.items.len);
                try buf.list.appendSlice(buf.allocator, slice);
                entry.events_start = start;
                entry.events_count = @intCast(slice.len);
            }
        }
    }
    table[@intCast(@intFromEnum(idx))] = entry;
}

/// Push a memo frame for the grow case: append a new entry to
/// `Side` reusing the popped frame's `rule_id`/`start_pos`/return/
/// entry/events_len fields with `best_end` seeded, then push the
/// matching tag=5 marker onto the JIT stack and increment `*bsp_ptr`.
/// Returns `events_mod.oom_sentinel` cast to non-zero on OOM
/// (caller routes to its fail handler).
fn pushGrowFrame(
    memo_ctx: *const jit_abi.MemoCtx,
    bsp_ptr: *u64,
    base: Frame,
    new_best: u32,
) std.mem.Allocator.Error!void {
    const side = ctxSide(memo_ctx);
    const stack = ctxStack(memo_ctx);
    const new_idx = try side.push(.{
        .rule_id = base.rule_id,
        .start_pos = base.start_pos,
        .return_pc = base.return_pc,
        .rule_entry_pc = base.rule_entry_pc,
        .events_len_at_entry = base.events_len_at_entry,
        .is_recall = 0,
        .best_end = new_best,
        .head_idx = base.head_idx,
    });
    const sp: u64 = bsp_ptr.*;
    std.debug.assert(sp < jit_abi.max_stack);
    stack[@intCast(sp)] = .{ .tag = 5, .val1 = @intFromEnum(new_idx), .val2 = 0, .event_len = 0 };
    bsp_ptr.* = sp + 1;
}

const testing = std.testing;

fn buildTestCtx(table: []Entry, side: *Side, heads: *Heads, stride: usize) jit_abi.MemoCtx {
    return .{
        .table_ptr = @intFromPtr(table.ptr),
        .stride = stride,
        .side_ptr = @intFromPtr(side),
        .events_buf_ptr = 0,
        .events_state_ptr = 0,
        .stack_ptr = 0,
        .jump_table_ptr = 0,
        .code_base = 0,
        .heads_ptr = @intFromPtr(heads),
        .memo_rule_count = table.len / stride,
        .helper_call_begin = 0,
        .helper_cached_slice = 0,
        .helper_replay_events = 0,
        .helper_ret = 0,
        .helper_backtrack = 0,
    };
}

test "memo.helperMemoCallBegin: empty entry returns miss" {
    var table: [4]Entry = .{Entry{ .state = .empty, .next_pos_or_frame = 0 }} ** 4;
    var side = Side.init(testing.allocator);
    defer side.deinit();
    var heads = try Heads.init(testing.allocator, 4);
    defer heads.deinit();
    const ctx = buildTestCtx(&table, &side, &heads, 2);
    var end_pos: u64 = undefined;
    var side_idx: u64 = undefined;
    try testing.expectEqual(call_miss, helperMemoCallBegin(&ctx, 0, 1, 0, 0, &end_pos, &side_idx));
}

test "memo.helperMemoCallBegin: success entry returns end pos" {
    var table: [4]Entry = .{Entry{ .state = .empty, .next_pos_or_frame = 0 }} ** 4;
    table[3] = .{ .state = .success, .next_pos_or_frame = 7 };
    var side = Side.init(testing.allocator);
    defer side.deinit();
    var heads = try Heads.init(testing.allocator, 4);
    defer heads.deinit();
    const ctx = buildTestCtx(&table, &side, &heads, 2);
    var end_pos: u64 = undefined;
    var side_idx: u64 = undefined;
    // idx = 1 * stride(2) + 1 = 3 -> success
    try testing.expectEqual(call_success, helperMemoCallBegin(&ctx, 0, 1, 1, 0, &end_pos, &side_idx));
    try testing.expectEqual(@as(u64, 7), end_pos);
}

test "memo.helperMemoCallBegin: fail entry returns fail" {
    var table: [4]Entry = .{Entry{ .state = .empty, .next_pos_or_frame = 0 }} ** 4;
    table[2] = .{ .state = .fail, .next_pos_or_frame = 0 };
    var side = Side.init(testing.allocator);
    defer side.deinit();
    var heads = try Heads.init(testing.allocator, 4);
    defer heads.deinit();
    const ctx = buildTestCtx(&table, &side, &heads, 2);
    var end_pos: u64 = undefined;
    var side_idx: u64 = undefined;
    // idx = 1 * stride(2) + 0 = 2 -> fail
    try testing.expectEqual(call_fail, helperMemoCallBegin(&ctx, 0, 1, 0, 0, &end_pos, &side_idx));
}
