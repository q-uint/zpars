/// Shared runtime state for the JIT (`Jit.JitWith`) and AOT engine
/// (`AotRuntime.EngineWith`). Both backends emit the same native code
/// from the same C-ABI helpers in `events.zig` / `memo.zig`, so they
/// need the same per-execute scratch space: an optional event log, an
/// optional memo table, and a `MemoCtx` populated each `execute()`.
///
/// Embed `RuntimeState(config)` as a `state` field; call `empty()` /
/// `initEvents()` / `initPackratEager()` / `initPackratLazy()` from the
/// matching constructor, `deinit()` from the host's deinit,
/// `beginExecute(input_len)` at the top of each execute, and
/// `populateMemoCtx(...)` once `stack_ptr` / `jump_table_ptr` /
/// `code_base` are known.
///
/// The VM (`Vm.zig`) does *not* use this -- its memo state is a set of
/// interpreter-friendly tagged unions, not the C-ABI extern structs the
/// JIT/AOT path needs.
const std = @import("std");
const jit_abi = @import("jit_abi.zig");
const events_mod = @import("events.zig");
const memo_mod = @import("memo.zig");
const CaptureTree = @import("CaptureTree.zig");
const LookaheadAnalysis = @import("LookaheadAnalysis.zig");

/// Re-export of the analysis sentinel meaning "no static upper bound."
/// Used by `applyEdit` to fall back to always-invalidate for rules whose
/// lookahead bound couldn't be statically established (or whose
/// `examined_max` slice is empty).
pub const lookahead_unbounded: u32 = LookaheadAnalysis.unbounded_value;

/// Decide whether a prefix memo entry at `(rule, start_pos)` can be
/// kept across an edit beginning at `edit_start`. Safe to keep iff the
/// rule's body provably could not have read any byte at or after
/// `edit_start`. `unbounded` rules always invalidate.
inline fn canKeepPrefix(start_pos: u32, rule_examined_max: u32, edit_start: u32) bool {
    if (rule_examined_max == lookahead_unbounded) return false;
    const examined_end, const overflow = @addWithOverflow(start_pos, rule_examined_max);
    if (overflow != 0) return false;
    return examined_end <= edit_start;
}

/// Contiguous byte-range edit on the parser input, in the LSP/tree-sitter
/// shape: bytes `[start, old_end)` of the previous input were replaced
/// with `[start, new_end)` in the new input. `start == old_end` is a
/// pure insertion; `start == new_end` is a pure deletion.
pub const Edit = struct {
    start: u32,
    old_end: u32,
    new_end: u32,
};

/// Shift a single absolute byte position past `edit.old_end` by the
/// edit's signed size delta. Positions before `edit.old_end` (including
/// those inside the deleted span) are returned unchanged -- they belong
/// to invalidated entries that won't be replayed.
inline fn shiftPos(pos: u32, edit: Edit) u32 {
    if (pos < edit.old_end) return pos;
    return pos - edit.old_end + edit.new_end;
}

/// Apply `shiftPos` to every position field of `ev` in place. Used to
/// reanchor cached events in `memo_events_buf` after an edit so a
/// later cache hit replays positions in post-edit coordinates.
fn shiftEventPos(ev: *CaptureTree.Event, edit: Edit) void {
    switch (ev.*) {
        .open => |*m| m.pos = shiftPos(m.pos, edit),
        .close => |*m| m.pos = shiftPos(m.pos, edit),
        .partial_close => |*m| m.pos = shiftPos(m.pos, edit),
        .error_open => |*m| m.pos = shiftPos(m.pos, edit),
        .error_close => |*m| m.pos = shiftPos(m.pos, edit),
        .missing => |*m| m.pos = shiftPos(m.pos, edit),
        .field_marker => |*m| m.pos = shiftPos(m.pos, edit),
        .token => |*t| {
            t.start = shiftPos(t.start, edit);
            t.end = shiftPos(t.end, edit);
        },
    }
}

pub fn RuntimeState(comptime config: jit_abi.Config) type {
    return struct {
        const Self = @This();

        /// Open/close capture events appended during execution and
        /// truncated on backtrack. Backed by the shared
        /// `events_mod.State` so the C-ABI helpers can write into it
        /// without going through a Zig wrapper.
        events: if (config.capture_events) events_mod.State else void =
            if (config.capture_events) undefined else {},

        /// Allocator that backs every memo allocation (table, side,
        /// heads, events buf). Stored so `deinit` can free without the
        /// caller having to thread it through.
        memo_allocator: if (config.memoize) std.mem.Allocator else void =
            if (config.memoize) undefined else {},
        /// `[memo_rule_count * memo_stride]memo.Entry`. JIT allocates
        /// the full size at init (input known); AOT grows it lazily in
        /// `ensureMemoCapacity` since input is per-execute.
        memo_table: if (config.memoize) []memo_mod.Entry else void =
            if (config.memoize) &.{} else {},
        /// `input.len + 1`. The JIT-emitted memo_call code reads it via
        /// `MemoCtx.stride`.
        memo_stride: if (config.memoize) usize else void =
            if (config.memoize) 0 else {},
        /// Number of memoized rules in the compiled grammar. Used as
        /// the bitset length when `setupLr` allocates new `Head`s and
        /// to size the memo table.
        memo_rule_count: if (config.memoize) u16 else void =
            if (config.memoize) 0 else {},
        /// Per-position active head + heads pool. Backs the LR (Warth)
        /// machinery; not touched on non-LR runs.
        memo_heads: if (config.memoize) memo_mod.Heads else void =
            if (config.memoize) undefined else {},
        /// Side table holding memo frame data. Grown as frames are
        /// pushed; never indexed by stack depth.
        memo_side: if (config.memoize) memo_mod.Side else void =
            if (config.memoize) undefined else {},
        /// Append-only events buffer for cached event ranges. Only
        /// present when both memoize and capture_events are on.
        memo_events_buf: if (config.memoize and config.capture_events) memo_mod.EventsBuf else void =
            if (config.memoize and config.capture_events) undefined else {},
        /// Backing storage for `MemoCtx` populated each execute. The
        /// JIT prologue loads `&memo_ctx` into a stack slot.
        memo_ctx: if (config.memoize) jit_abi.MemoCtx else void =
            if (config.memoize) undefined else {},
        /// Per-memo-rule lookahead bound from `LookaheadAnalysis`,
        /// indexed by memo rule id. Borrowed; the host (Jit/AotRuntime)
        /// owns the underlying storage and must keep it alive for the
        /// RuntimeState's lifetime. Empty slice means every rule is
        /// treated as unbounded -- legitimate for hosts that don't run
        /// `applyEdit`.
        examined_max: if (config.memoize) []const u32 else void =
            if (config.memoize) &[_]u32{} else {},

        /// Constructor for the no-flags config. Flagged configs go
        /// through `initEvents` / `initPackrat` instead; their host's
        /// plain `init` is gated off so this path is unreachable for
        /// them.
        pub fn empty() Self {
            return .{};
        }

        /// Constructor for `capture_events && !memoize`. Initializes
        /// the live event log; everything else stays at the empty
        /// defaults.
        pub fn initEvents(allocator: std.mem.Allocator) Self {
            comptime std.debug.assert(config.capture_events and !config.memoize);
            return .{ .events = events_mod.State.init(allocator) };
        }

        /// Constructor for backends that bind to a single input at init
        /// time (JIT). Sizes the memo table for `input_len`; later
        /// `execute()` calls reuse it without growing (the input is
        /// fixed). Use `initPackratLazy` instead for backends whose
        /// input is per-execute. `examined_max` is borrowed from the
        /// host; pass `&.{}` to opt out of `applyEdit` precision.
        pub fn initPackratEager(
            allocator: std.mem.Allocator,
            memo_rule_count: u16,
            input_len: usize,
            examined_max: []const u32,
        ) !Self {
            comptime std.debug.assert(config.memoize);
            return initPackratInternal(allocator, memo_rule_count, input_len + 1, examined_max);
        }

        /// Constructor for backends that size the memo table per-execute
        /// (AOT). Defers all memo-table allocation to the first
        /// `beginExecute` call, where `ensureMemoCapacity` grows it to
        /// `input.len + 1`. Use `initPackratEager` instead when the
        /// input is known at init. `examined_max` is borrowed from the
        /// host; pass `&.{}` to opt out of `applyEdit` precision.
        pub fn initPackratLazy(
            allocator: std.mem.Allocator,
            memo_rule_count: u16,
            examined_max: []const u32,
        ) !Self {
            comptime std.debug.assert(config.memoize);
            return initPackratInternal(allocator, memo_rule_count, 0, examined_max);
        }

        fn initPackratInternal(
            allocator: std.mem.Allocator,
            memo_rule_count: u16,
            initial_stride: usize,
            examined_max: []const u32,
        ) !Self {
            std.debug.assert(examined_max.len == 0 or examined_max.len == memo_rule_count);
            var heads = try memo_mod.Heads.init(allocator, initial_stride);
            errdefer heads.deinit();
            var side = memo_mod.Side.init(allocator);
            errdefer side.deinit();
            var self: Self = .{
                .memo_allocator = allocator,
                .memo_rule_count = memo_rule_count,
                .memo_stride = initial_stride,
                .memo_heads = heads,
                .memo_side = side,
                .examined_max = examined_max,
            };
            if (memo_rule_count > 0 and initial_stride > 0) {
                const table = try allocator.alloc(
                    memo_mod.Entry,
                    @as(usize, memo_rule_count) * initial_stride,
                );
                @memset(table, .{ .state = .empty, .next_pos_or_frame = 0 });
                self.memo_table = table;
            }
            if (config.capture_events) {
                self.events = events_mod.State.init(allocator);
                self.memo_events_buf = memo_mod.EventsBuf.init(allocator);
            }
            return self;
        }

        pub fn deinit(self: *Self) void {
            if (config.capture_events) self.events.deinit();
            if (config.memoize) {
                if (config.capture_events) self.memo_events_buf.deinit();
                self.memo_heads.deinit();
                self.memo_side.deinit();
                if (self.memo_table.len > 0) self.memo_allocator.free(self.memo_table);
            }
        }

        /// Reset for a new execute. Grows the memo table on first use
        /// and on input growth; clears events, heads, side, and the
        /// cached-events buffer. Returns OOM on allocation failure.
        pub fn beginExecute(self: *Self, input_len: usize) !void {
            if (config.capture_events) self.events.clear();
            if (config.memoize) {
                try self.ensureMemoCapacity(input_len);
                if (self.memo_rule_count > 0) {
                    const used = @as(usize, self.memo_rule_count) * self.memo_stride;
                    @memset(
                        self.memo_table[0..used],
                        .{ .state = .empty, .next_pos_or_frame = 0 },
                    );
                }
                self.memo_heads.clear();
                self.memo_side.clear();
                if (config.capture_events) {
                    self.memo_events_buf.list.clearRetainingCapacity();
                }
            }
        }

        /// Resize memo state to fit `input_len`. No-op when the existing
        /// allocation already covers it. JIT never grows past its init
        /// size (input is fixed); AOT grows on the first execute and on
        /// any later input that exceeds the previous high-water mark.
        fn ensureMemoCapacity(self: *Self, input_len: usize) !void {
            const stride = input_len + 1;
            const needed_table = @as(usize, self.memo_rule_count) * stride;
            if (needed_table > self.memo_table.len) {
                if (self.memo_table.len > 0) self.memo_allocator.free(self.memo_table);
                self.memo_table = try self.memo_allocator.alloc(memo_mod.Entry, needed_table);
            }
            self.memo_stride = stride;
            if (stride > self.memo_heads.arr.len) {
                self.memo_allocator.free(self.memo_heads.arr);
                self.memo_heads.arr = try self.memo_allocator.alloc(u32, stride);
            }
        }

        /// Reset for a re-execute that *reuses* the existing memo
        /// table. Call after `applyEdit` (or in any case where the
        /// caller wants the memo cache to survive across calls); the
        /// per-execute scratch (events, heads, side) is cleared but
        /// `memo_table` and `memo_events_buf` stay live so cached
        /// `(rule, pos)` results from previous executes can still hit.
        ///
        /// `input_len` must match the post-edit input length: the
        /// caller is responsible for keeping `applyEdit` in lockstep
        /// with the actual input bytes.
        pub fn beginExecuteReusing(self: *Self, input_len: usize) !void {
            if (config.capture_events) self.events.clear();
            if (config.memoize) {
                std.debug.assert(input_len + 1 == self.memo_stride);
                self.memo_heads.clear();
                self.memo_side.clear();
            }
        }

        /// Update memo state in place to reflect a contiguous byte-range
        /// edit on the input. Cells whose rule could have read into the
        /// edit zone are dropped; cells past the edit are shifted by
        /// the size delta; cached events have their absolute positions
        /// shifted. The next `beginExecuteReusing(new_input_len)` call
        /// sees a table that's correctly indexed for the post-edit
        /// input.
        ///
        /// Per-rule precision comes from `examined_max[rule_id]`
        /// (populated by `LookaheadAnalysis`). A prefix entry at
        /// `(rule, p)` is kept iff `p + examined_max[rule_id] <=
        /// edit.start`, i.e. the rule's body provably could not have
        /// read any byte the edit changes. Rules with
        /// `examined_max == unbounded_value` (recursive cycles, `*`/`+`
        /// loops, or grammars compiled without the analysis) always
        /// invalidate in the prefix.
        pub fn applyEdit(self: *Self, edit: Edit) !void {
            comptime std.debug.assert(config.memoize);
            std.debug.assert(edit.start <= edit.old_end);
            std.debug.assert(edit.start <= edit.new_end);
            std.debug.assert(self.memo_stride > 0);
            std.debug.assert(@as(usize, edit.old_end) < self.memo_stride);

            const old_stride = self.memo_stride;
            const old_len_in_text: usize = old_stride - 1;
            const deleted: usize = edit.old_end - edit.start;
            const inserted: usize = edit.new_end - edit.start;
            const new_len_in_text: usize = old_len_in_text - deleted + inserted;
            const new_stride: usize = new_len_in_text + 1;

            if (self.memo_rule_count > 0) {
                const new_table = try self.memo_allocator.alloc(
                    memo_mod.Entry,
                    @as(usize, self.memo_rule_count) * new_stride,
                );
                @memset(new_table, .{ .state = .empty, .next_pos_or_frame = 0 });

                for (0..self.memo_rule_count) |r| {
                    const old_row = r * old_stride;
                    const new_row = r * new_stride;
                    const rule_examined_max: u32 =
                        if (r < self.examined_max.len) self.examined_max[r] else lookahead_unbounded;

                    // Prefix [0, edit.start): same column in new table.
                    // Keep .success / .fail iff the rule's body could
                    // not have read any edit-zone byte. `.lr` is per-
                    // execute scratch; should never survive a normal
                    // exit, but drop it defensively.
                    var p: u32 = 0;
                    while (p < edit.start) : (p += 1) {
                        const e = self.memo_table[old_row + p];
                        switch (e.state) {
                            .empty, .lr => {},
                            .success, .fail => {
                                if (canKeepPrefix(p, rule_examined_max, edit.start)) {
                                    new_table[new_row + p] = e;
                                }
                            },
                        }
                    }

                    // [edit.start, edit.old_end): deleted bytes -- drop.
                    // [edit.start, edit.new_end) in the new table: fresh
                    // bytes, left at .empty by the @memset above.

                    // Suffix [edit.old_end, old_stride): shift by delta.
                    // p - edit.old_end >= 0 so the arithmetic is u32-safe.
                    p = edit.old_end;
                    while (p < old_stride) : (p += 1) {
                        const e = self.memo_table[old_row + p];
                        const new_p: u32 = p - edit.old_end + edit.new_end;
                        switch (e.state) {
                            .empty, .lr => {},
                            .fail => {
                                new_table[new_row + new_p] = e;
                            },
                            .success => {
                                new_table[new_row + new_p] = .{
                                    .state = .success,
                                    .next_pos_or_frame = e.next_pos_or_frame - edit.old_end + edit.new_end,
                                    .events_start = e.events_start,
                                    .events_count = e.events_count,
                                };
                            },
                        }
                    }
                }

                self.memo_allocator.free(self.memo_table);
                self.memo_table = new_table;
            }

            // Shift absolute positions inside the cached-events buffer.
            // Events belonging to invalidated entries become unreachable
            // but stay in the buffer (append-only); we leave their bytes
            // alone so the indices of surviving entries stay stable.
            if (config.capture_events) {
                for (self.memo_events_buf.list.items) |*ev| {
                    shiftEventPos(ev, edit);
                }
            }

            // Grow the per-position heads array if the new stride
            // exceeds it; clearing happens in beginExecuteReusing.
            if (new_stride > self.memo_heads.arr.len) {
                self.memo_allocator.free(self.memo_heads.arr);
                self.memo_heads.arr = try self.memo_allocator.alloc(u32, new_stride);
            }

            self.memo_stride = new_stride;
        }

        /// Populate `memo_ctx` with pointers into this state plus the
        /// caller-supplied stack/jump-table/code-base pointers. Call
        /// once per execute after `beginExecute`.
        pub fn populateMemoCtx(
            self: *Self,
            stack_ptr: u64,
            jump_table_ptr: u64,
            code_base: u64,
        ) void {
            comptime std.debug.assert(config.memoize);
            self.memo_ctx = .{
                .table_ptr = @intFromPtr(self.memo_table.ptr),
                .stride = self.memo_stride,
                .side_ptr = @intFromPtr(&self.memo_side),
                .events_buf_ptr = if (config.capture_events) @intFromPtr(&self.memo_events_buf) else 0,
                .events_state_ptr = if (config.capture_events) @intFromPtr(&self.events) else 0,
                .stack_ptr = stack_ptr,
                .jump_table_ptr = jump_table_ptr,
                .code_base = code_base,
                .heads_ptr = @intFromPtr(&self.memo_heads),
                .memo_rule_count = self.memo_rule_count,
                .helper_call_begin = @intFromPtr(&memo_mod.helperMemoCallBegin),
                .helper_cached_slice = @intFromPtr(&memo_mod.helperMemoCachedSlice),
                .helper_replay_events = if (config.capture_events) @intFromPtr(&memo_mod.helperMemoReplayEvents) else 0,
                .helper_ret = @intFromPtr(&memo_mod.helperMemoRet),
                .helper_backtrack = @intFromPtr(&memo_mod.helperMemoBacktrack),
            };
        }

        /// `JitCtx.events_state_ptr` value (or 0 when capture_events
        /// is off).
        pub fn eventsStatePtr(self: *Self) u64 {
            return if (config.capture_events) @intFromPtr(&self.events) else 0;
        }

        /// `JitCtx.memo_ctx_ptr` value (or 0 when memoize is off).
        pub fn memoCtxPtr(self: *Self) u64 {
            return if (config.memoize) @intFromPtr(&self.memo_ctx) else 0;
        }

        /// Build a capture tree from the events recorded on the last
        /// execute. Only available when `config.capture_events` is true.
        pub const buildCaptureTree = if (config.capture_events)
            struct {
                fn f(
                    self: *const Self,
                    tree_allocator: std.mem.Allocator,
                ) CaptureTree.BuildError!CaptureTree.Tree {
                    return CaptureTree.buildFromEvents(tree_allocator, self.events.items());
                }
            }.f
        else {};

        /// Raw view of recorded capture events. Valid until the next
        /// execute or deinit. Only available when `config.capture_events`
        /// is true.
        pub const getCaptureEvents = if (config.capture_events)
            struct {
                fn f(self: *const Self) []const CaptureTree.Event {
                    return self.events.items();
                }
            }.f
        else {};
    };
}

const testing = std.testing;

const TestState = RuntimeState(.{ .memoize = true, .capture_events = true });

/// Set the entry at `(rule, pos)` in the memo table. The state must
/// already have been sized for `pos` (i.e. `beginExecute` was called).
fn setEntry(state: *TestState, rule: u16, pos: u32, entry: memo_mod.Entry) void {
    const idx = @as(usize, rule) * state.memo_stride + pos;
    state.memo_table[idx] = entry;
}

fn getEntry(state: *const TestState, rule: u16, pos: u32) memo_mod.Entry {
    const idx = @as(usize, rule) * state.memo_stride + pos;
    return state.memo_table[idx];
}

test "applyEdit: pure insertion shifts suffix and grows stride" {
    // Rule 0's lookahead bound is 2 (matches the cached entry's match
    // length); the prefix entry at p=2 thus reaches byte 4 < edit.start=5
    // and is kept.
    const examined = [_]u32{2};
    var state = try TestState.initPackratLazy(testing.allocator, 1, &examined);
    defer state.deinit();
    try state.beginExecute(10);

    setEntry(&state, 0, 2, .{ .state = .success, .next_pos_or_frame = 4 });
    setEntry(&state, 0, 7, .{ .state = .success, .next_pos_or_frame = 9 });

    try state.applyEdit(.{ .start = 5, .old_end = 5, .new_end = 8 });

    try testing.expectEqual(@as(usize, 14), state.memo_stride);
    try testing.expectEqual(memo_mod.State.success, getEntry(&state, 0, 2).state);
    try testing.expectEqual(@as(u32, 4), getEntry(&state, 0, 2).next_pos_or_frame);
    try testing.expectEqual(memo_mod.State.success, getEntry(&state, 0, 10).state);
    try testing.expectEqual(@as(u32, 12), getEntry(&state, 0, 10).next_pos_or_frame);
    try testing.expectEqual(memo_mod.State.empty, getEntry(&state, 0, 7).state);
}

test "applyEdit: pure deletion shifts suffix back and shrinks stride" {
    const examined = [_]u32{2};
    var state = try TestState.initPackratLazy(testing.allocator, 1, &examined);
    defer state.deinit();
    try state.beginExecute(10);

    setEntry(&state, 0, 1, .{ .state = .success, .next_pos_or_frame = 3 });
    setEntry(&state, 0, 8, .{ .state = .success, .next_pos_or_frame = 10 });

    try state.applyEdit(.{ .start = 4, .old_end = 7, .new_end = 4 });

    try testing.expectEqual(@as(usize, 8), state.memo_stride);
    try testing.expectEqual(memo_mod.State.success, getEntry(&state, 0, 1).state);
    try testing.expectEqual(@as(u32, 3), getEntry(&state, 0, 1).next_pos_or_frame);
    try testing.expectEqual(memo_mod.State.success, getEntry(&state, 0, 5).state);
    try testing.expectEqual(@as(u32, 7), getEntry(&state, 0, 5).next_pos_or_frame);
}

test "applyEdit: replacement invalidates entries that read into the edit zone" {
    // Rule 0 reads up to 4 bytes past entry; rule 1 reads up to 4 too.
    // Edit at [4, 5)→[4, 5).
    // (0, p=2): 2 + 4 = 6 > 4 -> invalidate.
    // (1, p=0): 0 + 4 = 4 <= 4 -> keep.
    const examined = [_]u32{ 4, 4 };
    var state = try TestState.initPackratLazy(testing.allocator, 2, &examined);
    defer state.deinit();
    try state.beginExecute(10);

    setEntry(&state, 0, 2, .{ .state = .success, .next_pos_or_frame = 6 });
    setEntry(&state, 1, 0, .{ .state = .success, .next_pos_or_frame = 4 });

    try state.applyEdit(.{ .start = 4, .old_end = 5, .new_end = 5 });

    try testing.expectEqual(@as(usize, 11), state.memo_stride);
    try testing.expectEqual(memo_mod.State.empty, getEntry(&state, 0, 2).state);
    try testing.expectEqual(memo_mod.State.success, getEntry(&state, 1, 0).state);
    try testing.expectEqual(@as(u32, 4), getEntry(&state, 1, 0).next_pos_or_frame);
}

test "applyEdit: prefix .fail entries are conservatively invalidated" {
    var state = try TestState.initPackratLazy(testing.allocator, 1, &.{});
    defer state.deinit();
    try state.beginExecute(10);

    setEntry(&state, 0, 2, .{ .state = .fail, .next_pos_or_frame = 0 });
    setEntry(&state, 0, 8, .{ .state = .fail, .next_pos_or_frame = 0 });

    try state.applyEdit(.{ .start = 4, .old_end = 6, .new_end = 4 });

    try testing.expectEqual(memo_mod.State.empty, getEntry(&state, 0, 2).state);
    try testing.expectEqual(memo_mod.State.fail, getEntry(&state, 0, 6).state);
}

test "applyEdit: prefix .success entries past the edit are kept verbatim" {
    const examined = [_]u32{2};
    var state = try TestState.initPackratLazy(testing.allocator, 1, &examined);
    defer state.deinit();
    try state.beginExecute(10);

    setEntry(&state, 0, 1, .{ .state = .success, .next_pos_or_frame = 3 });

    try state.applyEdit(.{ .start = 5, .old_end = 7, .new_end = 5 });

    try testing.expectEqual(memo_mod.State.success, getEntry(&state, 0, 1).state);
    try testing.expectEqual(@as(u32, 3), getEntry(&state, 0, 1).next_pos_or_frame);
}

test "applyEdit: insertion at end keeps prefix entries within bound" {
    // Rule 0 reads up to 5 bytes past entry. Edit at byte 5.
    // Both the (rule, p=0) success and the (rule, p=2) fail are kept
    // because their reads are bounded by 0+5=5 and 2+5=7 respectively...
    // wait, 2+5=7 > 5 so the .fail at p=2 must be invalidated. Rebalance
    // by setting examined_max = 5 and only checking the entry that fits.
    const examined = [_]u32{5};
    var state = try TestState.initPackratLazy(testing.allocator, 1, &examined);
    defer state.deinit();
    try state.beginExecute(5);

    // 0 + 5 = 5 <= 5 -> keep.
    setEntry(&state, 0, 0, .{ .state = .success, .next_pos_or_frame = 5 });
    // 2 + 5 = 7 > 5 -> invalidate.
    setEntry(&state, 0, 2, .{ .state = .fail, .next_pos_or_frame = 0 });

    try state.applyEdit(.{ .start = 5, .old_end = 5, .new_end = 8 });

    try testing.expectEqual(@as(usize, 9), state.memo_stride);
    try testing.expectEqual(memo_mod.State.success, getEntry(&state, 0, 0).state);
    try testing.expectEqual(@as(u32, 5), getEntry(&state, 0, 0).next_pos_or_frame);
    try testing.expectEqual(memo_mod.State.empty, getEntry(&state, 0, 2).state);
}

test "applyEdit: insertion at start shifts everything past it" {
    var state = try TestState.initPackratLazy(testing.allocator, 1, &.{});
    defer state.deinit();
    try state.beginExecute(5);

    setEntry(&state, 0, 0, .{ .state = .success, .next_pos_or_frame = 3 });
    setEntry(&state, 0, 4, .{ .state = .success, .next_pos_or_frame = 5 });

    try state.applyEdit(.{ .start = 0, .old_end = 0, .new_end = 2 });

    try testing.expectEqual(@as(usize, 8), state.memo_stride);
    try testing.expectEqual(memo_mod.State.success, getEntry(&state, 0, 2).state);
    try testing.expectEqual(@as(u32, 5), getEntry(&state, 0, 2).next_pos_or_frame);
    try testing.expectEqual(memo_mod.State.success, getEntry(&state, 0, 6).state);
    try testing.expectEqual(@as(u32, 7), getEntry(&state, 0, 6).next_pos_or_frame);
}

test "applyEdit: cached event positions shift past the edit" {
    var state = try TestState.initPackratLazy(testing.allocator, 1, &.{});
    defer state.deinit();
    try state.beginExecute(10);

    try state.memo_events_buf.list.appendSlice(testing.allocator, &.{
        .{ .open = .{ .group_id = 0, .pos = 1 } }, // before edit
        .{ .close = .{ .group_id = 0, .pos = 7 } }, // after edit
        .{ .token = .{ .start = 8, .end = 10 } }, // both after edit
        .{ .field_marker = .{ .field_id = 3, .pos = 4 } }, // inside deleted span -- left as-is
    });

    try state.applyEdit(.{ .start = 3, .old_end = 6, .new_end = 4 });

    const evs = state.memo_events_buf.list.items;
    try testing.expectEqual(@as(u32, 1), evs[0].open.pos);
    try testing.expectEqual(@as(u32, 5), evs[1].close.pos);
    try testing.expectEqual(@as(u32, 6), evs[2].token.start);
    try testing.expectEqual(@as(u32, 8), evs[2].token.end);
    // Inside the deleted span: pos < old_end, so no shift.
    try testing.expectEqual(@as(u32, 4), evs[3].field_marker.pos);
}

test "applyEdit: heads array grows when stride exceeds capacity" {
    var state = try TestState.initPackratLazy(testing.allocator, 1, &.{});
    defer state.deinit();
    try state.beginExecute(5);
    const old_arr_len = state.memo_heads.arr.len;

    try state.applyEdit(.{ .start = 2, .old_end = 2, .new_end = 12 });

    try testing.expect(state.memo_heads.arr.len >= state.memo_stride);
    try testing.expect(state.memo_heads.arr.len > old_arr_len);
}

test "applyEdit then beginExecuteReusing keeps the table" {
    const examined = [_]u32{1};
    var state = try TestState.initPackratLazy(testing.allocator, 1, &examined);
    defer state.deinit();
    try state.beginExecute(10);

    setEntry(&state, 0, 1, .{ .state = .success, .next_pos_or_frame = 2 });

    try state.applyEdit(.{ .start = 5, .old_end = 5, .new_end = 7 });
    try state.beginExecuteReusing(12);

    try testing.expectEqual(memo_mod.State.success, getEntry(&state, 0, 1).state);
    try testing.expectEqual(@as(u32, 2), getEntry(&state, 0, 1).next_pos_or_frame);
}

test "applyEdit: empty examined_max defaults to unbounded (always invalidate)" {
    var state = try TestState.initPackratLazy(testing.allocator, 1, &.{});
    defer state.deinit();
    try state.beginExecute(10);

    // Match span entirely before the edit -- but with unbounded
    // examined_max, the rule could in theory have peeked anywhere, so
    // the prefix entry is dropped.
    setEntry(&state, 0, 0, .{ .state = .success, .next_pos_or_frame = 2 });

    try state.applyEdit(.{ .start = 5, .old_end = 5, .new_end = 6 });

    try testing.expectEqual(memo_mod.State.empty, getEntry(&state, 0, 0).state);
}

test "applyEdit: unbounded sentinel value invalidates aggressively" {
    const examined = [_]u32{lookahead_unbounded};
    var state = try TestState.initPackratLazy(testing.allocator, 1, &examined);
    defer state.deinit();
    try state.beginExecute(10);

    setEntry(&state, 0, 0, .{ .state = .success, .next_pos_or_frame = 2 });

    try state.applyEdit(.{ .start = 5, .old_end = 5, .new_end = 6 });

    try testing.expectEqual(memo_mod.State.empty, getEntry(&state, 0, 0).state);
}
