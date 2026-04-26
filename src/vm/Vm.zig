/// Grammar parsing VM.
///
/// Executes bytecode produced by the Compiler. Uses a backtracking
/// stack for ordered choice and a call stack for rule invocations.
const std = @import("std");
const I = @import("Instruction.zig");
const CaptureTree = @import("CaptureTree.zig");
const events_mod = @import("events.zig");

pub const Config = struct {
    max_stack: u32 = 1024,
    max_captures: u16 = 64,
    enable_stats: bool = false,
    /// Upper bound on code length for per-instruction profiling arrays.
    /// Only used when `enable_stats` is true.
    max_code: u32 = 4096,
    /// Record open/close events for each capture save so a tree can be
    /// built in a post-pass. Off by default: execution stays
    /// allocation-free and JIT/AOT backends stay untouched. When enabled,
    /// construct the VM with `initEvents` (the plain `init` constructor
    /// is gated off in this config because it has no allocator to back
    /// the event log).
    capture_events: bool = false,
};

pub const Vm = VmWith(.{});

pub fn VmWith(comptime config: Config) type {
    return struct {
        const Self = @This();

        const max_stack = config.max_stack;
        const max_captures = config.max_captures;

        const Entry = union(enum) {
            /// Backtrack point: saved position and instruction to jump to on failure.
            choice: struct {
                pos: usize,
                pc: u32,
            },
            /// Return address for rule calls.
            ret: u32,
            /// Undo log for a save instruction: restore old value on backtrack.
            /// When `capture_events` is enabled, `event_len` holds the length
            /// of the event log prior to this save, so the log can be
            /// truncated back on backtrack in lockstep with the slot restore.
            save: struct {
                slot: u16,
                old: ?usize,
                event_len: if (config.capture_events) u32 else void =
                    if (config.capture_events) 0 else {},
            },
            /// Events-only save frame: pushed by `event_open`/`event_close`
            /// when capture_events is on. Carries no slot to restore;
            /// backtrack just truncates the events log.
            event: struct {
                event_len: if (config.capture_events) u32 else void =
                    if (config.capture_events) 0 else {},
            },
            /// Labeled-failure catch frame: pushed by `lcatch` when
            /// capture_events recovery is in use. On a matching `throw`,
            /// the unwinder stops here, synthesizes `partial_close`
            /// events for unclosed opens above this frame, and transfers
            /// control to `handler_pc` with `pos` left at the throw
            /// site. Regular `fail` walks past this frame untouched, so
            /// committed-choice semantics are preserved. `event_len` is
            /// the events-log length at push time, used as the lower
            /// bound for `synthesizePartialCloses`.
            lcatch: struct {
                label: u16,
                handler_pc: u32,
                event_len: if (config.capture_events) u32 else void =
                    if (config.capture_events) 0 else {},
            },
            /// Memo frame: pushed by `memo_call` on a table miss. Holds the
            /// information needed for (a) left-recursion detection via
            /// stack walking, (b) seed-growing re-entries, and (c) head
            /// attachment per Warth's paper.
            memo: struct {
                rule_id: u16,
                /// True if this is a RECALL re-eval frame pushed from within
                /// a grow iteration. Its ret only updates memo_table, never
                /// enters grow mode, and its backtrack writes .fail and keeps
                /// unwinding normally.
                is_recall: bool,
                start_pos: u32,
                return_pc: u32,
                rule_entry_pc: u32,
                /// sentinel = first eval; otherwise the current best end pos.
                best_end: u32,
                /// Head this frame is attached to (maxInt u32 = no head yet).
                head_idx: u32,
                /// Snapshot of `events.list.items.len` at the moment this
                /// frame was pushed. On success-write the body's events
                /// are `events[events_len_at_entry..]`, and we copy that
                /// slice into `memo_events` for replay on cache hits.
                /// Inherited unchanged across grow re-pushes so an LR
                /// rule's cached range covers all iterations, matching
                /// the events left in the live log by the original run.
                events_len_at_entry: if (config.capture_events) u32 else void =
                    if (config.capture_events) 0 else {},
            },
        };

        /// Sentinel meaning "first evaluation in progress, no grow seed yet".
        const grow_sentinel: u32 = std.math.maxInt(u32);
        /// Sentinel head index meaning "no head attached".
        const no_head: u32 = std.math.maxInt(u32);

        pub const MemoState = enum(u8) {
            /// Not yet attempted at this position.
            empty,
            /// Body currently being evaluated: a memo frame is live on the
            /// stack at `stack[next_pos_or_frame]` (we reuse the field as a
            /// stack index). Self/cycle recursion hits this state and returns
            /// the frame's current seed.
            lr,
            /// Cached successful match; next_pos_or_frame is end_pos.
            success,
            /// Cached failure.
            fail,
        };

        pub const MemoEntry = struct {
            state: MemoState,
            /// Meaning depends on `state`:
            ///   .success -> end position of the match.
            ///   .lr      -> stack index of the live memo frame.
            ///   other    -> unused.
            next_pos_or_frame: u32,
            /// Slice of `self.memo_events` holding the open/close events
            /// the rule's body produced on its memoized invocation.
            /// Replayed on a `.success` cache hit so the events log and
            /// capture slots match what re-running the body would do.
            /// Only populated when `config.capture_events` is true.
            events_start: if (config.capture_events) u32 else void =
                if (config.capture_events) 0 else {},
            events_count: if (config.capture_events) u32 else void =
                if (config.capture_events) 0 else {},
        };

        const Head = struct {
            rule_id: u16,
            involved: std.DynamicBitSetUnmanaged,
            eval: std.DynamicBitSetUnmanaged,
        };

        pub const Span = struct {
            start: usize,
            end: usize,
        };

        pub const Trace = struct {
            writer: *Writer,
        };

        const opcode_count = std.meta.fields(I.Opcode).len;

        /// Per-instruction profiling counters.
        pub const InstStat = struct {
            exec_count: u64,
            backtrack_count: u64,
        };

        /// Snapshot of VM profiling statistics.
        pub const Stats = struct {
            steps: u64,
            opcode_exec_counts: [opcode_count]u64,
            opcode_backtrack_counts: [opcode_count]u64,
            /// Per-instruction stats, sliced to the actual code length.
            inst_stats: ?[]const InstStat,
        };

        code: []const I.Inst,
        charsets: []const I.Charset,
        string_data: []const u8,
        input: []const u8,
        trace: ?Trace = null,
        captures: [max_captures]?usize = .{null} ** max_captures,
        /// Number of bytecode instructions dispatched during the last execute().
        /// Useful for measuring the work saved by packrat memoization.
        steps: u64 = 0,
        /// Memo table for packrat execution. Empty unless initPackrat was used.
        /// Layout: entry(rule_id, pos) = memo_table[rule_id * stride + pos],
        /// where stride = input.len + 1.
        memo_table: []MemoEntry = &.{},
        memo_rule_count: u16 = 0,
        memo_allocator: ?std.mem.Allocator = null,
        /// Per-position head indices for Warth's seed-growing. Length = stride.
        heads: []u32 = &.{},
        heads_pool: std.ArrayListUnmanaged(Head) = .empty,

        opcode_exec_counts: [opcode_count]u64 = .{0} ** opcode_count,
        opcode_backtrack_counts: [opcode_count]u64 = .{0} ** opcode_count,
        inst_stats: if (config.enable_stats) [config.max_code]InstStat else void =
            if (config.enable_stats) .{InstStat{ .exec_count = 0, .backtrack_count = 0 }} ** config.max_code else {},

        /// Open/close capture events appended during execution and truncated
        /// on backtrack. Only present when `config.capture_events` is true.
        /// Backed by the shared `events_mod.State` so the JIT path can
        /// append to the same structure through its C-ABI helpers.
        events: if (config.capture_events) ?events_mod.State else void =
            if (config.capture_events) null else {},
        /// Append-only buffer of events captured by memoized rules. A
        /// memo entry's `events_start..events_start+events_count` slices
        /// into this buffer. Append-only because earlier entries must
        /// remain valid even as later iterations cache more events.
        /// Allocated by the packrat constructor when capture_events is
        /// on; freed in `deinit`.
        memo_events: if (config.capture_events)
            std.ArrayListUnmanaged(CaptureTree.Event)
        else
            void = if (config.capture_events) .empty else {},

        pub const Writer = std.Io.Writer;

        pub fn getStats(self: *const Self) Stats {
            return .{
                .steps = self.steps,
                .opcode_exec_counts = self.opcode_exec_counts,
                .opcode_backtrack_counts = self.opcode_backtrack_counts,
                .inst_stats = if (config.enable_stats)
                    self.inst_stats[0..self.code.len]
                else
                    null,
            };
        }

        fn resetStats(self: *Self) void {
            self.opcode_exec_counts = .{0} ** opcode_count;
            self.opcode_backtrack_counts = .{0} ** opcode_count;
            if (config.enable_stats) {
                @memset(self.inst_stats[0..self.code.len], .{ .exec_count = 0, .backtrack_count = 0 });
            }
        }

        /// Current length of the live events log (0 when capture_events
        /// is off). Used as both the snapshot recorded at memo-frame
        /// push time and the lower bound of the cached events range
        /// when a body completes successfully.
        inline fn currentEventsLen(self: *const Self) u32 {
            if (config.capture_events) {
                if (self.events) |*s| return @intCast(s.list.items.len);
            }
            return 0;
        }

        /// Copy `events[entry_len..now_len]` - the events the rule's
        /// body just appended - into `memo_events`, and return the
        /// (start, count) the memo entry should record. No-op (zeros)
        /// when capture_events is off or the rule produced no events.
        fn cacheRuleEvents(self: *Self, entry_len: u32) !struct { start: u32, count: u32 } {
            if (!config.capture_events) return .{ .start = 0, .count = 0 };
            const a = self.memo_allocator orelse return .{ .start = 0, .count = 0 };
            const live = if (self.events) |*s| s.list.items else return .{ .start = 0, .count = 0 };
            if (live.len <= entry_len) return .{ .start = 0, .count = 0 };
            const slice = live[entry_len..];
            const start: u32 = @intCast(self.memo_events.items.len);
            try self.memo_events.appendSlice(a, slice);
            return .{ .start = start, .count = @intCast(slice.len) };
        }

        /// Write `memo_table[idx] = .success` with the cached events
        /// range `m`'s body produced. When capture_events is off this
        /// degenerates to the original single-field write.
        fn writeMemoSuccess(self: *Self, idx: usize, end_pos: u32, m: anytype) !void {
            if (config.capture_events) {
                const cached = try self.cacheRuleEvents(m.events_len_at_entry);
                self.memo_table[idx] = .{
                    .state = .success,
                    .next_pos_or_frame = end_pos,
                    .events_start = cached.start,
                    .events_count = cached.count,
                };
            } else {
                self.memo_table[idx] = .{ .state = .success, .next_pos_or_frame = end_pos };
            }
        }

        /// Replay a cached events range on a `.success` cache hit:
        /// push a save frame for each event so backtrack truncates
        /// correctly, append the event to the live log, and write the
        /// matching capture slot. Returns true on success, false if
        /// the backtrack stack would overflow.
        fn replayCachedEvents(
            self: *Self,
            stack: *[max_stack]Entry,
            sp: *usize,
            entry: MemoEntry,
        ) !bool {
            if (!config.capture_events) return true;
            if (entry.events_count == 0) return true;
            const state = if (self.events) |*s| s else return true;
            const cached = self.memo_events.items[entry.events_start..][0..entry.events_count];
            for (cached) |ev| {
                if (sp.* >= max_stack) return false;
                switch (ev) {
                    .open => |m| {
                        const slot: u16 = m.group_id << 1;
                        const event_len = try events_mod.appendSave(state, slot, m.pos);
                        stack[sp.*] = .{ .save = .{
                            .slot = slot,
                            .old = self.captures[slot],
                            .event_len = event_len,
                        } };
                        sp.* += 1;
                        self.captures[slot] = m.pos;
                    },
                    .close => |m| {
                        const slot: u16 = (m.group_id << 1) | 1;
                        const event_len = try events_mod.appendSave(state, slot, m.pos);
                        stack[sp.*] = .{ .save = .{
                            .slot = slot,
                            .old = self.captures[slot],
                            .event_len = event_len,
                        } };
                        sp.* += 1;
                        self.captures[slot] = m.pos;
                    },
                    // Recovery-era variants don't write capture slots
                    // (they're tree-only). An events-only undo frame is
                    // sufficient: backtrack truncates the log back if
                    // the caller of the memoized rule fails afterwards.
                    .partial_close => |m| {
                        const event_len = try events_mod.appendPartialClose(state, m.group_id, m.pos);
                        stack[sp.*] = .{ .event = .{ .event_len = event_len } };
                        sp.* += 1;
                    },
                    .error_open => |m| {
                        const event_len = try events_mod.appendErrorOpen(state, m.group_id, m.pos);
                        stack[sp.*] = .{ .event = .{ .event_len = event_len } };
                        sp.* += 1;
                    },
                    .error_close => |m| {
                        const event_len = try events_mod.appendErrorClose(state, m.group_id, m.pos);
                        stack[sp.*] = .{ .event = .{ .event_len = event_len } };
                        sp.* += 1;
                    },
                    .missing => |m| {
                        const event_len = try events_mod.appendMissing(state, m.group_id, m.pos);
                        stack[sp.*] = .{ .event = .{ .event_len = event_len } };
                        sp.* += 1;
                    },
                    .token => |t| {
                        const event_len = try events_mod.appendToken(state, t.start, t.end);
                        stack[sp.*] = .{ .event = .{ .event_len = event_len } };
                        sp.* += 1;
                    },
                    .field_marker => |fm| {
                        const event_len = try events_mod.appendField(state, fm.field_id, fm.pos);
                        stack[sp.*] = .{ .event = .{ .event_len = event_len } };
                        sp.* += 1;
                    },
                }
            }
            return true;
        }

        inline fn recordExec(self: *Self, pc: u32, op: I.Opcode) void {
            self.opcode_exec_counts[@intFromEnum(op)] += 1;
            if (config.enable_stats) {
                self.inst_stats[pc].exec_count += 1;
            }
        }

        inline fn recordBacktrack(self: *Self, pc: u32, op: I.Opcode) void {
            self.opcode_backtrack_counts[@intFromEnum(op)] += 1;
            if (config.enable_stats) {
                self.inst_stats[pc].backtrack_count += 1;
            }
        }

        /// Default constructor. Unavailable when `config.capture_events`
        /// is true - use `initEvents` instead, which supplies the
        /// allocator that backs the event log.
        pub const init = if (config.capture_events) {} else struct {
            fn f(code: []const I.Inst, charsets: []const I.Charset, string_data: []const u8, input: []const u8) Self {
                return .{ .code = code, .charsets = charsets, .string_data = string_data, .input = input };
            }
        }.f;

        /// Constructor for event-recording runs. Only available when
        /// `config.capture_events` is true. The allocator backs the event
        /// log; call `deinit` to release it.
        pub const initEvents = if (config.capture_events)
            struct {
                fn f(
                    allocator: std.mem.Allocator,
                    code: []const I.Inst,
                    charsets: []const I.Charset,
                    string_data: []const u8,
                    input: []const u8,
                ) Self {
                    return .{
                        .code = code,
                        .charsets = charsets,
                        .string_data = string_data,
                        .input = input,
                        .events = events_mod.State.init(allocator),
                    };
                }
            }.f
        else {};

        /// Packrat constructor. Allocates a memo table sized for `memo_rule_count`
        /// rules and `input.len + 1` positions. Call `deinit` to free it. If
        /// `memo_rule_count` is 0 the call is equivalent to `init`.
        ///
        /// When `config.capture_events` is true, the same allocator backs
        /// both the live events log and the memo events buffer used to
        /// replay captures on a cache hit.
        pub const initPackrat = struct {
            fn f(
                allocator: std.mem.Allocator,
                code: []const I.Inst,
                charsets: []const I.Charset,
                string_data: []const u8,
                memo_rule_count: u16,
                input: []const u8,
            ) !Self {
                var vm = Self{
                    .code = code,
                    .charsets = charsets,
                    .string_data = string_data,
                    .input = input,
                    .memo_rule_count = memo_rule_count,
                };
                if (config.capture_events) {
                    vm.events = events_mod.State.init(allocator);
                }
                if (memo_rule_count > 0) {
                    const stride = input.len + 1;
                    const table = try allocator.alloc(MemoEntry, @as(usize, memo_rule_count) * stride);
                    @memset(table, .{ .state = .empty, .next_pos_or_frame = 0 });
                    const heads = try allocator.alloc(u32, stride);
                    @memset(heads, no_head);
                    vm.memo_table = table;
                    vm.heads = heads;
                    vm.memo_allocator = allocator;
                }
                return vm;
            }
        }.f;

        pub fn deinit(self: *Self) void {
            if (self.memo_allocator) |a| {
                a.free(self.memo_table);
                a.free(self.heads);
                for (self.heads_pool.items) |*h| {
                    h.involved.deinit(a);
                    h.eval.deinit(a);
                }
                self.heads_pool.deinit(a);
                self.memo_table = &.{};
                self.heads = &.{};
                if (config.capture_events) {
                    self.memo_events.deinit(a);
                }
                self.memo_allocator = null;
            }
            if (config.capture_events) {
                if (self.events) |*s| s.deinit();
            }
        }

        /// Run the VM. Returns the position after the match, or null on failure.
        /// Errors are only produced by the packrat path (allocating head
        /// state during Warth's seed-growing); the non-packrat path is
        /// infallible.
        pub fn execute(self: *Self) !?usize {
            var pc: u32 = 0;
            var pos: usize = 0;
            var stack: [max_stack]Entry = undefined;
            var sp: usize = 0;

            self.steps = 0;
            self.resetStats();
            if (config.capture_events) {
                if (self.events) |*s| s.clear();
            }
            while (pc < self.code.len) {
                const inst = self.code[pc];
                self.steps += 1;
                self.recordExec(pc, inst.op);
                self.traceStep(pc, pos, sp, inst);
                switch (inst.op) {
                    .char => {
                        if (pos < self.input.len and self.input[pos] == inst.data.byte) {
                            pos += 1;
                            pc += 1;
                        } else {
                            if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                            return null;
                        }
                    },
                    .string => {
                        const ref = inst.data.string;
                        const str = self.string_data[ref.offset..][0..ref.len];
                        if (pos + ref.len <= self.input.len and
                            std.mem.eql(u8, self.input[pos..][0..ref.len], str))
                        {
                            pos += ref.len;
                            pc += 1;
                        } else {
                            if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                            return null;
                        }
                    },
                    .any => {
                        if (pos < self.input.len) {
                            pos += 1;
                            pc += 1;
                        } else {
                            if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                            return null;
                        }
                    },
                    .set => {
                        const cs = self.charsets[inst.data.charset];
                        if (pos < self.input.len and I.charsetContains(cs, self.input[pos])) {
                            pos += 1;
                            pc += 1;
                        } else {
                            if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                            return null;
                        }
                    },
                    .neg_set => {
                        const cs = self.charsets[inst.data.charset];
                        if (pos < self.input.len and !I.charsetContains(cs, self.input[pos])) {
                            pos += 1;
                            pc += 1;
                        } else {
                            if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                            return null;
                        }
                    },
                    .optional_char => {
                        if (pos < self.input.len and self.input[pos] == inst.data.byte) {
                            pos += 1;
                        }
                        pc += 1;
                    },
                    .choice => {
                        if (sp >= max_stack) return null;
                        stack[sp] = .{ .choice = .{ .pos = pos, .pc = inst.data.offset } };
                        sp += 1;
                        pc += 1;
                    },
                    .commit => {
                        // Discard the matching backtrack entry (.choice
                        // or .lcatch) without disturbing any .save /
                        // .event frames a rule call inside the alt body
                        // pushed on top of it. Same shape as `.ret`'s
                        // splice for .ret/.memo - the intervening
                        // frames stay so the outer scope's backtrack
                        // can still undo them if it later fails.
                        // Only capture_events can land frames above the
                        // matching .choice; without it, the top frame
                        // is the choice itself, so a plain pop suffices.
                        if (config.capture_events) spliceCtrlFrame(&stack, &sp) else sp -= 1;
                        pc = inst.data.offset;
                    },
                    .fail => {
                        if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                        return null;
                    },
                    .fail_twice => {
                        // Predicate cut: discard the matching backtrack
                        // entry, then fail again. Same splice as commit
                        // so intervening event frames don't get popped
                        // out of order (which would let the predicate
                        // resume at the wrong choice and silently take
                        // the wrong branch).
                        if (config.capture_events) spliceCtrlFrame(&stack, &sp) else sp -= 1;
                        if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                        return null;
                    },
                    .jump => {
                        pc = inst.data.offset;
                    },
                    .call => {
                        if (sp >= max_stack) return null;
                        stack[sp] = .{ .ret = pc + 1 };
                        sp += 1;
                        pc = inst.data.offset;
                    },
                    .memo_call => {
                        const mc = inst.data.memo;
                        if (self.memo_table.len == 0) {
                            if (sp >= max_stack) return null;
                            stack[sp] = .{ .ret = pc + 1 };
                            sp += 1;
                            pc = mc.offset;
                        } else {
                            const stride = self.input.len + 1;
                            const idx = @as(usize, mc.rule_id) * stride + pos;

                            // RECALL. When a head is active at this position:
                            //  - rules outside involved set are cut off (fail)
                            //  - rules still in eval_set are re-evaluated in
                            //    place via a recall-frame; the memo_table
                            //    keeps its current seed so that nested
                            //    recursion inside the re-eval reads the seed.
                            const active_head = self.heads[pos];
                            if (active_head != no_head) {
                                const h = &self.heads_pool.items[active_head];
                                if (self.memo_table[idx].state == .empty and
                                    mc.rule_id != h.rule_id and
                                    !h.involved.isSet(mc.rule_id))
                                {
                                    if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                                    return null;
                                }
                                if (h.eval.isSet(mc.rule_id)) {
                                    h.eval.unset(mc.rule_id);
                                    if (sp >= max_stack) return null;
                                    stack[sp] = .{ .memo = .{
                                        .rule_id = mc.rule_id,
                                        .is_recall = true,
                                        .start_pos = @intCast(pos),
                                        .return_pc = pc + 1,
                                        .rule_entry_pc = mc.offset,
                                        .best_end = grow_sentinel,
                                        .head_idx = active_head,
                                        .events_len_at_entry = if (config.capture_events)
                                            self.currentEventsLen()
                                        else {},
                                    } };
                                    sp += 1;
                                    pc = mc.offset;
                                    continue;
                                }
                            }

                            switch (self.memo_table[idx].state) {
                                .success => {
                                    if (config.capture_events) {
                                        if (!try self.replayCachedEvents(&stack, &sp, self.memo_table[idx])) return null;
                                    }
                                    pos = self.memo_table[idx].next_pos_or_frame;
                                    pc += 1;
                                },
                                .fail => {
                                    if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                                    return null;
                                },
                                .lr => {
                                    // Re-entering a rule whose body is still
                                    // being evaluated. SETUP-LR: walk the
                                    // stack and attach every frame above the
                                    // target to a shared head.
                                    const frame_idx = self.memo_table[idx].next_pos_or_frame;
                                    try self.setupLrVm(&stack, sp, frame_idx, mc.rule_id);
                                    // Return the frame's current seed.
                                    const fr = stack[frame_idx].memo;
                                    if (fr.best_end != grow_sentinel) {
                                        // There is a real seed available.
                                        pos = fr.best_end;
                                        pc += 1;
                                    } else {
                                        // FAIL seed (pre-grow or grow hasn't advanced yet).
                                        if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                                        return null;
                                    }
                                },
                                .empty => {
                                    if (sp >= max_stack) return null;
                                    self.memo_table[idx] = .{
                                        .state = .lr,
                                        .next_pos_or_frame = @intCast(sp),
                                    };
                                    stack[sp] = .{ .memo = .{
                                        .rule_id = mc.rule_id,
                                        .is_recall = false,
                                        .start_pos = @intCast(pos),
                                        .return_pc = pc + 1,
                                        .rule_entry_pc = mc.offset,
                                        .best_end = grow_sentinel,
                                        .head_idx = no_head,
                                        .events_len_at_entry = if (config.capture_events)
                                            self.currentEventsLen()
                                        else {},
                                    } };
                                    sp += 1;
                                    pc = mc.offset;
                                },
                            }
                        }
                    },
                    .ret => {
                        // Find the matching call/memo frame, skipping
                        // any save / event frames the body pushed for
                        // live captures. Those frames stay on the
                        // stack so the outer caller's backtrack can
                        // still undo them; we just close the gap.
                        var ret_idx = sp;
                        while (ret_idx > 0) : (ret_idx -= 1) {
                            const tag = std.meta.activeTag(stack[ret_idx - 1]);
                            if (tag == .ret or tag == .memo) break;
                        }
                        std.debug.assert(ret_idx > 0);
                        ret_idx -= 1;
                        const popped = stack[ret_idx];
                        var k: usize = ret_idx;
                        while (k + 1 < sp) : (k += 1) stack[k] = stack[k + 1];
                        sp -= 1;
                        switch (popped) {
                            .ret => |addr| pc = addr,
                            .memo => |m| {
                                const stride = self.input.len + 1;
                                const idx = @as(usize, m.rule_id) * stride + m.start_pos;
                                const cur_end: u32 = @intCast(pos);
                                if (m.is_recall) {
                                    // RECALL re-eval completed. Only update
                                    // the memo if the answer strictly grew,
                                    // so that a late recall producing a
                                    // smaller match does not clobber an
                                    // earlier successful seed.
                                    const prev = self.memo_table[idx];
                                    const prev_end: u32 = if (prev.state == .success)
                                        prev.next_pos_or_frame
                                    else
                                        m.start_pos;
                                    const report_end = if (cur_end > prev_end) cur_end else prev_end;
                                    if (cur_end > prev_end) {
                                        try self.writeMemoSuccess(idx, cur_end, m);
                                    }
                                    pos = report_end;
                                    pc = m.return_pc;
                                } else if (m.best_end == grow_sentinel) {
                                    // First evaluation just completed.
                                    if (m.head_idx != no_head) {
                                        // LR detected during the descent. If
                                        // we are the rule that owns the head,
                                        // enter GROW-LR; otherwise return the
                                        // answer to the participant caller.
                                        if (self.heads_pool.items[m.head_idx].rule_id == m.rule_id) {
                                            try self.writeMemoSuccess(idx, cur_end, m);
                                            self.heads[m.start_pos] = m.head_idx;
                                            try self.resetEvalSet(m.head_idx);
                                            if (sp >= max_stack) return null;
                                            var frame = m;
                                            frame.best_end = cur_end;
                                            stack[sp] = .{ .memo = frame };
                                            sp += 1;
                                            pos = m.start_pos;
                                            pc = m.rule_entry_pc;
                                        } else {
                                            // Participant in someone else's
                                            // cycle: hand answer back up.
                                            try self.writeMemoSuccess(idx, cur_end, m);
                                            pc = m.return_pc;
                                        }
                                    } else {
                                        try self.writeMemoSuccess(idx, cur_end, m);
                                        pc = m.return_pc;
                                    }
                                } else {
                                    // A grow iteration just completed. During
                                    // the iteration, recall re-evals may have
                                    // written a better answer into memo_table
                                    // even if this iteration itself produced
                                    // a shorter match via an alternation
                                    // fallback. The memo entry holds the true
                                    // current best, so compare against that.
                                    const memo_end: u32 = if (self.memo_table[idx].state == .success)
                                        self.memo_table[idx].next_pos_or_frame
                                    else
                                        m.best_end;
                                    const new_best = if (cur_end > memo_end) cur_end else memo_end;
                                    if (new_best > m.best_end) {
                                        try self.writeMemoSuccess(idx, new_best, m);
                                        try self.resetEvalSet(m.head_idx);
                                        if (sp >= max_stack) return null;
                                        var frame = m;
                                        frame.best_end = new_best;
                                        stack[sp] = .{ .memo = frame };
                                        sp += 1;
                                        pos = m.start_pos;
                                        pc = m.rule_entry_pc;
                                    } else {
                                        // Done growing. Drop head from this pos.
                                        self.heads[m.start_pos] = no_head;
                                        pos = m.best_end;
                                        pc = m.return_pc;
                                    }
                                }
                            },
                            else => unreachable,
                        }
                    },
                    .save => {
                        if (sp >= max_stack) return null;
                        const slot = inst.data.slot;
                        if (config.capture_events) {
                            const state = if (self.events) |*s| s else @panic("capture_events enabled but no events state; use Self.initEvents");
                            const event_len = try events_mod.appendSave(state, slot, @intCast(pos));
                            stack[sp] = .{ .save = .{
                                .slot = slot,
                                .old = self.captures[slot],
                                .event_len = event_len,
                            } };
                        } else {
                            stack[sp] = .{ .save = .{ .slot = slot, .old = self.captures[slot] } };
                        }
                        sp += 1;
                        self.captures[slot] = pos;
                        pc += 1;
                    },
                    .event_open, .event_close => {
                        if (config.capture_events) {
                            if (sp >= max_stack) return null;
                            const state = if (self.events) |*s| s else @panic("capture_events enabled but no events state; use Self.initEvents");
                            const group_id = inst.data.slot;
                            const slot: u16 = if (inst.op == .event_open)
                                group_id << 1
                            else
                                (group_id << 1) | 1;
                            const event_len = try events_mod.appendSave(state, slot, @intCast(pos));
                            stack[sp] = .{ .event = .{ .event_len = event_len } };
                            sp += 1;
                        }
                        pc += 1;
                    },
                    .event_error_open => {
                        if (config.capture_events) {
                            if (sp >= max_stack) return null;
                            const state = if (self.events) |*s| s else @panic("capture_events enabled but no events state; use Self.initEvents");
                            const event_len = try events_mod.appendErrorOpen(state, inst.data.slot, @intCast(pos));
                            stack[sp] = .{ .event = .{ .event_len = event_len } };
                            sp += 1;
                        }
                        pc += 1;
                    },
                    .event_error_close => {
                        if (config.capture_events) {
                            if (sp >= max_stack) return null;
                            const state = if (self.events) |*s| s else @panic("capture_events enabled but no events state; use Self.initEvents");
                            const event_len = try events_mod.appendErrorClose(state, inst.data.slot, @intCast(pos));
                            stack[sp] = .{ .event = .{ .event_len = event_len } };
                            sp += 1;
                        }
                        pc += 1;
                    },
                    .event_missing => {
                        if (config.capture_events) {
                            if (sp >= max_stack) return null;
                            const state = if (self.events) |*s| s else @panic("capture_events enabled but no events state; use Self.initEvents");
                            const event_len = try events_mod.appendMissing(state, inst.data.slot, @intCast(pos));
                            stack[sp] = .{ .event = .{ .event_len = event_len } };
                            sp += 1;
                        }
                        pc += 1;
                    },
                    .event_token => {
                        // Emitted by the compiler immediately after a literal-
                        // matching opcode (`char` / `string`) under
                        // `token_events = .all|.tagged`. The literal's byte
                        // length is in `inst.data.byte`, so the token spans
                        // `[pos - len, pos)` after the literal succeeds.
                        if (config.capture_events) {
                            if (sp >= max_stack) return null;
                            const state = if (self.events) |*s| s else @panic("capture_events enabled but no events state; use Self.initEvents");
                            const len: u32 = inst.data.byte;
                            const start: u32 = @intCast(pos - len);
                            const event_len = try events_mod.appendToken(state, start, @intCast(pos));
                            stack[sp] = .{ .event = .{ .event_len = event_len } };
                            sp += 1;
                        }
                        pc += 1;
                    },
                    .event_field => {
                        // Stamp the field id onto the next-emitted open/token
                        // node. The id rides in `inst.data.slot`. Compiler
                        // emits this immediately before the call/literal it
                        // tags; backtracking truncates the marker along with
                        // the call/literal it precedes.
                        if (config.capture_events) {
                            if (sp >= max_stack) return null;
                            const state = if (self.events) |*s| s else @panic("capture_events enabled but no events state; use Self.initEvents");
                            const event_len = try events_mod.appendField(state, inst.data.slot, @intCast(pos));
                            stack[sp] = .{ .event = .{ .event_len = event_len } };
                            sp += 1;
                        }
                        pc += 1;
                    },
                    .lcatch => {
                        if (sp >= max_stack) return null;
                        const ch = inst.data.catch_handler;
                        if (config.capture_events) {
                            stack[sp] = .{ .lcatch = .{
                                .label = ch.label,
                                .handler_pc = ch.handler_pc,
                                .event_len = self.currentEventsLen(),
                            } };
                        } else {
                            stack[sp] = .{ .lcatch = .{
                                .label = ch.label,
                                .handler_pc = ch.handler_pc,
                            } };
                        }
                        sp += 1;
                        pc += 1;
                    },
                    .throw => {
                        if (try self.unwindThrow(&stack, &sp, &pc, &pos, inst.data.slot)) continue;
                        return null;
                    },
                    .match => {
                        return pos;
                    },
                }
            }
            return null;
        }

        /// Find the most recent `.choice` or `.lcatch` frame and splice
        /// it out of the stack, leaving any `.save` / `.event` / `.ret`
        /// frames pushed on top of it intact. Used by `commit` and
        /// `fail_twice` so a rule call inside an alt body (which leaves
        /// event frames on top after returning) doesn't cause us to
        /// pop the wrong frame and resume at the wrong control point.
        /// Caller guarantees a matching frame exists.
        fn spliceCtrlFrame(stack: *[max_stack]Entry, sp: *usize) void {
            var idx = sp.*;
            while (idx > 0) : (idx -= 1) {
                const tag = std.meta.activeTag(stack[idx - 1]);
                if (tag == .choice or tag == .lcatch) break;
            }
            std.debug.assert(idx > 0);
            idx -= 1;
            var k: usize = idx;
            while (k + 1 < sp.*) : (k += 1) stack[k] = stack[k + 1];
            sp.* -= 1;
        }

        fn backtrack(self: *Self, stack: *[max_stack]Entry, sp: *usize, pc: *u32, pos: *usize) bool {
            while (sp.* > 0) {
                sp.* -= 1;
                switch (stack[sp.*]) {
                    .choice => |c| {
                        if (self.trace) |t| {
                            t.writer.print("      backtrack -> pc={d} pos={d}\n", .{ c.pc, c.pos }) catch {};
                        }
                        self.recordBacktrack(c.pc, self.code[c.pc].op);
                        pc.* = c.pc;
                        pos.* = c.pos;
                        return true;
                    },
                    .ret => {},
                    .save => |s| {
                        self.captures[s.slot] = s.old;
                        if (config.capture_events) {
                            if (self.events) |*state| events_mod.truncate(state, s.event_len);
                        }
                    },
                    .event => |e| {
                        if (config.capture_events) {
                            if (self.events) |*state| events_mod.truncate(state, e.event_len);
                        }
                    },
                    .lcatch => |c| {
                        // Regular `fail` walks past lcatch frames - they
                        // only catch labeled failures. Truncate the
                        // events log to the catch's pre-push length so
                        // any events emitted inside the protected region
                        // are dropped (consistent with the rest of the
                        // backtrack rollback).
                        if (config.capture_events) {
                            if (self.events) |*state| events_mod.truncate(state, c.event_len);
                        }
                    },
                    .memo => |m| {
                        if (m.is_recall) {
                            // RECALL re-eval failed. Do NOT clobber a prior
                            // successful seed: if this re-eval produced no
                            // better match, the caller should see the seed
                            // value still cached in memo_table. Convert our
                            // failure into the cached success via a
                            // redirect, the same way grow-failure does.
                            const stride = self.input.len + 1;
                            const idx = @as(usize, m.rule_id) * stride + m.start_pos;
                            const prev = self.memo_table[idx];
                            if (prev.state == .success) {
                                pc.* = m.return_pc;
                                pos.* = prev.next_pos_or_frame;
                                return true;
                            }
                            self.memo_table[idx] = .{ .state = .fail, .next_pos_or_frame = 0 };
                        } else if (m.best_end != grow_sentinel) {
                            // Grow iteration failed. Stop growing and resume
                            // at the return site with the best seed.
                            self.heads[m.start_pos] = no_head;
                            pc.* = m.return_pc;
                            pos.* = m.best_end;
                            return true;
                        } else {
                            // First evaluation failed.
                            const stride = self.input.len + 1;
                            const idx = @as(usize, m.rule_id) * stride + m.start_pos;
                            self.memo_table[idx] = .{ .state = .fail, .next_pos_or_frame = 0 };
                        }
                    },
                }
            }
            return false;
        }

        /// Sentinel label: an `lcatch` frame with `label == wildcard_label`
        /// catches throws of any label.
        pub const wildcard_label: u16 = std.math.maxInt(u16);

        /// Labeled-failure unwind. Walks the stack downward, skipping
        /// every frame except a matching `.lcatch`. Critically does NOT
        /// roll back captures or events on `.save`/`.event`/`.memo`
        /// frames - the throw policy preserves them, and the matching
        /// `.lcatch` synthesizes `partial_close` events for any opens
        /// left dangling above it. Returns true on a successful catch
        /// (control transferred to the handler), false if no matching
        /// catch was found (caller treats as a hard fail).
        fn unwindThrow(
            self: *Self,
            stack: *[max_stack]Entry,
            sp: *usize,
            pc: *u32,
            pos: *usize,
            label: u16,
        ) !bool {
            while (sp.* > 0) {
                sp.* -= 1;
                switch (stack[sp.*]) {
                    .lcatch => |c| {
                        const matches = c.label == label or c.label == wildcard_label;
                        if (!matches) continue;
                        if (config.capture_events) {
                            if (self.events) |*state| {
                                try synthesizePartialCloses(state, c.event_len, @intCast(pos.*));
                            }
                        }
                        if (self.trace) |t| {
                            t.writer.print("      throw caught -> pc={d} pos={d}\n", .{ c.handler_pc, pos.* }) catch {};
                        }
                        pc.* = c.handler_pc;
                        // pos stays at the throw site, by design.
                        return true;
                    },
                    // Throw unwinds *past* every other frame without
                    // rollback. Captures and events are preserved.
                    else => continue,
                }
            }
            return false;
        }

        /// Walk `events[catch_event_len..]` to find any `.open`s that
        /// were not paired with a `.close` (or `.partial_close`) before
        /// the throw site. For each such still-open frame, append a
        /// `.partial_close` event at `throw_pos`, innermost-first, so
        /// the captured-tree builder sees a balanced log with the
        /// dangling rules tagged `.rule_partial`.
        fn synthesizePartialCloses(
            state: *events_mod.State,
            catch_event_len: u32,
            throw_pos: u32,
        ) !void {
            var open_stack: [max_stack]u16 = undefined;
            var open_sp: usize = 0;

            const live = state.list.items[catch_event_len..];
            for (live) |ev| switch (ev) {
                .open => |m| {
                    // Capture nesting is bounded by call-stack depth, so
                    // overflow here means we miscompiled the grammar.
                    std.debug.assert(open_sp < max_stack);
                    open_stack[open_sp] = m.group_id;
                    open_sp += 1;
                },
                .close => |c| {
                    // The live event log is balanced up to the throw site,
                    // so a mismatch here is an invariant violation, not user error.
                    std.debug.assert(open_sp > 0 and open_stack[open_sp - 1] == c.group_id);
                    open_sp -= 1;
                },
                .partial_close => |c| {
                    std.debug.assert(open_sp > 0 and open_stack[open_sp - 1] == c.group_id);
                    open_sp -= 1;
                },
                // Diagnostic-only events: error_open/error_close are emitted by
                // recovery handlers, which themselves can't throw, so they never
                // appear inside an unwind window. `missing`, `.token`, and
                // `.field_marker` are purely informational, with no nesting
                // effect on the open stack.
                .error_open, .error_close, .missing, .token, .field_marker => {},
            };

            while (open_sp > 0) {
                open_sp -= 1;
                _ = try events_mod.appendPartialClose(state, open_stack[open_sp], throw_pos);
            }
        }

        /// Return the span for capture group `i`, or null if not captured.
        pub fn getCapture(self: *const Self, i: u16) ?Span {
            const start = self.captures[i * 2] orelse return null;
            const end = self.captures[i * 2 + 1] orelse return null;
            return .{ .start = start, .end = end };
        }

        /// Return the matched slice for capture group `i`, or null if not captured.
        pub fn getCaptureSlice(self: *const Self, i: u16) ?[]const u8 {
            const span = self.getCapture(i) orelse return null;
            return self.input[span.start..span.end];
        }

        /// Build a capture tree from the recorded open/close events of the
        /// last `execute()` call. Only available when `config.capture_events`
        /// is true. The returned `Tree` owns its nodes via `tree_allocator`
        /// (which may differ from the VM's events allocator); call
        /// `tree.deinit()` to release them.
        pub const buildCaptureTree = if (config.capture_events)
            struct {
                fn f(self: *const Self, tree_allocator: std.mem.Allocator) CaptureTree.BuildError!CaptureTree.Tree {
                    const evs = if (self.events) |*s| s.items() else &.{};
                    return CaptureTree.buildFromEvents(tree_allocator, evs);
                }
            }.f
        else {};

        /// Raw view of recorded capture events. Valid until the next
        /// `execute()` call or `deinit()`. Only available when
        /// `config.capture_events` is true.
        pub const getCaptureEvents = if (config.capture_events)
            struct {
                fn f(self: *const Self) []const CaptureTree.Event {
                    return if (self.events) |*s| s.items() else &.{};
                }
            }.f
        else {};

        /// Warth's SETUP-LR. Ensure the memo frame at `frame_idx` has a
        /// head, then walk the stack downward from `sp_top` until we find a
        /// frame already pointing at that head; every memo frame we cross
        /// joins the head's involved set.
        fn setupLrVm(
            self: *Self,
            stack: *[max_stack]Entry,
            sp_top: usize,
            frame_idx: u32,
            recur_rule_id: u16,
        ) !void {
            const allocator = self.memo_allocator.?;
            const num_rules = self.memo_rule_count;

            // Create a head if the target frame does not have one yet.
            var target_head_idx = stack[frame_idx].memo.head_idx;
            if (target_head_idx == no_head) {
                var involved = try std.DynamicBitSetUnmanaged.initEmpty(allocator, num_rules);
                const eval = try std.DynamicBitSetUnmanaged.initEmpty(allocator, num_rules);
                involved.set(stack[frame_idx].memo.rule_id);
                try self.heads_pool.append(allocator, .{
                    .rule_id = stack[frame_idx].memo.rule_id,
                    .involved = involved,
                    .eval = eval,
                });
                target_head_idx = @intCast(self.heads_pool.items.len - 1);
                stack[frame_idx].memo.head_idx = target_head_idx;
            }

            // Also add the recurring rule itself to the involved set; it may
            // not yet be represented by a frame (e.g. direct self-recursion).
            self.heads_pool.items[target_head_idx].involved.set(recur_rule_id);

            // Walk stack from top down, tagging memo frames with this head.
            var i: usize = sp_top;
            while (i > 0) {
                i -= 1;
                if (stack[i] != .memo) continue;
                const fr = &stack[i].memo;
                if (fr.head_idx == target_head_idx) break;
                fr.head_idx = target_head_idx;
                self.heads_pool.items[target_head_idx].involved.set(fr.rule_id);
                if (i == frame_idx) break;
            }
        }

        /// Set eval_set = involved_set at the start of each grow iteration.
        fn resetEvalSet(self: *Self, head_idx: u32) !void {
            const h = &self.heads_pool.items[head_idx];
            h.eval.setRangeValue(.{ .start = 0, .end = h.eval.bit_length }, false);
            var it = h.involved.iterator(.{});
            while (it.next()) |bit| h.eval.set(bit);
        }

        fn traceStep(self: *Self, pc: u32, pos: usize, sp: usize, inst: I.Inst) void {
            const t = self.trace orelse return;
            const w = t.writer;
            // pc, stack depth, position, remaining input preview
            w.print("{d:>4}: sp={d:<3} pos={d:<3} ", .{ pc, sp, pos }) catch return;
            // input context: show up to 16 bytes from current position
            w.writeByte('"') catch return;
            const remaining = self.input[pos..];
            const preview = remaining[0..@min(remaining.len, 16)];
            for (preview) |b| {
                if (b >= 0x20 and b < 0x7F)
                    w.writeByte(b) catch return
                else
                    w.print("\\x{x:0>2}", .{b}) catch return;
            }
            if (remaining.len > 16) w.writeAll("...") catch {};
            w.writeAll("\" ") catch return;
            // opcode
            switch (inst.op) {
                .char => {
                    const b = inst.data.byte;
                    if (b >= 0x20 and b < 0x7F)
                        w.print("char '{c}'", .{b}) catch {}
                    else
                        w.print("char 0x{x:0>2}", .{b}) catch {};
                },
                .string => {
                    const ref = inst.data.string;
                    const str = self.string_data[ref.offset..][0..ref.len];
                    w.print("string \"{s}\"", .{str}) catch {};
                },
                .any => w.writeAll("any") catch {},
                .set => w.print("set [#{d}]", .{inst.data.charset}) catch {},
                .neg_set => w.print("neg_set [#{d}]", .{inst.data.charset}) catch {},
                .optional_char => {
                    const b = inst.data.byte;
                    if (b >= 0x20 and b < 0x7F)
                        w.print("opt_char '{c}'", .{b}) catch {}
                    else
                        w.print("opt_char 0x{x:0>2}", .{b}) catch {};
                },
                .choice => w.print("choice -> {d}", .{inst.data.offset}) catch {},
                .commit => w.print("commit -> {d}", .{inst.data.offset}) catch {},
                .fail => w.writeAll("fail") catch {},
                .fail_twice => w.writeAll("fail_twice") catch {},
                .jump => w.print("jump -> {d}", .{inst.data.offset}) catch {},
                .call => w.print("call -> {d}", .{inst.data.offset}) catch {},
                .memo_call => w.print("memo_call R{d} -> {d}", .{ inst.data.memo.rule_id, inst.data.memo.offset }) catch {},
                .ret => w.writeAll("ret") catch {},
                .save => w.print("save {d}", .{inst.data.slot}) catch {},
                .event_open => w.print("event_open g{d}", .{inst.data.slot}) catch {},
                .event_close => w.print("event_close g{d}", .{inst.data.slot}) catch {},
                .event_error_open => w.print("event_error_open L{d}", .{inst.data.slot}) catch {},
                .event_error_close => w.print("event_error_close L{d}", .{inst.data.slot}) catch {},
                .event_missing => w.print("event_missing L{d}", .{inst.data.slot}) catch {},
                .event_token => w.print("event_token len={d}", .{inst.data.byte}) catch {},
                .event_field => w.print("event_field f{d}", .{inst.data.slot}) catch {},
                .throw => w.print("throw L{d}", .{inst.data.slot}) catch {},
                .lcatch => w.print("lcatch L{d} -> {d}", .{ inst.data.catch_handler.label, inst.data.catch_handler.handler_pc }) catch {},
                .match => w.writeAll("match") catch {},
            }
            w.writeByte('\n') catch {};
        }
    };
}

const testing = std.testing;
const Compiler = @import("Compiler.zig").Compiler;
const EreScanner = @import("../ere/Scanner.zig").Scanner;
const EreParser = @import("../ere/Parser.zig").Parser;
const PegScanner = @import("../peg/Scanner.zig").Scanner;
const PegParser = @import("../peg/Parser.zig").Parser;

fn compileEre(source: []const u8) !Compiler {
    var scanner = EreScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, source);
    const rules = try parser.parse();
    return Compiler.compile(rules);
}

fn compilePeg(source: []const u8) !Compiler {
    var scanner = PegScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, source);
    const rules = try parser.parse();
    return Compiler.compile(rules);
}

fn expectMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = try compileEre(source);
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), input);
    const result = try vm.execute();
    try testing.expectEqual(expected, result);
}

fn expectPegMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = try compilePeg(source);
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), input);
    const result = try vm.execute();
    try testing.expectEqual(expected, result);
}

test "literal match" {
    try expectMatch("abc", "abc", 3);
    try expectMatch("abc", "abx", null);
    try expectMatch("abc", "ab", null);
}

test "alternation" {
    try expectMatch("a|b", "a", 1);
    try expectMatch("a|b", "b", 1);
    try expectMatch("a|b", "c", null);
}

test "star repetition" {
    try expectMatch("a*", "", 0);
    try expectMatch("a*", "aaa", 3);
    try expectMatch("a*b", "aaab", 4);
    try expectMatch("a*b", "b", 1);
}

test "plus repetition" {
    try expectMatch("a+", "", null);
    try expectMatch("a+", "aaa", 3);
}

test "optional" {
    try expectMatch("a?b", "ab", 2);
    try expectMatch("a?b", "b", 1);
}

test "character class" {
    try expectMatch("[a-z]+", "hello", 5);
    try expectMatch("[a-z]+", "HELLO", null);
    try expectMatch("[0-9]+", "42", 2);
}

test "negated character class" {
    try expectMatch("[^0-9]+", "abc", 3);
    try expectMatch("[^0-9]+", "123", null);
}

test "dot wildcard" {
    try expectMatch("a.c", "abc", 3);
    try expectMatch("a.c", "aXc", 3);
    try expectMatch("a.c", "ac", null);
}

test "grouped alternation" {
    try expectMatch("(ab|cd)e", "abe", 3);
    try expectMatch("(ab|cd)e", "cde", 3);
    try expectMatch("(ab|cd)e", "ace", null);
}

test "interval repetition" {
    try expectMatch("a{2,4}", "a", null);
    try expectMatch("a{2,4}", "aa", 2);
    try expectMatch("a{2,4}", "aaa", 3);
    try expectMatch("a{2,4}", "aaaa", 4);
    try expectMatch("a{2,4}", "aaaaa", 4);
}

test "alternation with common prefix" {
    try expectMatch("https|http", "https", 5);
    try expectMatch("https|http", "http", 4);
    try expectMatch("https|http", "httq", null);
    try expectMatch("httpAB|httpCD", "httpAB", 6);
    try expectMatch("httpAB|httpCD", "httpCD", 6);
    try expectMatch("httpAB|httpCD", "httpXX", null);
    try expectMatch("httpAB|httpCD", "http", null);
    try expectMatch("ab|a", "ab", 2);
    try expectMatch("ab|a", "a", 1);
    try expectMatch("ab|a", "x", null);
}

test "peg: single rule" {
    try expectPegMatch("Main <- \"hello\"", "hello", 5);
    try expectPegMatch("Main <- \"hello\"", "world", null);
}

test "peg: rule references" {
    try expectPegMatch(
        \\Main  <- Greeting " " Name
        \\Greeting <- "hi" / "hello"
        \\Name <- [a-z]+
    , "hi world", 8);
    try expectPegMatch(
        \\Main  <- Greeting " " Name
        \\Greeting <- "hi" / "hello"
        \\Name <- [a-z]+
    , "hello world", 11);
}

test "peg: recursive rules" {
    try expectPegMatch(
        \\Expr   <- Term ("+" Term)*
        \\Term   <- Factor ("*" Factor)*
        \\Factor <- "(" Expr ")" / [0-9]+
    , "1+2*3", 5);
    try expectPegMatch(
        \\Expr   <- Term ("+" Term)*
        \\Term   <- Factor ("*" Factor)*
        \\Factor <- "(" Expr ")" / [0-9]+
    , "(1+2)*3", 7);
}

test "peg: not predicate" {
    try expectPegMatch(
        \\Line <- (!"\n" .)*
    , "hello world", 11);
    try expectPegMatch(
        \\Line <- (!"\n" .)* "\n"
    , "hello\n", 6);
}

test "capture: single group" {
    var compiler = try compileEre("a(bc)d");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "abcd");
    try testing.expectEqual(@as(?usize, 4), try vm.execute());
    try testing.expectEqualStrings("bc", vm.getCaptureSlice(0).?);
}

test "capture: multiple groups" {
    var compiler = try compileEre("(a+)(b+)");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "aaabb");
    try testing.expectEqual(@as(?usize, 5), try vm.execute());
    try testing.expectEqualStrings("aaa", vm.getCaptureSlice(0).?);
    try testing.expectEqualStrings("bb", vm.getCaptureSlice(1).?);
}

test "capture: alternation picks correct branch" {
    var compiler = try compileEre("(ab)|(cd)");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "cd");
    try testing.expectEqual(@as(?usize, 2), try vm.execute());
    // First group did not match.
    try testing.expectEqual(@as(?Vm.Span, null), vm.getCapture(0));
    // Second group matched.
    try testing.expectEqualStrings("cd", vm.getCaptureSlice(1).?);
}

test "capture: nested groups" {
    var compiler = try compileEre("((a)(b))");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "ab");
    try testing.expectEqual(@as(?usize, 2), try vm.execute());
    try testing.expectEqualStrings("ab", vm.getCaptureSlice(0).?);
    try testing.expectEqualStrings("a", vm.getCaptureSlice(1).?);
    try testing.expectEqualStrings("b", vm.getCaptureSlice(2).?);
}

test "capture: group with repetition" {
    var compiler = try compileEre("(a+)b");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "aaab");
    try testing.expectEqual(@as(?usize, 4), try vm.execute());
    try testing.expectEqualStrings("aaa", vm.getCaptureSlice(0).?);
}

test "capture: repeated capture is one group (POSIX)" {
    // `(a)+` must be a single group whose flat slot holds the last
    // match, not one fresh group per bytecode-level iteration.
    var compiler = try compileEre("(a)+");
    try testing.expectEqual(@as(u16, 1), compiler.getCaptureCount());
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "aaa");
    try testing.expectEqual(@as(?usize, 3), try vm.execute());
    try testing.expectEqualStrings("a", vm.getCaptureSlice(0).?);
    try testing.expectEqual(@as(?Vm.Span, null), vm.getCapture(1));
}

test "capture: bounded repetition of capture is one group" {
    var compiler = try compileEre("(ab){2,3}");
    try testing.expectEqual(@as(u16, 1), compiler.getCaptureCount());
}

test "capture: no match clears captures" {
    var compiler = try compileEre("(a)b");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "ac");
    try testing.expectEqual(@as(?usize, null), try vm.execute());
    // Capture should be null after failed match (undone by backtrack).
    try testing.expectEqual(@as(?Vm.Span, null), vm.getCapture(0));
}

fn compilePegOpts(source: []const u8, opts: Compiler.Options) !Compiler {
    var scanner = PegScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, source);
    const rules = try parser.parse();
    return Compiler.compileOpts(rules, opts);
}

test "packrat: same result as non-packrat on simple grammar" {
    const src =
        \\Main <- Greet " " Name
        \\Greet <- "hi" / "hello"
        \\Name  <- [a-z]+
    ;
    const input = "hello world";

    var plain = try compilePegOpts(src, .{ .memoize = false });
    var vm_plain = Vm.init(plain.getCode(), plain.getCharsets(), plain.getStringData(), input);
    const r_plain = try vm_plain.execute();

    var memo = try compilePegOpts(src, .{ .memoize = true });
    var vm_memo = try Vm.initPackrat(
        testing.allocator,
        memo.getCode(),
        memo.getCharsets(),
        memo.getStringData(),
        memo.getMemoRuleCount(),
        input,
    );
    defer vm_memo.deinit();
    const r_memo = try vm_memo.execute();

    try testing.expectEqual(r_plain, r_memo);
    try testing.expectEqual(@as(?usize, 11), r_memo);
}

test "packrat: rewrites rule callsites into memo_call" {
    // Every rule here is capture-free, so every call should become a memo_call.
    const src =
        \\Main <- A B
        \\A    <- "a"
        \\B    <- "b"
    ;
    var c = try compilePegOpts(src, .{ .memoize = true });
    try testing.expectEqual(@as(u16, 3), c.getMemoRuleCount());

    var call_count: usize = 0;
    var memo_count: usize = 0;
    for (c.getCode()) |inst| {
        if (inst.op == .call) call_count += 1;
        if (inst.op == .memo_call) memo_count += 1;
    }
    try testing.expectEqual(@as(usize, 0), call_count);
    try testing.expectEqual(@as(usize, 3), memo_count);
}

test "packrat: redundant rule re-entry is cached" {
    // S tries the first alternative; E matches but "!" fails, so S
    // backtracks and tries the second alternative. Without packrat,
    // E is re-executed from pos 0; with packrat, the second call is
    // a table hit.
    const src =
        \\S <- E "!" / E "?"
        \\E <- "a" "b" "c" "d"
    ;
    const input = "abcd?";

    var plain = try compilePegOpts(src, .{ .memoize = false });
    var vm_plain = Vm.init(plain.getCode(), plain.getCharsets(), plain.getStringData(), input);
    const r_plain = try vm_plain.execute();

    var memo = try compilePegOpts(src, .{ .memoize = true });
    var vm_memo = try Vm.initPackrat(
        testing.allocator,
        memo.getCode(),
        memo.getCharsets(),
        memo.getStringData(),
        memo.getMemoRuleCount(),
        input,
    );
    defer vm_memo.deinit();
    const r_memo = try vm_memo.execute();

    try testing.expectEqual(@as(?usize, 5), r_plain);
    try testing.expectEqual(r_plain, r_memo);
    // Packrat must execute strictly fewer instructions: the second
    // call to E returns via a memo hit instead of running the body.
    try testing.expect(vm_memo.steps < vm_plain.steps);
}

test "packrat: failure memoization" {
    // Same idea but E fails - the failure must be cached too, so the
    // second call to E returns fail without re-running the body.
    const src =
        \\S <- E "x" / E "y"
        \\E <- "a" "b" "c"
    ;
    const input = "abqy";

    var plain = try compilePegOpts(src, .{ .memoize = false });
    var vm_plain = Vm.init(plain.getCode(), plain.getCharsets(), plain.getStringData(), input);
    const r_plain = try vm_plain.execute();

    var memo = try compilePegOpts(src, .{ .memoize = true });
    var vm_memo = try Vm.initPackrat(
        testing.allocator,
        memo.getCode(),
        memo.getCharsets(),
        memo.getStringData(),
        memo.getMemoRuleCount(),
        input,
    );
    defer vm_memo.deinit();
    const r_memo = try vm_memo.execute();

    try testing.expectEqual(@as(?usize, null), r_plain);
    try testing.expectEqual(r_plain, r_memo);
    try testing.expect(vm_memo.steps < vm_plain.steps);
}

fn runPackrat(src: []const u8, input: []const u8) !?usize {
    var c = try compilePegOpts(src, .{ .memoize = true });
    var vm = try Vm.initPackrat(
        testing.allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        c.getMemoRuleCount(),
        input,
    );
    defer vm.deinit();
    return try vm.execute();
}

test "warth: direct left recursion (single digit seed)" {
    const src =
        \\Expr <- Expr "+" Num / Num
        \\Num  <- [0-9]+
    ;
    try testing.expectEqual(@as(?usize, 1), try runPackrat(src, "1"));
}

test "warth: direct left recursion grows across input" {
    const src =
        \\Expr <- Expr "+" Num / Num
        \\Num  <- [0-9]+
    ;
    try testing.expectEqual(@as(?usize, 5), try runPackrat(src, "1+2+3"));
}

test "warth: direct left recursion stops at non-matching suffix" {
    const src =
        \\Expr <- Expr "+" Num / Num
        \\Num  <- [0-9]+
    ;
    try testing.expectEqual(@as(?usize, 3), try runPackrat(src, "1+2+x"));
}

test "warth: left recursion with two operators" {
    const src =
        \\Expr <- Expr "+" Num / Expr "-" Num / Num
        \\Num  <- [0-9]+
    ;
    try testing.expectEqual(@as(?usize, 7), try runPackrat(src, "1+2-3+4"));
}

test "warth: left recursion that never matches returns null" {
    const src =
        \\Expr <- Expr "+" Num / Num
        \\Num  <- [0-9]+
    ;
    try testing.expectEqual(@as(?usize, null), try runPackrat(src, "x"));
}

test "warth: indirect left recursion through two rules" {
    const src =
        \\A <- B
        \\B <- A "x" / "y"
    ;
    try testing.expectEqual(@as(?usize, 3), try runPackrat(src, "yxx"));
}

test "warth: participant memo is not stale after grow" {
    // After A grows via indirect LR (A -> B -> A "x" / "y"), memo[B, 0]
    // must reflect the grown match, not a stale shorter re-eval. If
    // top backtracks and queries B at position 0, the answer should
    // be the full "yxx".
    const src =
        \\Top <- (A "qqqq") / B
        \\A   <- B
        \\B   <- (A "x") / "y"
    ;
    try testing.expectEqual(@as(?usize, 3), try runPackrat(src, "yxx"));
}

test "warth: indirect LR with three rules in cycle" {
    const src =
        \\A <- B
        \\B <- C
        \\C <- A "x" / "y"
    ;
    try testing.expectEqual(@as(?usize, 4), try runPackrat(src, "yxxx"));
}

test "rules_as_captures: PEG grammar produces a tree mirroring the call hierarchy" {
    // The smallest end-to-end check that the JIT capture_events +
    // packrat replay infrastructure actually produces a parse tree
    // through a real grammar. Each rule emits open/close events keyed
    // by rule_id, and the tree builder already turns those into
    // hierarchical nodes - no grammar syntax change required.
    const src =
        \\Expr <- Term ("+" Term)*
        \\Term <- [0-9]+
    ;
    var c = try compilePegOpts(src, .{ .rules_as_captures = true });
    const Cfg = VmWith(.{ .capture_events = true });
    var vm = Cfg.initEvents(
        testing.allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        "1+2+3",
    );
    defer vm.deinit();
    try testing.expectEqual(@as(?usize, 5), try vm.execute());

    var tree = try vm.buildCaptureTree(testing.allocator);
    defer tree.deinit();

    try testing.expectEqual(@as(usize, 1), tree.roots.len);
    const expr = tree.roots[0];
    try testing.expectEqualStrings("Expr", c.getRuleName(expr.group_id));
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 0, .end = 5 }, expr.span);
    try testing.expectEqual(@as(usize, 3), expr.children.len);
    for (expr.children) |term| {
        try testing.expectEqualStrings("Term", c.getRuleName(term.group_id));
    }
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 0, .end = 1 }, expr.children[0].span);
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 2, .end = 3 }, expr.children[1].span);
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 4, .end = 5 }, expr.children[2].span);
}

test "packrat + captures: memoized rule replays events on cache hit" {
    // No grammar format in the codebase produces multi-rule programs
    // with captures (PEG has no capture syntax, ERE is single-rule),
    // so we hand-craft bytecode for the equivalent of:
    //   Main <- A 'x' / A 'y'
    //   A    <- '(' '$0' 'a' '$1' ')'    (where $N are open/close saves)
    //
    // Input "(a)y" forces a backtrack-then-retry on A: the first call
    // misses, runs the body, and gets backtracked; the second call
    // must hit the cache and replay the events plus capture writes.
    const code = [_]I.Inst{
        .{ .op = .choice, .data = .{ .offset = 5 } }, // 0: try alt1
        .{ .op = .memo_call, .data = .{ .memo = .{ .rule_id = 0, .offset = 8 } } }, // 1: A
        .{ .op = .char, .data = .{ .byte = 'x' } }, // 2
        .{ .op = .commit, .data = .{ .offset = 7 } }, // 3: jump to match
        .{ .op = .fail }, // 4: unreachable padding
        .{ .op = .memo_call, .data = .{ .memo = .{ .rule_id = 0, .offset = 8 } } }, // 5: alt2: A
        .{ .op = .char, .data = .{ .byte = 'y' } }, // 6
        .{ .op = .match }, // 7
        .{ .op = .char, .data = .{ .byte = '(' } }, // 8: A body
        .{ .op = .save, .data = .{ .slot = 0 } }, // 9: open group 0
        .{ .op = .char, .data = .{ .byte = 'a' } }, // 10
        .{ .op = .save, .data = .{ .slot = 1 } }, // 11: close group 0
        .{ .op = .char, .data = .{ .byte = ')' } }, // 12
        .{ .op = .ret }, // 13
    };

    const Cfg = VmWith(.{ .capture_events = true });
    var vm = try Cfg.initPackrat(testing.allocator, &code, &.{}, "", 1, "(a)y");
    defer vm.deinit();
    try testing.expectEqual(@as(?usize, 4), try vm.execute());

    // Capture slots reflect the hit-replayed writes from alt2.
    try testing.expectEqualStrings("a", vm.getCaptureSlice(0).?);

    // Live events log holds exactly the surviving alt's events: the
    // miss-path events were truncated on backtrack, then re-appended
    // by the cache-hit replay before alt2's 'y' matched.
    const evs = vm.getCaptureEvents();
    try testing.expectEqual(@as(usize, 2), evs.len);
    try testing.expectEqual(CaptureTreeMod.Event{ .open = .{ .group_id = 0, .pos = 1 } }, evs[0]);
    try testing.expectEqual(CaptureTreeMod.Event{ .close = .{ .group_id = 0, .pos = 2 } }, evs[1]);
}

const EventVm = VmWith(.{ .capture_events = true });
const CaptureTreeMod = @import("CaptureTree.zig");

// Recovery tests live in their own file (programmatic-AST end-to-end
// scenarios for labeled failures, partial_close synthesis, ERROR /
// MISSING node mapping).
comptime {
    _ = @import("recovery_test.zig");
}

test "capture events: flat single group" {
    var compiler = try compileEre("a(bc)d");
    var vm = EventVm.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "abcd",
    );
    defer vm.deinit();
    try testing.expectEqual(@as(?usize, 4), try vm.execute());

    // Flat API still works alongside events.
    try testing.expectEqualStrings("bc", vm.getCaptureSlice(0).?);

    const events = vm.getCaptureEvents();
    try testing.expectEqual(@as(usize, 2), events.len);
    try testing.expectEqual(CaptureTreeMod.Event{ .open = .{ .group_id = 0, .pos = 1 } }, events[0]);
    try testing.expectEqual(CaptureTreeMod.Event{ .close = .{ .group_id = 0, .pos = 3 } }, events[1]);
}

test "capture events: tree with nested groups" {
    var compiler = try compileEre("((a)(b))");
    var vm = EventVm.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "ab",
    );
    defer vm.deinit();
    try testing.expectEqual(@as(?usize, 2), try vm.execute());

    var tree = try vm.buildCaptureTree(testing.allocator);
    defer tree.deinit();

    try testing.expectEqual(@as(usize, 1), tree.roots.len);
    const outer = tree.roots[0];
    try testing.expectEqual(@as(u16, 0), outer.group_id);
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 0, .end = 2 }, outer.span);
    try testing.expectEqual(@as(usize, 2), outer.children.len);
    try testing.expectEqual(@as(u16, 1), outer.children[0].group_id);
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 0, .end = 1 }, outer.children[0].span);
    try testing.expectEqual(@as(u16, 2), outer.children[1].group_id);
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 1, .end = 2 }, outer.children[1].span);
}

test "capture events: repetition yields sibling nodes" {
    // Non-obvious: the flat-slot API only retains the LAST (a) match, but
    // the event stream preserves every successful iteration so the tree
    // shows all three.
    var compiler = try compileEre("(a)(a)(a)");
    var vm = EventVm.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "aaa",
    );
    defer vm.deinit();
    try testing.expectEqual(@as(?usize, 3), try vm.execute());

    var tree = try vm.buildCaptureTree(testing.allocator);
    defer tree.deinit();

    try testing.expectEqual(@as(usize, 3), tree.roots.len);
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 0, .end = 1 }, tree.roots[0].span);
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 1, .end = 2 }, tree.roots[1].span);
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 2, .end = 3 }, tree.roots[2].span);
}

test "capture events: same group repeated via +" {
    var compiler = try compileEre("(a)+");
    var vm = EventVm.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "aaa",
    );
    defer vm.deinit();
    try testing.expectEqual(@as(?usize, 3), try vm.execute());

    var tree = try vm.buildCaptureTree(testing.allocator);
    defer tree.deinit();

    // Each iteration leaves its own sibling event pair, all with group_id 0.
    try testing.expectEqual(@as(usize, 3), tree.roots.len);
    for (tree.roots) |n| try testing.expectEqual(@as(u16, 0), n.group_id);
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 0, .end = 1 }, tree.roots[0].span);
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 1, .end = 2 }, tree.roots[1].span);
    try testing.expectEqual(CaptureTreeMod.Span{ .start = 2, .end = 3 }, tree.roots[2].span);
}

test "capture events: backtrack discards failed-branch events" {
    // First alternative matches (a)(b), fails on "c", backtracks; second
    // alternative matches (x)(y). Events from the discarded branch must
    // be truncated from the log.
    var compiler = try compileEre("(a)(b)c|(x)(y)");
    var vm = EventVm.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "xy",
    );
    defer vm.deinit();
    try testing.expectEqual(@as(?usize, 2), try vm.execute());

    const events = vm.getCaptureEvents();
    // Only the two surviving captures (groups 2 and 3, since 0/1 were
    // compiled into the failing branch) should appear in the log.
    try testing.expectEqual(@as(usize, 4), events.len);
    try testing.expectEqual(CaptureTreeMod.Event{ .open = .{ .group_id = 2, .pos = 0 } }, events[0]);
    try testing.expectEqual(CaptureTreeMod.Event{ .close = .{ .group_id = 2, .pos = 1 } }, events[1]);
    try testing.expectEqual(CaptureTreeMod.Event{ .open = .{ .group_id = 3, .pos = 1 } }, events[2]);
    try testing.expectEqual(CaptureTreeMod.Event{ .close = .{ .group_id = 3, .pos = 2 } }, events[3]);
}

test "capture events: failed match produces empty log" {
    var compiler = try compileEre("(a)b");
    var vm = EventVm.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "ac",
    );
    defer vm.deinit();
    try testing.expectEqual(@as(?usize, null), try vm.execute());
    try testing.expectEqual(@as(usize, 0), vm.getCaptureEvents().len);
}

test "capture events: cleared between runs" {
    var compiler = try compileEre("(a)");
    var vm = EventVm.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "a",
    );
    defer vm.deinit();
    _ = try vm.execute();
    try testing.expectEqual(@as(usize, 2), vm.getCaptureEvents().len);
    // Re-run on same input; events should not accumulate.
    _ = try vm.execute();
    try testing.expectEqual(@as(usize, 2), vm.getCaptureEvents().len);
}

const StatsVm = VmWith(.{ .enable_stats = true });
const StatsCompiler = @import("Compiler.zig").CompilerWith(.{});

test "stats: per-instruction and per-opcode counts" {
    // "a|b" matching "b" must try "a" (fail), backtrack, then match "b".
    var compiler = try compileEre("a|b");
    const code = compiler.getCode();
    var vm = StatsVm.init(code, compiler.getCharsets(), compiler.getStringData(), "b");
    const result = try vm.execute();
    try testing.expectEqual(@as(?usize, 1), result);

    const stats = vm.getStats();
    // At least one instruction executed.
    try testing.expect(stats.steps > 0);
    // The choice opcode must have been executed at least once.
    try testing.expect(stats.opcode_exec_counts[@intFromEnum(I.Opcode.choice)] >= 1);
    // Since "a" fails and backtracks to "b", there must be at least one backtrack.
    try testing.expect(stats.opcode_backtrack_counts[@intFromEnum(I.Opcode.char)] >= 1);

    // Per-instruction stats are available.
    const inst_stats = stats.inst_stats.?;
    try testing.expectEqual(code.len, inst_stats.len);
    // Every instruction that executed should have a non-zero exec_count.
    var total_exec: u64 = 0;
    for (inst_stats) |s| total_exec += s.exec_count;
    try testing.expectEqual(stats.steps, total_exec);
}

test "stats: backtrack counts on choice points" {
    // "(ab|cd)e" matching "cde": first alternative "ab" fails at 'a',
    // backtracks to try "cd", which succeeds.
    var compiler = try compileEre("(ab|cd)e");
    var vm = StatsVm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "cde");
    const result = try vm.execute();
    try testing.expectEqual(@as(?usize, 3), result);

    const stats = vm.getStats();
    const inst_stats = stats.inst_stats.?;
    // Find choice instructions and verify at least one was backtracked to.
    var found_backtrack = false;
    for (inst_stats) |s| {
        if (s.backtrack_count > 0) found_backtrack = true;
    }
    try testing.expect(found_backtrack);
}

test "stats: reset between executions" {
    var compiler = try compileEre("a|b");
    var vm = StatsVm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "a");
    _ = try vm.execute();
    const steps1 = vm.getStats().steps;

    // Re-run on same input.
    _ = try vm.execute();
    const steps2 = vm.getStats().steps;

    // Steps should reflect only the latest execution, not accumulate.
    try testing.expectEqual(steps1, steps2);
}
