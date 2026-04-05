/// Grammar parsing VM.
///
/// Executes bytecode produced by the Compiler. Uses a backtracking
/// stack for ordered choice and a call stack for rule invocations.
const std = @import("std");
const I = @import("Instruction.zig");

const Vm = @This();

const max_stack = 1024;
const max_captures = 64;

const Entry = union(enum) {
    /// Backtrack point: saved position and instruction to jump to on failure.
    choice: struct {
        pos: usize,
        pc: u32,
    },
    /// Return address for rule calls.
    ret: u32,
    /// Undo log for a save instruction: restore old value on backtrack.
    save: struct {
        slot: u16,
        old: ?usize,
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

pub const Writer = @TypeOf(@as(std.fs.File.Writer, undefined).interface);

pub const Trace = struct {
    writer: *Writer,
};

pub fn init(code: []const I.Inst, charsets: []const I.Charset, string_data: []const u8, input: []const u8) Vm {
    return .{ .code = code, .charsets = charsets, .string_data = string_data, .input = input };
}

/// Packrat constructor. Allocates a memo table sized for `memo_rule_count`
/// rules and `input.len + 1` positions. Call `deinit` to free it. If
/// `memo_rule_count` is 0 the call is equivalent to `init`.
pub fn initPackrat(
    allocator: std.mem.Allocator,
    code: []const I.Inst,
    charsets: []const I.Charset,
    string_data: []const u8,
    memo_rule_count: u16,
    input: []const u8,
) !Vm {
    var vm = Vm{
        .code = code,
        .charsets = charsets,
        .string_data = string_data,
        .input = input,
        .memo_rule_count = memo_rule_count,
    };
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

pub fn deinit(self: *Vm) void {
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
        self.memo_allocator = null;
    }
}

/// Run the VM. Returns the position after the match, or null on failure.
/// Errors are only produced by the packrat path (allocating head
/// state during Warth's seed-growing); the non-packrat path is
/// infallible.
pub fn execute(self: *Vm) !?usize {
    var pc: u32 = 0;
    var pos: usize = 0;
    var stack: [max_stack]Entry = undefined;
    var sp: usize = 0;

    self.steps = 0;
    while (pc < self.code.len) {
        const inst = self.code[pc];
        self.steps += 1;
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
                stack[sp] = .{ .choice = .{ .pos = pos, .pc = inst.data.offset } };
                sp += 1;
                pc += 1;
            },
            .commit => {
                // Pop the backtrack entry (discard it) and jump.
                sp -= 1;
                pc = inst.data.offset;
            },
            .fail => {
                if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                return null;
            },
            .fail_twice => {
                // Pop one entry then fail.
                sp -= 1;
                if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                return null;
            },
            .jump => {
                pc = inst.data.offset;
            },
            .call => {
                stack[sp] = .{ .ret = pc + 1 };
                sp += 1;
                pc = inst.data.offset;
            },
            .memo_call => {
                const mc = inst.data.memo;
                if (self.memo_table.len == 0) {
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
                            stack[sp] = .{ .memo = .{
                                .rule_id = mc.rule_id,
                                .is_recall = true,
                                .start_pos = @intCast(pos),
                                .return_pc = pc + 1,
                                .rule_entry_pc = mc.offset,
                                .best_end = grow_sentinel,
                                .head_idx = active_head,
                            } };
                            sp += 1;
                            pc = mc.offset;
                            continue;
                        }
                    }

                    switch (self.memo_table[idx].state) {
                        .success => {
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
                            } };
                            sp += 1;
                            pc = mc.offset;
                        },
                    }
                }
            },
            .ret => {
                sp -= 1;
                switch (stack[sp]) {
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
                                self.memo_table[idx] = .{ .state = .success, .next_pos_or_frame = cur_end };
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
                                    self.memo_table[idx] = .{ .state = .success, .next_pos_or_frame = cur_end };
                                    self.heads[m.start_pos] = m.head_idx;
                                    try self.resetEvalSet(m.head_idx);
                                    var frame = m;
                                    frame.best_end = cur_end;
                                    stack[sp] = .{ .memo = frame };
                                    sp += 1;
                                    pos = m.start_pos;
                                    pc = m.rule_entry_pc;
                                } else {
                                    // Participant in someone else's
                                    // cycle: hand answer back up.
                                    self.memo_table[idx] = .{ .state = .success, .next_pos_or_frame = cur_end };
                                    pc = m.return_pc;
                                }
                            } else {
                                self.memo_table[idx] = .{ .state = .success, .next_pos_or_frame = cur_end };
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
                                self.memo_table[idx] = .{ .state = .success, .next_pos_or_frame = new_best };
                                try self.resetEvalSet(m.head_idx);
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
                const slot = inst.data.slot;
                stack[sp] = .{ .save = .{ .slot = slot, .old = self.captures[slot] } };
                sp += 1;
                self.captures[slot] = pos;
                pc += 1;
            },
            .match => {
                return pos;
            },
        }
    }
    return null;
}

fn backtrack(self: *Vm, stack: *[max_stack]Entry, sp: *usize, pc: *u32, pos: *usize) bool {
    while (sp.* > 0) {
        sp.* -= 1;
        switch (stack[sp.*]) {
            .choice => |c| {
                if (self.trace) |t| {
                    t.writer.print("      backtrack -> pc={d} pos={d}\n", .{ c.pc, c.pos }) catch {};
                }
                pc.* = c.pc;
                pos.* = c.pos;
                return true;
            },
            .ret => {},
            .save => |s| {
                self.captures[s.slot] = s.old;
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

/// Return the span for capture group `i`, or null if not captured.
pub fn getCapture(self: *const Vm, i: u16) ?Span {
    const start = self.captures[i * 2] orelse return null;
    const end = self.captures[i * 2 + 1] orelse return null;
    return .{ .start = start, .end = end };
}

/// Return the matched slice for capture group `i`, or null if not captured.
pub fn getCaptureSlice(self: *const Vm, i: u16) ?[]const u8 {
    const span = self.getCapture(i) orelse return null;
    return self.input[span.start..span.end];
}

/// Warth's SETUP-LR. Ensure the memo frame at `frame_idx` has a
/// head, then walk the stack downward from `sp_top` until we find a
/// frame already pointing at that head; every memo frame we cross
/// joins the head's involved set.
fn setupLrVm(
    self: *Vm,
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
fn resetEvalSet(self: *Vm, head_idx: u32) !void {
    const h = &self.heads_pool.items[head_idx];
    h.eval.setRangeValue(.{ .start = 0, .end = h.eval.bit_length }, false);
    var it = h.involved.iterator(.{});
    while (it.next()) |bit| h.eval.set(bit);
}

fn traceStep(self: *Vm, pc: u32, pos: usize, sp: usize, inst: I.Inst) void {
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
        .match => w.writeAll("match") catch {},
    }
    w.writeByte('\n') catch {};
}

const testing = std.testing;
const Compiler = @import("Compiler.zig");
const EreScanner = @import("../ere/Scanner.zig");
const EreParser = @import("../ere/Parser.zig");
const PegScanner = @import("../peg/Scanner.zig");
const PegParser = @import("../peg/Parser.zig");

fn compileEre(source: []const u8) Compiler {
    var scanner = EreScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, source);
    const rules = parser.parse() catch return Compiler{};
    return Compiler.compile(rules);
}

fn compilePeg(source: []const u8) Compiler {
    var scanner = PegScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, source);
    const rules = parser.parse() catch return Compiler{};
    return Compiler.compile(rules);
}

fn expectMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = compileEre(source);
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), input);
    const result = try vm.execute();
    try testing.expectEqual(expected, result);
}

fn expectPegMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = compilePeg(source);
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
    var compiler = compileEre("a(bc)d");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "abcd");
    try testing.expectEqual(@as(?usize, 4), try vm.execute());
    try testing.expectEqualStrings("bc", vm.getCaptureSlice(0).?);
}

test "capture: multiple groups" {
    var compiler = compileEre("(a+)(b+)");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "aaabb");
    try testing.expectEqual(@as(?usize, 5), try vm.execute());
    try testing.expectEqualStrings("aaa", vm.getCaptureSlice(0).?);
    try testing.expectEqualStrings("bb", vm.getCaptureSlice(1).?);
}

test "capture: alternation picks correct branch" {
    var compiler = compileEre("(ab)|(cd)");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "cd");
    try testing.expectEqual(@as(?usize, 2), try vm.execute());
    // First group did not match.
    try testing.expectEqual(@as(?Span, null), vm.getCapture(0));
    // Second group matched.
    try testing.expectEqualStrings("cd", vm.getCaptureSlice(1).?);
}

test "capture: nested groups" {
    var compiler = compileEre("((a)(b))");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "ab");
    try testing.expectEqual(@as(?usize, 2), try vm.execute());
    try testing.expectEqualStrings("ab", vm.getCaptureSlice(0).?);
    try testing.expectEqualStrings("a", vm.getCaptureSlice(1).?);
    try testing.expectEqualStrings("b", vm.getCaptureSlice(2).?);
}

test "capture: group with repetition" {
    var compiler = compileEre("(a+)b");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "aaab");
    try testing.expectEqual(@as(?usize, 4), try vm.execute());
    try testing.expectEqualStrings("aaa", vm.getCaptureSlice(0).?);
}

test "capture: no match clears captures" {
    var compiler = compileEre("(a)b");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "ac");
    try testing.expectEqual(@as(?usize, null), try vm.execute());
    // Capture should be null after failed match (undone by backtrack).
    try testing.expectEqual(@as(?Span, null), vm.getCapture(0));
}

fn compilePegOpts(source: []const u8, opts: Compiler.Options) Compiler {
    var scanner = PegScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, source);
    const rules = parser.parse() catch return Compiler{};
    return Compiler.compileOpts(rules, opts);
}

test "packrat: same result as non-packrat on simple grammar" {
    const src =
        \\Main <- Greet " " Name
        \\Greet <- "hi" / "hello"
        \\Name  <- [a-z]+
    ;
    const input = "hello world";

    var plain = compilePegOpts(src, .{ .memoize = false });
    var vm_plain = Vm.init(plain.getCode(), plain.getCharsets(), plain.getStringData(), input);
    const r_plain = try vm_plain.execute();

    var memo = compilePegOpts(src, .{ .memoize = true });
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
    var c = compilePegOpts(src, .{ .memoize = true });
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

    var plain = compilePegOpts(src, .{ .memoize = false });
    var vm_plain = Vm.init(plain.getCode(), plain.getCharsets(), plain.getStringData(), input);
    const r_plain = try vm_plain.execute();

    var memo = compilePegOpts(src, .{ .memoize = true });
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
    // Same idea but E fails — the failure must be cached too, so the
    // second call to E returns fail without re-running the body.
    const src =
        \\S <- E "x" / E "y"
        \\E <- "a" "b" "c"
    ;
    const input = "abqy";

    var plain = compilePegOpts(src, .{ .memoize = false });
    var vm_plain = Vm.init(plain.getCode(), plain.getCharsets(), plain.getStringData(), input);
    const r_plain = try vm_plain.execute();

    var memo = compilePegOpts(src, .{ .memoize = true });
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
    var c = compilePegOpts(src, .{ .memoize = true });
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

test "packrat: captures keep rule out of memoization" {
    // The ERE capture emits `save`, so the whole wrapper rule is
    // ineligible. But it still compiles and runs correctly.
    var c = compilePegOpts("Main <- \"a\" \"b\"", .{ .memoize = true });
    // Single-rule PEG (no multi-rule call sites) yields zero memoizable rules.
    try testing.expectEqual(@as(u16, 0), c.getMemoRuleCount());
    var vm = try Vm.initPackrat(
        testing.allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        c.getMemoRuleCount(),
        "ab",
    );
    defer vm.deinit();
    try testing.expectEqual(@as(?usize, 2), try vm.execute());
}
