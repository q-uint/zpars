/// Static analysis that computes, per rule, a conservative upper bound
/// on how far past its entry position the rule's body can read.
///
/// Two quantities per rule:
///   * `consume_max`  - max bytes consumed on a successful path
///                      (worst-case pos advance when the rule returns).
///   * `examined_max` - max byte offset (relative to entry pos) that any
///                      execution path through the body could read,
///                      including peeks via `&P` / `!P` and reads on
///                      failed alternatives. Always `>= consume_max`.
///
/// `RuntimeState.applyEdit` consults `examined_max[rule_id]` to decide
/// whether a cached `(rule, p)` memo entry can survive a byte-range edit
/// without re-execution. An entry whose
///   `start_pos + examined_max[rule_id] <= edit.start`
/// can be kept verbatim; otherwise its body could have read a byte that
/// the edit changed, so it must be invalidated.
///
/// Bounds are conservative. Rules that participate in a positive-weight
/// CFG cycle (unbounded `*` or `+` loops) or a call-graph cycle (mutual
/// or self recursion) get `examined_max = unbounded_value`, which the
/// caller treats as always-invalidate.
///
/// Caller-provided buffers (no allocator). The Compiler stack-allocates
/// a `Scratch` and a `[]RuleSummary` sized for `max_code` and runs the
/// pass once per compile.
const std = @import("std");
const I = @import("Instruction.zig");

/// Sentinel meaning "no static upper bound." Treated by `applyEdit` as
/// "always invalidate prefix entries for this rule" -- the rule's body
/// could have read any byte up to end of input.
pub const unbounded_value: u32 = std.math.maxInt(u32);

pub const RuleSummary = struct {
    consume_max: u32 = 0,
    examined_max: u32 = 0,

    pub fn isUnbounded(self: RuleSummary) bool {
        return self.examined_max == unbounded_value;
    }
};

/// Per-rule scratch buffers. Sized to the worst-case rule body length
/// (`max_code`). Caller stack-allocates one and reuses across rules in
/// a single `analyze` call.
pub fn Scratch(comptime max_rule_size: u32) type {
    return struct {
        pos_in: [max_rule_size]u32 = undefined,
        reachable: [max_rule_size]bool = undefined,
        on_worklist: [max_rule_size]bool = undefined,
        worklist: [max_rule_size]u32 = undefined,
        lcatch_targets: [max_rule_size]u32 = undefined,
    };
}

/// Compute per-rule summaries. `out_summaries.len` must equal
/// `rule_addrs.len`. The scratch buffers must be sized for at least the
/// largest rule body in `code`.
///
/// `code` is the full compiled bytecode (after `rewriteMemoCalls`, before
/// the optimizer, since the optimizer rewrites bytecode but preserves
/// `examined_max` semantics). Rule `i`'s body is
/// `code[rule_addrs[i]..rule_ends[i]]`, terminated by `ret` (or `match`
/// for the single-rule entry path).
pub fn analyze(
    code: []const I.Inst,
    rule_addrs: []const u32,
    rule_ends: []const u32,
    out_summaries: []RuleSummary,
    scratch: anytype,
) void {
    std.debug.assert(rule_addrs.len == rule_ends.len);
    std.debug.assert(out_summaries.len == rule_addrs.len);
    const n = rule_addrs.len;

    for (out_summaries) |*s| s.* = .{};
    if (n == 0) return;

    // Outer fixed-point loop: re-run every rule's analysis until none of
    // their summaries change. Mutual recursion never converges -- the
    // iteration cap below promotes still-changing rules to unbounded.
    const max_iter: usize = @max(@as(usize, 16), n * 4);
    var iter: usize = 0;
    while (iter < max_iter) : (iter += 1) {
        var changed = false;
        for (0..n) |rule_idx| {
            const new_summary = analyzeOne(
                code,
                rule_addrs[rule_idx],
                rule_ends[rule_idx],
                rule_addrs,
                out_summaries,
                scratch,
            ) catch RuleSummary{
                .consume_max = unbounded_value,
                .examined_max = unbounded_value,
            };
            if (!summariesEqual(new_summary, out_summaries[rule_idx])) {
                out_summaries[rule_idx] = new_summary;
                changed = true;
            }
        }
        if (!changed) return;
    }

    // Didn't converge -- any rule whose re-analysis still disagrees with
    // the current summary is in an unsettled cycle. Mark unbounded.
    for (0..n) |rule_idx| {
        const re = analyzeOne(
            code,
            rule_addrs[rule_idx],
            rule_ends[rule_idx],
            rule_addrs,
            out_summaries,
            scratch,
        ) catch RuleSummary{
            .consume_max = unbounded_value,
            .examined_max = unbounded_value,
        };
        if (!summariesEqual(re, out_summaries[rule_idx])) {
            out_summaries[rule_idx] = .{
                .consume_max = unbounded_value,
                .examined_max = unbounded_value,
            };
        }
    }
}

fn summariesEqual(a: RuleSummary, b: RuleSummary) bool {
    return a.consume_max == b.consume_max and a.examined_max == b.examined_max;
}

const InnerError = error{Unbounded};

/// CFG dataflow for a single rule. Tracks `pos_offset_in[pc]` (max
/// pos-offset relative to rule entry on any path reaching `pc`) via a
/// worklist; updates `examined_max` on input-reading instructions and
/// `consume_max` on `ret`. Returns `Unbounded` if the worklist runs past
/// `work_cap` (signals a positive-weight CFG cycle) or if the rule body
/// exceeds the scratch buffers' static size.
fn analyzeOne(
    code: []const I.Inst,
    rule_start: u32,
    rule_end: u32,
    rule_addrs: []const u32,
    callee_summaries: []const RuleSummary,
    scratch: anytype,
) InnerError!RuleSummary {
    if (rule_end <= rule_start) return .{};
    const len: u32 = rule_end - rule_start;
    if (len > scratch.pos_in.len) return error.Unbounded;

    const pos_in = scratch.pos_in[0..len];
    const reachable = scratch.reachable[0..len];
    const on_worklist = scratch.on_worklist[0..len];
    const worklist = scratch.worklist[0..len];
    const lcatch_targets = scratch.lcatch_targets[0..len];

    @memset(pos_in, 0);
    @memset(reachable, false);
    @memset(on_worklist, false);

    var wl_len: usize = 0;
    var lc_len: usize = 0;

    reachable[0] = true;
    pos_in[0] = 0;
    worklist[wl_len] = 0;
    wl_len += 1;
    on_worklist[0] = true;

    var examined_max: u32 = 0;
    var consume_max: u32 = 0;

    // Worklist work cap. Loose upper bound on relaxation steps for an
    // acyclic CFG. Going past this means a positive-weight cycle exists
    // (unbounded loops, recursive calls).
    const work_cap: usize = @max(@as(usize, 256), @as(usize, len) * 32);
    var work_count: usize = 0;

    // Outer loop alternates worklist drain with lcatch handler-PC
    // re-seeding. A handler runs at the throw-site pos, which is bounded
    // above by the rule body's `examined_max`. As the body discovers
    // more reads, the handler's incoming pos_offset grows, so we reseed
    // once per outer pass until stable.
    var last_seed: u32 = 0;
    while (true) {
        while (wl_len > 0) {
            if (work_count >= work_cap) return error.Unbounded;
            work_count += 1;

            wl_len -= 1;
            const i = worklist[wl_len];
            on_worklist[i] = false;
            const pc = rule_start + i;
            const inst = code[pc];
            const in_pos = pos_in[i];

            switch (inst.op) {
                .char, .any, .set, .neg_set, .optional_char => {
                    const after = satAdd(in_pos, 1);
                    examined_max = @max(examined_max, after);
                    try propagate(i + 1, after, len, pos_in, reachable, on_worklist, worklist, &wl_len);
                },
                .string => {
                    const slen: u32 = inst.data.string.len;
                    const after = satAdd(in_pos, slen);
                    examined_max = @max(examined_max, after);
                    try propagate(i + 1, after, len, pos_in, reachable, on_worklist, worklist, &wl_len);
                },
                .save, .event_open, .event_close, .event_error_open,
                .event_error_close, .event_missing, .event_token, .event_field => {
                    try propagate(i + 1, in_pos, len, pos_in, reachable, on_worklist, worklist, &wl_len);
                },
                .choice => {
                    const target = try ruleLocalTarget(inst.data.offset, rule_start, rule_end);
                    try propagate(i + 1, in_pos, len, pos_in, reachable, on_worklist, worklist, &wl_len);
                    try propagate(target, in_pos, len, pos_in, reachable, on_worklist, worklist, &wl_len);
                },
                .commit, .jump => {
                    const target = try ruleLocalTarget(inst.data.offset, rule_start, rule_end);
                    try propagate(target, in_pos, len, pos_in, reachable, on_worklist, worklist, &wl_len);
                },
                .lcatch => {
                    const target = try ruleLocalTarget(inst.data.catch_handler.handler_pc, rule_start, rule_end);
                    try propagate(i + 1, in_pos, len, pos_in, reachable, on_worklist, worklist, &wl_len);
                    var seen = false;
                    for (lcatch_targets[0..lc_len]) |existing| {
                        if (existing == target) {
                            seen = true;
                            break;
                        }
                    }
                    if (!seen) {
                        lcatch_targets[lc_len] = target;
                        lc_len += 1;
                    }
                },
                .fail, .fail_twice, .throw => {
                    // Path terminates here. The matching choice/lcatch's
                    // L successor already covers post-failure flow.
                },
                .call, .memo_call => {
                    const target_addr = if (inst.op == .call) inst.data.offset else inst.data.memo.offset;
                    const callee_idx = ruleIdxForAddr(target_addr, rule_addrs) orelse {
                        return error.Unbounded;
                    };
                    const cs = callee_summaries[callee_idx];
                    if (cs.examined_max == unbounded_value) return error.Unbounded;
                    examined_max = @max(examined_max, satAdd(in_pos, cs.examined_max));
                    const after = satAdd(in_pos, cs.consume_max);
                    try propagate(i + 1, after, len, pos_in, reachable, on_worklist, worklist, &wl_len);
                },
                .ret, .match => {
                    consume_max = @max(consume_max, in_pos);
                    examined_max = @max(examined_max, in_pos);
                },
            }
        }

        if (lc_len == 0) break;
        if (examined_max == last_seed) break;
        last_seed = examined_max;
        for (lcatch_targets[0..lc_len]) |target| {
            try propagate(target, examined_max, len, pos_in, reachable, on_worklist, worklist, &wl_len);
        }
    }

    if (examined_max == unbounded_value) return error.Unbounded;

    return .{ .consume_max = consume_max, .examined_max = examined_max };
}

/// Saturating add: if `a + b` would overflow u32, return `unbounded_value`.
inline fn satAdd(a: u32, b: u32) u32 {
    const s, const overflow = @addWithOverflow(a, b);
    return if (overflow != 0) unbounded_value else s;
}

/// Translate an absolute bytecode address into a rule-local index, or
/// return `Unbounded` if the target lies outside the rule's body.
fn ruleLocalTarget(abs_pc: u32, rule_start: u32, rule_end: u32) InnerError!u32 {
    if (abs_pc < rule_start or abs_pc >= rule_end) return error.Unbounded;
    return abs_pc - rule_start;
}

fn ruleIdxForAddr(abs_pc: u32, rule_addrs: []const u32) ?usize {
    for (rule_addrs, 0..) |addr, i| {
        if (addr == abs_pc) return i;
    }
    return null;
}

/// Mark `i` reachable with `pos_in[i] >= new_in` and queue it for
/// re-propagation if its value strictly grew. `i >= len` happens when
/// fall-through runs past the rule's body (compiler always emits a
/// terminating `ret`, so this is benign in practice; treat as a
/// dead-end path).
fn propagate(
    i: u32,
    new_in: u32,
    len: u32,
    pos_in: []u32,
    reachable: []bool,
    on_worklist: []bool,
    worklist: []u32,
    wl_len: *usize,
) InnerError!void {
    if (i >= len) return;
    if (reachable[i] and pos_in[i] >= new_in) return;
    reachable[i] = true;
    pos_in[i] = new_in;
    if (!on_worklist[i]) {
        worklist[wl_len.*] = i;
        wl_len.* += 1;
        on_worklist[i] = true;
    }
}

const testing = std.testing;

const TestScratch = Scratch(4096);

fn analyzeWith(
    code: []const I.Inst,
    rule_addrs: []const u32,
    rule_ends: []const u32,
    out: []RuleSummary,
) void {
    var scratch: TestScratch = .{};
    analyze(code, rule_addrs, rule_ends, out, &scratch);
}

test "analyze: single-rule literal sequence" {
    const code = [_]I.Inst{
        .{ .op = .char, .data = .{ .byte = 'a' } },
        .{ .op = .char, .data = .{ .byte = 'b' } },
        .{ .op = .ret },
    };
    var out: [1]RuleSummary = undefined;
    analyzeWith(&code, &.{0}, &.{3}, &out);

    try testing.expectEqual(@as(u32, 2), out[0].consume_max);
    try testing.expectEqual(@as(u32, 2), out[0].examined_max);
}

test "analyze: alternation takes max over branches" {
    const code = [_]I.Inst{
        .{ .op = .choice, .data = .{ .offset = 3 } },
        .{ .op = .char, .data = .{ .byte = 'a' } },
        .{ .op = .commit, .data = .{ .offset = 5 } },
        .{ .op = .char, .data = .{ .byte = 'b' } },
        .{ .op = .char, .data = .{ .byte = 'b' } },
        .{ .op = .ret },
    };
    var out: [1]RuleSummary = undefined;
    analyzeWith(&code, &.{0}, &.{6}, &out);

    try testing.expectEqual(@as(u32, 2), out[0].consume_max);
    try testing.expectEqual(@as(u32, 2), out[0].examined_max);
}

test "analyze: leading not-predicate (!P P)" {
    // `!"x" "y"`:
    //   choice L
    //   char 'x'
    //   fail_twice
    // L: char 'y'
    //   ret
    const code = [_]I.Inst{
        .{ .op = .choice, .data = .{ .offset = 3 } },
        .{ .op = .char, .data = .{ .byte = 'x' } },
        .{ .op = .fail_twice },
        .{ .op = .char, .data = .{ .byte = 'y' } },
        .{ .op = .ret },
    };
    var out: [1]RuleSummary = undefined;
    analyzeWith(&code, &.{0}, &.{5}, &out);

    try testing.expectEqual(@as(u32, 1), out[0].consume_max);
    try testing.expectEqual(@as(u32, 1), out[0].examined_max);
}

test "analyze: trailing not-predicate increases examined past consume" {
    // `"y" !"x"`:
    //   char 'y'
    //   choice L
    //   char 'x'
    //   fail_twice
    // L: ret
    const code = [_]I.Inst{
        .{ .op = .char, .data = .{ .byte = 'y' } },
        .{ .op = .choice, .data = .{ .offset = 4 } },
        .{ .op = .char, .data = .{ .byte = 'x' } },
        .{ .op = .fail_twice },
        .{ .op = .ret },
    };
    var out: [1]RuleSummary = undefined;
    analyzeWith(&code, &.{0}, &.{5}, &out);

    try testing.expectEqual(@as(u32, 1), out[0].consume_max);
    try testing.expectEqual(@as(u32, 2), out[0].examined_max);
}

test "analyze: call propagates callee bounds" {
    // Rule 0: call rule 1; ret
    // Rule 1: char; char; ret
    const code = [_]I.Inst{
        .{ .op = .call, .data = .{ .offset = 2 } },
        .{ .op = .ret },
        .{ .op = .char, .data = .{ .byte = 'a' } },
        .{ .op = .char, .data = .{ .byte = 'b' } },
        .{ .op = .ret },
    };
    var out: [2]RuleSummary = undefined;
    analyzeWith(&code, &.{ 0, 2 }, &.{ 2, 5 }, &out);

    try testing.expectEqual(@as(u32, 2), out[1].consume_max);
    try testing.expectEqual(@as(u32, 2), out[1].examined_max);
    try testing.expectEqual(@as(u32, 2), out[0].consume_max);
    try testing.expectEqual(@as(u32, 2), out[0].examined_max);
}

test "analyze: pure self-recursion has examined_max zero" {
    // `R = R`. The analysis's (0, 0) fixed point is correct: each
    // (theoretical) invocation reads zero bytes. At runtime the call
    // loops forever and never returns success, so no `.success` memo
    // entry can be produced -- there is nothing to invalidate.
    const code = [_]I.Inst{
        .{ .op = .call, .data = .{ .offset = 0 } },
        .{ .op = .ret },
    };
    var out: [1]RuleSummary = undefined;
    analyzeWith(&code, &.{0}, &.{2}, &out);

    try testing.expectEqual(@as(u32, 0), out[0].consume_max);
    try testing.expectEqual(@as(u32, 0), out[0].examined_max);
}

test "analyze: self-recursion with progress is unbounded" {
    // `R = "a" R`. Each invocation consumes one byte before recursing,
    // so the call's contribution to `examined_max` strictly grows each
    // outer-loop iteration -- eventually trips the iter cap and gets
    // marked unbounded.
    const code = [_]I.Inst{
        .{ .op = .char, .data = .{ .byte = 'a' } },
        .{ .op = .call, .data = .{ .offset = 0 } },
        .{ .op = .ret },
    };
    var out: [1]RuleSummary = undefined;
    analyzeWith(&code, &.{0}, &.{3}, &out);

    try testing.expect(out[0].isUnbounded());
}

test "analyze: mutual recursion with progress is unbounded" {
    // Rule 0: char 'a'; call rule 1; ret
    // Rule 1: char 'b'; call rule 0; ret
    const code = [_]I.Inst{
        .{ .op = .char, .data = .{ .byte = 'a' } },
        .{ .op = .call, .data = .{ .offset = 3 } },
        .{ .op = .ret },
        .{ .op = .char, .data = .{ .byte = 'b' } },
        .{ .op = .call, .data = .{ .offset = 0 } },
        .{ .op = .ret },
    };
    var out: [2]RuleSummary = undefined;
    analyzeWith(&code, &.{ 0, 3 }, &.{ 3, 6 }, &out);

    try testing.expect(out[0].isUnbounded());
    try testing.expect(out[1].isUnbounded());
}

test "analyze: unbounded loop (a*) is unbounded" {
    // L: choice end; char 'a'; commit L; end: ret
    const code = [_]I.Inst{
        .{ .op = .choice, .data = .{ .offset = 3 } },
        .{ .op = .char, .data = .{ .byte = 'a' } },
        .{ .op = .commit, .data = .{ .offset = 0 } },
        .{ .op = .ret },
    };
    var out: [1]RuleSummary = undefined;
    analyzeWith(&code, &.{0}, &.{4}, &out);

    try testing.expect(out[0].isUnbounded());
}

test "analyze: bounded repetition (a){0,3} is bounded" {
    const code = [_]I.Inst{
        .{ .op = .choice, .data = .{ .offset = 3 } }, // 0
        .{ .op = .char, .data = .{ .byte = 'a' } }, // 1
        .{ .op = .commit, .data = .{ .offset = 3 } }, // 2
        .{ .op = .choice, .data = .{ .offset = 6 } }, // 3
        .{ .op = .char, .data = .{ .byte = 'a' } }, // 4
        .{ .op = .commit, .data = .{ .offset = 6 } }, // 5
        .{ .op = .choice, .data = .{ .offset = 9 } }, // 6
        .{ .op = .char, .data = .{ .byte = 'a' } }, // 7
        .{ .op = .commit, .data = .{ .offset = 9 } }, // 8
        .{ .op = .ret }, // 9
    };
    var out: [1]RuleSummary = undefined;
    analyzeWith(&code, &.{0}, &.{10}, &out);

    try testing.expectEqual(@as(u32, 3), out[0].consume_max);
    try testing.expectEqual(@as(u32, 3), out[0].examined_max);
}

test "analyze: string instruction contributes its length" {
    const code = [_]I.Inst{
        .{ .op = .string, .data = .{ .string = .{ .offset = 0, .len = 5 } } },
        .{ .op = .ret },
    };
    var out: [1]RuleSummary = undefined;
    analyzeWith(&code, &.{0}, &.{2}, &out);

    try testing.expectEqual(@as(u32, 5), out[0].consume_max);
    try testing.expectEqual(@as(u32, 5), out[0].examined_max);
}

test "analyze: empty rule body returns zero" {
    const code = [_]I.Inst{
        .{ .op = .ret },
    };
    var out: [1]RuleSummary = undefined;
    analyzeWith(&code, &.{0}, &.{1}, &out);

    try testing.expectEqual(@as(u32, 0), out[0].consume_max);
    try testing.expectEqual(@as(u32, 0), out[0].examined_max);
}
