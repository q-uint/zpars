/// Runtime ABNF matcher — tree-walking interpreter over `Ast.Node`.
///
/// Unlike the comptime `Abnf.Compile`, this matcher works with grammars
/// loaded at runtime. Feed it the merged rules from `Validator.validate()`
/// and match arbitrary input strings against any rule.
///
///     var scanner = Scanner.init(grammar);
///     const tokens = scanner.scanTokens();
///     var parser = Parser.init(tokens, grammar);
///     const rules = try parser.parse();
///     var validator = Validator.init(allocator, rules);
///     const merged = try validator.validate();
///     const matcher = Matcher.init(allocator, merged);
///     const r = matcher.match("start", "hello world").?;
///     // r.value == "hello", r.rest == " world"
const std = @import("std");
const Ast = @import("Ast.zig");

const Matcher = @This();

pub const Result = struct {
    /// The matched input span.
    value: []const u8,
    /// Unconsumed input after the match.
    rest: []const u8,
};

/// Maximum recursion depth to guard against stack overflow.
const max_depth = 256;

const RuleIndex = std.hash_map.StringHashMapUnmanaged(u32);

pub const MemoKind = enum(u8) {
    /// Not yet attempted.
    empty,
    /// Body evaluation is currently on the stack. `payload` is the
    /// index of an LrFrame in lr_stack, whose seed is consulted on
    /// self-recursion. This also acts as the "current best seed"
    /// during GROW-LR: its `end_pos` is read from the frame.
    lr,
    /// Cached success; payload holds the end offset of the match.
    success,
    /// Cached failure.
    fail,
};

pub const MemoEntry = struct {
    kind: MemoKind,
    payload: u32,
};

/// One active APPLY-RULE frame. While its (rule, pos) memo entry is
/// kind=.lr, inner calls read the seed here. Once the first body
/// evaluation returns, `head_idx` is inspected: null means no LR
/// happened, otherwise we entered LR-ANSWER / GROW-LR.
const LrFrame = struct {
    rule_id: u32,
    start_pos: u32,
    /// null = FAIL seed; otherwise end_pos of the current seed.
    seed_end: ?u32,
    /// null until SETUP-LR attaches this frame to a head.
    head_idx: ?u32,
};

/// Warth's Head: tracks the rules involved in a given LR cycle at a
/// given position, plus a working set for the current grow iteration.
const Head = struct {
    rule_id: u32,
    /// Bitset of rule ids involved in this LR cycle.
    involved: std.DynamicBitSetUnmanaged,
    /// Bitset (subset of involved) of rules still to re-evaluate in
    /// the current grow iteration.
    eval: std.DynamicBitSetUnmanaged,
};

rules: []const Ast.Rule,
rule_index: RuleIndex,
/// Start pointer of the input passed to `match()`, used for anchor_start.
match_input_start: [*]const u8 = undefined,
/// Total length of the input passed to match(), used for memo indexing.
match_input_len: usize = 0,
/// Packrat memo table. Empty unless matchPackrat is in use.
/// Layout: entry(rule_id, pos) = packrat_memo[rule_id * memo_stride + pos],
/// where memo_stride = match_input_len + 1.
packrat_memo: []MemoEntry = &.{},
memo_stride: usize = 0,
/// Stack of active LR frames, in call order. Frames are pushed in
/// applyRule and stay alive for the entire matchPackrat so that
/// memo entries of kind .lr can safely reference them by index.
lr_stack: std.ArrayListUnmanaged(LrFrame) = .empty,
/// Per-position head pointer (index into heads_pool), or null.
heads: []?u32 = &.{},
heads_pool: std.ArrayListUnmanaged(Head) = .empty,
/// Allocator used for packrat state (lr_stack, heads_pool, bitsets).
/// Set by matchPackrat, cleared on return.
packrat_allocator: ?std.mem.Allocator = null,
/// Counts rule-body descents (i.e. actual work done, not memo hits).
/// Useful for measuring the reduction from memoization.
rule_body_entries: u64 = 0,
/// Counts memo hits (success + fail + in_progress). Zero outside packrat.
memo_hits: u64 = 0,

pub fn init(allocator: std.mem.Allocator, rules: []const Ast.Rule) Matcher {
    var index = RuleIndex{};
    index.ensureTotalCapacity(allocator, @intCast(rules.len)) catch {};
    for (rules, 0..) |rule, i| {
        const key = asciiLowerAlloc(allocator, rule.name) catch rule.name;
        // Only store first occurrence; rules are already merged by Validator.
        _ = index.getOrPutValue(allocator, key, @intCast(i)) catch {};
    }
    return .{ .rules = rules, .rule_index = index };
}

fn asciiLowerAlloc(allocator: std.mem.Allocator, s: []const u8) ![]const u8 {
    const buf = try allocator.alloc(u8, s.len);
    for (s, 0..) |c, i| {
        buf[i] = std.ascii.toLower(c);
    }
    return buf;
}

/// Match `input` against the rule named `rule_name`.
/// Returns null if the rule is not found or the input does not match.
pub fn match(self: *Matcher, rule_name: []const u8, input: []const u8) ?Result {
    self.match_input_start = input.ptr;
    self.match_input_len = input.len;
    self.packrat_memo = &.{};
    self.memo_stride = 0;
    self.rule_body_entries = 0;
    self.memo_hits = 0;
    return self.matchRulename(rule_name, input, 0);
}

/// Packrat-memoized match with Warth's seed-growing left-recursion
/// support (including indirect LR via involved/eval sets). Allocates
/// a memo table sized (num_rules * (input.len+1)) plus per-position
/// head pointers and an LR stack. All packrat state is released
/// before returning.
pub fn matchPackrat(
    self: *Matcher,
    allocator: std.mem.Allocator,
    rule_name: []const u8,
    input: []const u8,
) !?Result {
    self.match_input_start = input.ptr;
    self.match_input_len = input.len;
    self.rule_body_entries = 0;
    self.memo_hits = 0;
    const stride = input.len + 1;

    const table = try allocator.alloc(MemoEntry, self.rules.len * stride);
    @memset(table, .{ .kind = .empty, .payload = 0 });
    self.packrat_memo = table;
    self.memo_stride = stride;

    const heads = try allocator.alloc(?u32, stride);
    @memset(heads, null);
    self.heads = heads;
    self.heads_pool = .empty;
    self.lr_stack = .empty;
    self.packrat_allocator = allocator;

    defer {
        allocator.free(table);
        allocator.free(heads);
        for (self.heads_pool.items) |*h| {
            h.involved.deinit(allocator);
            h.eval.deinit(allocator);
        }
        self.heads_pool.deinit(allocator);
        self.lr_stack.deinit(allocator);
        self.packrat_memo = &.{};
        self.memo_stride = 0;
        self.heads = &.{};
        self.packrat_allocator = null;
    }

    return self.matchRulename(rule_name, input, 0);
}

fn matchNode(self: *const Matcher, node: Ast.Node, input: []const u8, depth: usize) ?Result {
    if (depth > max_depth) return null;

    return switch (node) {
        .char_val => |cv| matchCharVal(cv, input),
        .num_val => |nv| matchNumVal(nv, input),
        .prose_val => null,
        .rulename => |name| self.matchRulename(name, input, depth),
        .alternation => |alts| self.matchAlternation(alts, input, depth),
        .concatenation => |elems| self.matchConcatenation(elems, input, depth),
        .repetition => |rep| self.matchRepetition(rep, input, depth),
        .and_predicate => |inner| {
            // Succeed if inner matches, but consume nothing.
            if (self.matchNode(inner.*, input, depth + 1)) |_|
                return .{ .value = input[0..0], .rest = input }
            else
                return null;
        },
        .not_predicate => |inner| {
            // Succeed if inner does NOT match, consume nothing.
            if (self.matchNode(inner.*, input, depth + 1)) |_|
                return null
            else
                return .{ .value = input[0..0], .rest = input };
        },
        .char_class => |ranges| matchCharClass(ranges, input),
        .neg_char_class => |ranges| matchNegCharClass(ranges, input),
        .anchor_start => {
            if (input.ptr == self.match_input_start)
                return .{ .value = input[0..0], .rest = input }
            else
                return null;
        },
        .anchor_end => {
            if (input.len == 0)
                return .{ .value = input[0..0], .rest = input }
            else
                return null;
        },
        .any => {
            if (input.len == 0) return null;
            return .{ .value = input[0..1], .rest = input[1..] };
        },
        .capture => |inner| self.matchNode(inner.*, input, depth + 1),
    };
}

fn matchCharClass(ranges: []const Ast.ClassRange, input: []const u8) ?Result {
    if (input.len == 0) return null;
    const c = input[0];
    for (ranges) |r| {
        if (c >= r.lo and c <= r.hi)
            return .{ .value = input[0..1], .rest = input[1..] };
    }
    return null;
}

fn matchNegCharClass(ranges: []const Ast.ClassRange, input: []const u8) ?Result {
    if (input.len == 0) return null;
    const c = input[0];
    for (ranges) |r| {
        if (c >= r.lo and c <= r.hi) return null;
    }
    return .{ .value = input[0..1], .rest = input[1..] };
}

fn matchCharVal(cv: Ast.CharVal, input: []const u8) ?Result {
    if (input.len < cv.value.len) return null;
    const span = input[0..cv.value.len];

    if (cv.case_sensitive) {
        if (!std.mem.eql(u8, span, cv.value)) return null;
    } else {
        if (!std.ascii.eqlIgnoreCase(span, cv.value)) return null;
    }

    return .{ .value = span, .rest = input[cv.value.len..] };
}

fn matchNumVal(nv: Ast.NumVal, input: []const u8) ?Result {
    switch (nv) {
        .single => |byte| {
            if (input.len == 0 or input[0] != byte) return null;
            return .{ .value = input[0..1], .rest = input[1..] };
        },
        .range => |r| {
            if (input.len == 0 or input[0] < r.lo or input[0] > r.hi) return null;
            return .{ .value = input[0..1], .rest = input[1..] };
        },
        .concat => |bytes| {
            if (input.len < bytes.len) return null;
            if (!std.mem.eql(u8, input[0..bytes.len], bytes)) return null;
            return .{ .value = input[0..bytes.len], .rest = input[bytes.len..] };
        },
    }
}

fn matchAlternation(self: *const Matcher, alts: []const Ast.Node, input: []const u8, depth: usize) ?Result {
    for (alts) |alt| {
        if (self.matchNode(alt, input, depth)) |r| return r;
    }
    return null;
}

fn matchConcatenation(self: *const Matcher, elems: []const Ast.Node, input: []const u8, depth: usize) ?Result {
    var rest = input;
    for (elems) |elem| {
        const r = self.matchNode(elem, rest, depth) orelse return null;
        rest = r.rest;
    }
    return .{ .value = input[0 .. input.len - rest.len], .rest = rest };
}

fn matchRepetition(self: *const Matcher, rep: Ast.Repetition, input: []const u8, depth: usize) ?Result {
    var rest = input;
    var count: usize = 0;

    while (rep.max == null or count < rep.max.?) {
        const r = self.matchNode(rep.element.*, rest, depth) orelse break;
        // Guard against zero-length matches causing infinite loops.
        if (r.rest.len == rest.len) break;
        rest = r.rest;
        count += 1;
    }

    if (count < rep.min) return null;
    return .{ .value = input[0 .. input.len - rest.len], .rest = rest };
}

fn matchRulename(self: *const Matcher, name: []const u8, input: []const u8, depth: usize) ?Result {
    // Core rules (RFC 5234 Appendix B) bypass memoization entirely.
    if (matchCoreRule(name, input)) |r| return r;

    var lower_buf: [256]u8 = undefined;
    const key = asciiLowerBuf(name, &lower_buf) orelse return null;
    const idx = self.rule_index.get(key) orelse return null;

    const m: *Matcher = @constCast(self);

    if (self.packrat_memo.len == 0) {
        m.rule_body_entries += 1;
        return self.matchNode(self.rules[idx].node, input, depth + 1);
    }

    return self.applyRule(idx, input, depth);
}

/// Byte offset of `input.ptr` relative to `match_input_start`.
fn posOf(self: *const Matcher, input: []const u8) u32 {
    return @intCast(@intFromPtr(input.ptr) - @intFromPtr(self.match_input_start));
}

/// Reconstruct a Result from a (start, end) pair using the original input.
fn resultFrom(self: *const Matcher, start: u32, end: u32) Result {
    const whole = self.match_input_start[0..self.match_input_len];
    return .{ .value = whole[start..end], .rest = whole[end..] };
}

/// Read the current memo entry for (rule_id, pos), applying RECALL's
/// adjustments when a head is active at this position.
fn recall(self: *const Matcher, rule_id: u32, pos: u32, depth: usize) RecallResult {
    const entry_idx = rule_id * self.memo_stride + pos;
    const m: *Matcher = @constCast(self);
    var entry = self.packrat_memo[entry_idx];
    const h_opt = self.heads[pos];
    if (h_opt == null) return .{ .entry = entry };

    const h = &m.heads_pool.items[h_opt.?];

    // Rules outside the involved set (and not the head itself) must
    // not piggyback on the current grow iteration: pretend they fail.
    if (entry.kind == .empty and rule_id != h.rule_id and !h.involved.isSet(rule_id)) {
        return .{ .entry = .{ .kind = .fail, .payload = 0 } };
    }

    // Rules still queued for re-evaluation this iteration: remove them
    // from the eval set and recompute their answer now. Results must
    // only grow monotonically across iterations; a re-eval producing
    // a smaller end (or FAIL) is ignored so that a late failing
    // iteration does not clobber an earlier successful seed.
    if (h.eval.isSet(rule_id)) {
        h.eval.unset(rule_id);
        const whole = self.match_input_start[0..self.match_input_len];
        const slice = whole[pos..];
        m.rule_body_entries += 1;
        const ans = self.matchNode(self.rules[rule_id].node, slice, depth + 1);
        if (ans) |ok| {
            const end: u32 = @intCast(pos + (slice.len - ok.rest.len));
            const prev = self.packrat_memo[entry_idx];
            const prev_end: u32 = if (prev.kind == .success) prev.payload else pos;
            if (end > prev_end) {
                self.packrat_memo[entry_idx] = .{ .kind = .success, .payload = end };
            }
        }
        entry = self.packrat_memo[entry_idx];
    }
    return .{ .entry = entry };
}

const RecallResult = struct { entry: MemoEntry };

/// Warth's APPLY-RULE on (rule_id, pos=posOf(input)).
fn applyRule(self: *const Matcher, rule_id: u32, input: []const u8, depth: usize) ?Result {
    const pos = self.posOf(input);
    const entry_idx = rule_id * self.memo_stride + pos;
    const m: *Matcher = @constCast(self);
    const allocator = self.packrat_allocator.?;

    const rc = self.recall(rule_id, pos, depth);
    const entry = rc.entry;

    switch (entry.kind) {
        .success => {
            m.memo_hits += 1;
            return self.resultFrom(pos, entry.payload);
        },
        .fail => {
            m.memo_hits += 1;
            return null;
        },
        .lr => {
            // Re-entering a rule whose body is still being evaluated.
            m.memo_hits += 1;
            const lr_idx = entry.payload;
            self.setupLr(rule_id, lr_idx) catch return null;
            const lr = m.lr_stack.items[lr_idx];
            if (lr.seed_end) |end| {
                return self.resultFrom(lr.start_pos, end);
            }
            return null;
        },
        .empty => {
            // Fresh APPLY-RULE. Push an LR frame with FAIL seed and
            // tag the memo entry .lr while the body runs.
            const lr_idx: u32 = @intCast(m.lr_stack.items.len);
            m.lr_stack.append(allocator, .{
                .rule_id = rule_id,
                .start_pos = pos,
                .seed_end = null,
                .head_idx = null,
            }) catch return null;
            self.packrat_memo[entry_idx] = .{ .kind = .lr, .payload = lr_idx };

            m.rule_body_entries += 1;
            const ans = self.matchNode(self.rules[rule_id].node, input, depth + 1);

            const head_idx = m.lr_stack.items[lr_idx].head_idx;
            _ = m.lr_stack.pop();

            if (head_idx == null) {
                if (ans) |ok| {
                    const end: u32 = @intCast(pos + (input.len - ok.rest.len));
                    self.packrat_memo[entry_idx] = .{ .kind = .success, .payload = end };
                } else {
                    self.packrat_memo[entry_idx] = .{ .kind = .fail, .payload = 0 };
                }
                return ans;
            }

            // LR was detected. Run LR-ANSWER.
            return self.lrAnswer(rule_id, pos, head_idx.?, ans, input, depth);
        },
    }
}

/// Warth's SETUP-LR: attach a Head to the frame at lr_idx (creating
/// one if needed) and drag every frame above it into the Head's
/// involved set.
fn setupLr(self: *const Matcher, rule_id: u32, lr_idx: u32) !void {
    const m: *Matcher = @constCast(self);
    const allocator = self.packrat_allocator.?;
    const num_rules = self.rules.len;

    if (m.lr_stack.items[lr_idx].head_idx == null) {
        var involved = try std.DynamicBitSetUnmanaged.initEmpty(allocator, num_rules);
        const eval = try std.DynamicBitSetUnmanaged.initEmpty(allocator, num_rules);
        involved.set(rule_id);
        try m.heads_pool.append(allocator, .{
            .rule_id = rule_id,
            .involved = involved,
            .eval = eval,
        });
        m.lr_stack.items[lr_idx].head_idx = @intCast(m.heads_pool.items.len - 1);
    }
    const head_idx = m.lr_stack.items[lr_idx].head_idx.?;

    // Walk from the top of lr_stack down until we hit a frame already
    // pointing at this head; everything we cross is in the cycle.
    var i: usize = m.lr_stack.items.len;
    while (i > 0) {
        i -= 1;
        const fr = &m.lr_stack.items[i];
        if (fr.head_idx != null and fr.head_idx.? == head_idx) break;
        fr.head_idx = head_idx;
        m.heads_pool.items[head_idx].involved.set(fr.rule_id);
    }
}

/// Warth's LR-ANSWER: the body finished; if the memo still points
/// at a head whose rule matches ours, we're the outer frame and must
/// grow. Otherwise we were just a participant and return the seed.
fn lrAnswer(
    self: *const Matcher,
    rule_id: u32,
    pos: u32,
    head_idx: u32,
    first_ans: ?Result,
    input: []const u8,
    depth: usize,
) ?Result {
    const m: *Matcher = @constCast(self);
    const entry_idx = rule_id * self.memo_stride + pos;

    if (m.heads_pool.items[head_idx].rule_id != rule_id) {
        // Someone else is the head of this cycle; finalise our own
        // memo with whatever the first eval produced and hand the
        // answer back up. Leaving the memo as .lr would leave a
        // dangling lr_stack index since our frame has been popped.
        if (first_ans) |ok| {
            const end: u32 = @intCast(pos + (input.len - ok.rest.len));
            self.packrat_memo[entry_idx] = .{ .kind = .success, .payload = end };
        } else {
            self.packrat_memo[entry_idx] = .{ .kind = .fail, .payload = 0 };
        }
        return first_ans;
    }

    if (first_ans == null) {
        self.packrat_memo[entry_idx] = .{ .kind = .fail, .payload = 0 };
        return null;
    }
    const first_end: u32 = @intCast(pos + (input.len - first_ans.?.rest.len));
    self.packrat_memo[entry_idx] = .{ .kind = .success, .payload = first_end };
    return self.growLr(rule_id, pos, head_idx, input, depth);
}

/// Warth's GROW-LR: iteratively re-run the head's body. Each
/// iteration resets eval_set = involved_set so all participants get
/// re-evaluated exactly once; memo success is consulted otherwise.
fn growLr(
    self: *const Matcher,
    rule_id: u32,
    pos: u32,
    head_idx: u32,
    input: []const u8,
    depth: usize,
) ?Result {
    const m: *Matcher = @constCast(self);
    const entry_idx = rule_id * self.memo_stride + pos;
    m.heads[pos] = head_idx;

    while (true) {
        // Reset eval_set = involved_set for this iteration.
        const h = &m.heads_pool.items[head_idx];
        var it = h.involved.iterator(.{});
        h.eval.setRangeValue(.{ .start = 0, .end = h.eval.bit_length }, false);
        while (it.next()) |bit| h.eval.set(bit);

        m.rule_body_entries += 1;
        const ans = self.matchNode(self.rules[rule_id].node, input, depth + 1);
        if (ans == null) break;
        const end: u32 = @intCast(pos + (input.len - ans.?.rest.len));
        const cur = self.packrat_memo[entry_idx];
        const cur_end: u32 = if (cur.kind == .success) cur.payload else pos;
        if (end <= cur_end) break;
        self.packrat_memo[entry_idx] = .{ .kind = .success, .payload = end };
    }

    m.heads[pos] = null;

    const final = self.packrat_memo[entry_idx];
    if (final.kind == .success) return self.resultFrom(pos, final.payload);
    return null;
}

fn asciiLowerBuf(s: []const u8, buf: *[256]u8) ?[]const u8 {
    if (s.len > buf.len) return null;
    for (s, 0..) |c, i| {
        buf[i] = std.ascii.toLower(c);
    }
    return buf[0..s.len];
}

fn matchCoreRule(name: []const u8, input: []const u8) ?Result {
    // Special cases: multi-byte or always-true rules.
    if (std.ascii.eqlIgnoreCase("CRLF", name)) {
        if (input.len >= 2 and input[0] == 0x0D and input[1] == 0x0A)
            return .{ .value = input[0..2], .rest = input[2..] };
        return null;
    }
    if (std.ascii.eqlIgnoreCase("LWSP", name)) {
        // *(WSP / CRLF WSP) — zero or more.
        var rest = input;
        while (rest.len > 0) {
            if (rest[0] == 0x20 or rest[0] == 0x09) {
                rest = rest[1..];
            } else if (rest.len >= 3 and rest[0] == 0x0D and rest[1] == 0x0A and
                (rest[2] == 0x20 or rest[2] == 0x09))
            {
                rest = rest[3..];
            } else break;
        }
        return .{ .value = input[0 .. input.len - rest.len], .rest = rest };
    }
    if (std.ascii.eqlIgnoreCase("OCTET", name)) {
        if (input.len == 0) return null;
        return .{ .value = input[0..1], .rest = input[1..] };
    }

    // Single-byte predicate rules — table-driven dispatch.
    const pred_rules = comptime .{
        .{ "ALPHA", std.ascii.isAlphabetic },
        .{ "BIT", isBit },
        .{ "CHAR", isChar },
        .{ "CR", matchExact(0x0D) },
        .{ "LF", matchExact(0x0A) },
        .{ "CTL", isCtl },
        .{ "DIGIT", std.ascii.isDigit },
        .{ "DQUOTE", matchExact(0x22) },
        .{ "HEXDIG", std.ascii.isHex },
        .{ "HTAB", matchExact(0x09) },
        .{ "SP", matchExact(0x20) },
        .{ "VCHAR", isVchar },
        .{ "WSP", isWsp },
    };

    inline for (pred_rules) |entry| {
        if (std.ascii.eqlIgnoreCase(entry[0], name))
            return matchPred(input, entry[1]);
    }
    return null;
}

fn matchPred(input: []const u8, comptime pred: *const fn (u8) bool) ?Result {
    if (input.len == 0) return null;
    if (pred(input[0])) return .{ .value = input[0..1], .rest = input[1..] };
    return null;
}

fn matchExact(comptime expected: u8) *const fn (u8) bool {
    return struct {
        fn f(c: u8) bool {
            return c == expected;
        }
    }.f;
}

fn isBit(c: u8) bool {
    return c == '0' or c == '1';
}

fn isChar(c: u8) bool {
    return c >= 0x01 and c <= 0x7F;
}

fn isCtl(c: u8) bool {
    return c <= 0x1F or c == 0x7F;
}

fn isVchar(c: u8) bool {
    return c >= 0x21 and c <= 0x7E;
}

fn isWsp(c: u8) bool {
    return c == 0x20 or c == 0x09;
}

const Scanner = @import("abnf/Scanner.zig");
const Parser = @import("abnf/Parser.zig");
const Validator = @import("Validator.zig");

/// Parse an ABNF grammar, validate it, and return a Matcher ready to use.
fn compileMatcher(allocator: std.mem.Allocator, grammar: []const u8) !struct { matcher: Matcher, arena: std.heap.ArenaAllocator } {
    var scanner = Scanner.init(grammar);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, grammar);
    const rules = try parser.parse();
    std.debug.assert(parser.getDiagnostics().len == 0);
    var arena = std.heap.ArenaAllocator.init(allocator);
    var validator = Validator.init(arena.allocator(), rules);
    const merged = try validator.validate();
    return .{ .matcher = Matcher.init(arena.allocator(), merged), .arena = arena };
}

test "single char_val rule (case-insensitive)" {
    var ctx = try compileMatcher(std.testing.allocator, "greeting = \"hello\"");
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("greeting", "Hello world").?;
    try std.testing.expectEqualStrings("Hello", r.value);
    try std.testing.expectEqualStrings(" world", r.rest);
}

test "case-sensitive string" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\foo = %s"Hello"
    );
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "Hello") != null);
    try std.testing.expect(ctx.matcher.match("foo", "hello") == null);
}

test "numeric range" {
    var ctx = try compileMatcher(std.testing.allocator, "upper = %x41-5A");
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("upper", "Abc").?;
    try std.testing.expectEqualStrings("A", r.value);
    try std.testing.expectEqualStrings("bc", r.rest);
    try std.testing.expect(ctx.matcher.match("upper", "abc") == null);
}

test "numeric single" {
    var ctx = try compileMatcher(std.testing.allocator, "at = %x40");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("at", "@") != null);
    try std.testing.expect(ctx.matcher.match("at", "A") == null);
}

test "numeric concat" {
    var ctx = try compileMatcher(std.testing.allocator, "ab = %x41.42");
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("ab", "ABcd").?;
    try std.testing.expectEqualStrings("AB", r.value);
    try std.testing.expectEqualStrings("cd", r.rest);
}

test "alternation" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\bit = "0" / "1"
    );
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("bit", "0") != null);
    try std.testing.expect(ctx.matcher.match("bit", "1") != null);
    try std.testing.expect(ctx.matcher.match("bit", "2") == null);
}

test "concatenation" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\pair = %x41 %x42
    );
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("pair", "ABcd").?;
    try std.testing.expectEqualStrings("AB", r.value);
}

test "repetition" {
    var ctx = try compileMatcher(std.testing.allocator, "digits = 1*DIGIT");
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("digits", "123abc").?;
    try std.testing.expectEqualStrings("123", r.value);
    try std.testing.expectEqualStrings("abc", r.rest);
}

test "repetition bounded" {
    var ctx = try compileMatcher(std.testing.allocator, "two = 2*3DIGIT");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("two", "1") == null);
    const r = ctx.matcher.match("two", "123456").?;
    try std.testing.expectEqualStrings("123", r.value);
}

test "option (optional)" {
    var ctx = try compileMatcher(std.testing.allocator, "maybe = [DIGIT]");
    defer ctx.arena.deinit();
    const r1 = ctx.matcher.match("maybe", "5abc").?;
    try std.testing.expectEqualStrings("5", r1.value);
    const r2 = ctx.matcher.match("maybe", "abc").?;
    try std.testing.expectEqualStrings("", r2.value);
}

test "multi-rule grammar" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\number = 1*DIGIT
        \\pair = number "," number
    );
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("pair", "42,7!").?;
    try std.testing.expectEqualStrings("42,7", r.value);
    try std.testing.expectEqualStrings("!", r.rest);
}

test "core rule: ALPHA" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = ALPHA");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "A") != null);
    try std.testing.expect(ctx.matcher.match("foo", "z") != null);
    try std.testing.expect(ctx.matcher.match("foo", "5") == null);
}

test "core rule: DIGIT" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = DIGIT");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "0") != null);
    try std.testing.expect(ctx.matcher.match("foo", "9") != null);
    try std.testing.expect(ctx.matcher.match("foo", "a") == null);
}

test "core rule: WSP" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = WSP");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", " ") != null);
    try std.testing.expect(ctx.matcher.match("foo", "\t") != null);
    try std.testing.expect(ctx.matcher.match("foo", "a") == null);
}

test "incremental alternation (=/)" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\foo = "a"
        \\foo =/ "b"
    );
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "a") != null);
    try std.testing.expect(ctx.matcher.match("foo", "b") != null);
    try std.testing.expect(ctx.matcher.match("foo", "c") == null);
}

test "case-insensitive rule name lookup" {
    var ctx = try compileMatcher(std.testing.allocator, "Foo = digit");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "5") != null);
}

test "group with alternation" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\foo = ("a" / "b") "c"
    );
    defer ctx.arena.deinit();
    const r1 = ctx.matcher.match("foo", "ac").?;
    try std.testing.expectEqualStrings("ac", r1.value);
    const r2 = ctx.matcher.match("foo", "bc").?;
    try std.testing.expectEqualStrings("bc", r2.value);
    try std.testing.expect(ctx.matcher.match("foo", "cc") == null);
}

test "HTTP version" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\version = "HTTP/" 1*DIGIT "." 1*DIGIT
    );
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("version", "HTTP/1.1 OK").?;
    try std.testing.expectEqualStrings("HTTP/1.1", r.value);
    try std.testing.expectEqualStrings(" OK", r.rest);
}

test "pair" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\number = 1*DIGIT
        \\pair   = number "," number
    );
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("pair", "42,7!").?;
    try std.testing.expectEqualStrings("42,7", r.value);
    try std.testing.expectEqualStrings("!", r.rest);
}

test "undefined rule returns null" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = \"a\"");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("nonexistent", "a") == null);
}

test "empty input" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = \"a\"");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "") == null);
}

test "repetition star (zero or more)" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = *DIGIT");
    defer ctx.arena.deinit();
    const r1 = ctx.matcher.match("foo", "abc").?;
    try std.testing.expectEqualStrings("", r1.value);
    const r2 = ctx.matcher.match("foo", "123abc").?;
    try std.testing.expectEqualStrings("123", r2.value);
}

test "packrat: same result as plain match" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\number = 1*DIGIT
        \\pair   = number "," number
    );
    defer ctx.arena.deinit();
    const r_plain = ctx.matcher.match("pair", "42,7!").?;
    const r_memo = (try ctx.matcher.matchPackrat(std.testing.allocator, "pair", "42,7!")).?;
    try std.testing.expectEqualStrings(r_plain.value, r_memo.value);
    try std.testing.expectEqualStrings(r_plain.rest, r_memo.rest);
    try std.testing.expectEqualStrings("42,7", r_memo.value);
}

test "packrat: redundant rule re-entry is cached" {
    // `s` tries the first alternative; `e` matches at position 0 but
    // "!" fails, so s backtracks to the second alternative which calls
    // e at position 0 again. Without packrat, e is re-evaluated; with
    // packrat, the second call is a table hit.
    var ctx = try compileMatcher(std.testing.allocator,
        \\s = (e "!") / (e "?")
        \\e = "a" "b" "c" "d"
    );
    defer ctx.arena.deinit();

    const plain = ctx.matcher.match("s", "abcd?").?;
    const plain_descents = ctx.matcher.rule_body_entries;

    const memo = (try ctx.matcher.matchPackrat(std.testing.allocator, "s", "abcd?")).?;
    const memo_descents = ctx.matcher.rule_body_entries;

    try std.testing.expectEqualStrings("abcd?", plain.value);
    try std.testing.expectEqualStrings("abcd?", memo.value);
    // Without packrat, `e` is entered twice (once per `s` alternative).
    // With packrat, it is entered once; the second call is a memo hit.
    try std.testing.expect(memo_descents < plain_descents);
    try std.testing.expect(ctx.matcher.memo_hits > 0);
}

test "packrat: failure memoization" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\s = (e "x") / (e "y")
        \\e = "a" "b" "c"
    );
    defer ctx.arena.deinit();
    try std.testing.expect((try ctx.matcher.matchPackrat(std.testing.allocator, "s", "abqy")) == null);
    try std.testing.expect((try ctx.matcher.matchPackrat(std.testing.allocator, "s", "abcy")) != null);
}

test "packrat: cached success preserves value/rest" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\s = (w "!") / (w "?")
        \\w = "hello"
    );
    defer ctx.arena.deinit();
    const r = (try ctx.matcher.matchPackrat(std.testing.allocator, "s", "hello?")).?;
    try std.testing.expectEqualStrings("hello?", r.value);
    try std.testing.expectEqualStrings("", r.rest);
}

test "packrat: direct left recursion (single digit)" {
    // Minimal left-recursive grammar for left-associative addition.
    // Without Warth's seed-growing, this would infinite-recurse; with
    // it, the first iteration seeds on the base case and each grow
    // step extends the match by one "+DIGIT" suffix.
    var ctx = try compileMatcher(std.testing.allocator,
        \\expr = expr "+" DIGIT / DIGIT
    );
    defer ctx.arena.deinit();
    const r = (try ctx.matcher.matchPackrat(std.testing.allocator, "expr", "1")).?;
    try std.testing.expectEqualStrings("1", r.value);
    try std.testing.expectEqualStrings("", r.rest);
}

test "packrat: direct left recursion (grows across input)" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\expr = expr "+" DIGIT / DIGIT
    );
    defer ctx.arena.deinit();
    const r = (try ctx.matcher.matchPackrat(std.testing.allocator, "expr", "1+2+3")).?;
    try std.testing.expectEqualStrings("1+2+3", r.value);
    try std.testing.expectEqualStrings("", r.rest);
}

test "packrat: direct left recursion (stops at non-matching suffix)" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\expr = expr "+" DIGIT / DIGIT
    );
    defer ctx.arena.deinit();
    const r = (try ctx.matcher.matchPackrat(std.testing.allocator, "expr", "1+2+x")).?;
    try std.testing.expectEqualStrings("1+2", r.value);
    try std.testing.expectEqualStrings("+x", r.rest);
}

test "packrat: left recursion with two operators" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\expr = expr "+" DIGIT / expr "-" DIGIT / DIGIT
    );
    defer ctx.arena.deinit();
    const r = (try ctx.matcher.matchPackrat(std.testing.allocator, "expr", "1+2-3+4")).?;
    try std.testing.expectEqualStrings("1+2-3+4", r.value);
}

test "packrat: left recursion that never matches returns null" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\expr = expr "+" DIGIT / DIGIT
    );
    defer ctx.arena.deinit();
    try std.testing.expect((try ctx.matcher.matchPackrat(std.testing.allocator, "expr", "x")) == null);
}

test "packrat: indirect left recursion through two rules" {
    // a -> b -> a "x" / "y". Warth's involved/eval sets should
    // include both a and b in the cycle so each grow iteration
    // re-evaluates b with the new seed of a.
    var ctx = try compileMatcher(std.testing.allocator,
        \\a = b
        \\b = a "x" / "y"
    );
    defer ctx.arena.deinit();
    const r = (try ctx.matcher.matchPackrat(std.testing.allocator, "a", "yxx")).?;
    try std.testing.expectEqualStrings("yxx", r.value);
    try std.testing.expectEqualStrings("", r.rest);
}

test "packrat: participant memo is not stale after grow" {
    // After `a` grows via the indirect LR cycle a -> b -> a "x" / "y",
    // memo[b, 0] is last written by the iteration that DID NOT grow
    // (it took the short "y" branch). If that stale value leaks out
    // to a post-cycle query of `b` at position 0, the match is wrong.
    //
    // top tries alt 1, which fails after `a` grows successfully;
    // backtracking to alt 2 queries `b` at position 0. Correct
    // answer: b also matches the full "yxx" via LR growing. A stale
    // memo would let b only match "y".
    var ctx = try compileMatcher(std.testing.allocator,
        \\top = (a "qqqq") / b
        \\a   = b
        \\b   = (a "x") / "y"
    );
    defer ctx.arena.deinit();
    const r = (try ctx.matcher.matchPackrat(std.testing.allocator, "top", "yxx")).?;
    try std.testing.expectEqualStrings("yxx", r.value);
}

test "packrat: indirect LR with three rules in cycle" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\a = b
        \\b = c
        \\c = a "x" / "y"
    );
    defer ctx.arena.deinit();
    const r = (try ctx.matcher.matchPackrat(std.testing.allocator, "a", "yxxx")).?;
    try std.testing.expectEqualStrings("yxxx", r.value);
}
