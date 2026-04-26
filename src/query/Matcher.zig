/// Runs a compiled `Query` against a `CaptureTree.Tree` and yields
/// matches with their captures.
///
/// Algorithm: depth-first walk over the tree; at each node, try each
/// top-level pattern; emit a `Match` per successful (pattern, node) pair.
/// Pattern matching against a node body uses recursive backtracking
/// over child sequences with greedy `*`/`+` and gap-allowed positional
/// matching; `.` removes the gap before the next pattern.
const std = @import("std");
const ast = @import("Ast.zig");
const CaptureTree = @import("../vm/CaptureTree.zig");
const vm_mod = @import("../vm/Vm.zig");
const PegScanner = @import("../peg/Scanner.zig").Scanner;
const PegParser = @import("../peg/Parser.zig").Parser;
const VmCompiler = @import("../vm/Compiler.zig").Compiler;

pub const Capture = struct {
    name_id: u16,
    node: *const CaptureTree.Node,
};

pub const Match = struct {
    /// Index into `Query.patterns`.
    pattern_id: u16,
    /// Captures bound during this match. Order is the order they were
    /// bound during traversal -- not necessarily textual order.
    captures: []const Capture,
};

pub const Error = std.mem.Allocator.Error;

/// Runs `query` against `tree` and returns all matches, ordered by
/// (DFS visit order of the matched node, pattern index). The matches
/// and their capture arrays are owned by the cursor's arena.
pub const Cursor = struct {
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    matches: []const Match,
    pos: usize = 0,

    pub fn init(
        allocator: std.mem.Allocator,
        query: *const ast.Query,
        tree: *const CaptureTree.Tree,
        input: []const u8,
    ) Error!Cursor {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();

        var ctx = MatchCtx{
            .arena_alloc = arena.allocator(),
            .scratch_alloc = allocator,
            .input = input,
        };
        defer ctx.scratch_captures.deinit(allocator);
        defer ctx.matches.deinit(arena.allocator());

        for (tree.roots) |*root| try walkAndMatch(&ctx, query, root);

        const owned = try arena.allocator().dupe(Match, ctx.matches.items);
        return .{
            .allocator = allocator,
            .arena = arena,
            .matches = owned,
        };
    }

    pub fn deinit(self: *Cursor) void {
        self.arena.deinit();
    }

    pub fn next(self: *Cursor) ?Match {
        if (self.pos >= self.matches.len) return null;
        const m = self.matches[self.pos];
        self.pos += 1;
        return m;
    }
};

const MatchCtx = struct {
    /// Owns persisted match results and their capture arrays.
    arena_alloc: std.mem.Allocator,
    /// Backs transient buffers (scratch_captures, per-`*`-site save
    /// stacks). Cleared between top-level match attempts.
    scratch_alloc: std.mem.Allocator,
    input: []const u8,
    matches: std.ArrayListUnmanaged(Match) = .empty,
    scratch_captures: std.ArrayListUnmanaged(Capture) = .empty,
};

fn walkAndMatch(
    ctx: *MatchCtx,
    query: *const ast.Query,
    node: *const CaptureTree.Node,
) Error!void {
    // Top-level patterns are tried once per visited node. The outer
    // `pattern.quantifier` is only consulted by `matchPatternInSeq` when
    // matching a child sequence, so a quantifier on a top-level pattern
    // (e.g. `(Term)*`) is effectively a no-op -- it behaves the same as
    // `(Term)`. This mirrors tree-sitter's behavior.
    for (query.patterns, 0..) |pattern, idx| {
        ctx.scratch_captures.clearRetainingCapacity();
        if (try matchPattern(ctx, pattern, node)) {
            const captures = try ctx.arena_alloc.dupe(Capture, ctx.scratch_captures.items);
            try ctx.matches.append(ctx.arena_alloc, .{
                .pattern_id = @intCast(idx),
                .captures = captures,
            });
        }
    }
    for (node.children) |*child| try walkAndMatch(ctx, query, child);
}

fn matchPattern(
    ctx: *MatchCtx,
    pattern: ast.Pattern,
    node: *const CaptureTree.Node,
) Error!bool {
    const matched = switch (pattern.body) {
        .node => |np| try matchNode(ctx, np, node),
        .alt => |alts| blk: {
            for (alts) |alt| {
                const save = ctx.scratch_captures.items.len;
                if (try matchPattern(ctx, alt, node)) break :blk true;
                ctx.scratch_captures.shrinkRetainingCapacity(save);
            }
            break :blk false;
        },
        .group => |gp| blk: {
            // Tier-1 limitation: predicates are evaluated *after* the
            // inner match returns success; they don't trigger backtracking
            // within complex inners (e.g. `+` quantifiers). The dominant
            // use case `((Rule) @cap (#pred? @cap ...))` has no internal
            // backtracking dimension, so this is fine in practice.
            if (try matchPattern(ctx, gp.inner.*, node)) {
                if (runPredicates(ctx, gp.predicates)) break :blk true;
            }
            break :blk false;
        },
    };
    if (matched) {
        if (pattern.capture) |cap_id| {
            try ctx.scratch_captures.append(ctx.scratch_alloc, .{
                .name_id = cap_id,
                .node = node,
            });
        }
    }
    return matched;
}

fn matchNode(
    ctx: *MatchCtx,
    np: ast.NodePattern,
    node: *const CaptureTree.Node,
) Error!bool {
    if (!kindMatches(np, node)) return false;
    // Predicates run at the success base case of the child sequence so
    // that a failing predicate triggers backtracking into matchChildSeq
    // (which can then try a different child binding for `@captures`
    // referenced in the predicate).
    //
    // Capture-binding order: the *outer* `pattern.capture` on the
    // wrapping Pattern is bound by `matchPattern` only after we return,
    // so node-level predicates here cannot reference their own outer
    // `@capture` -- only captures bound by children. Use the grouping
    // form `((Foo) @x (#pred? @x ...))` if you need to reference a
    // capture on the matched node itself.
    return matchChildSeq(ctx, np.children, 0, node.children, 0, false, np.predicates);
}

fn kindMatches(np: ast.NodePattern, node: *const CaptureTree.Node) bool {
    switch (np.kind) {
        .any => return true,
        .rule_named => |id| {
            if (node.kind != .rule and node.kind != .rule_partial) return false;
            if (node.group_id != id) return false;
            if (np.partial) |want_partial| {
                if (want_partial and node.kind != .rule_partial) return false;
                if (!want_partial and node.kind != .rule) return false;
            }
            return true;
        },
        .error_kind => return node.kind == .error_node,
        .missing_kind => return node.kind == .missing_node,
    }
}

/// Match `patterns[pi..]` against `actuals[ai..]`. `anchored` means the
/// next non-anchor pattern must match at exactly position `ai` (no gap).
/// `predicates` are evaluated at the success base case so that a failing
/// predicate causes backtracking into the most recent alternative.
fn matchChildSeq(
    ctx: *MatchCtx,
    patterns: []const ast.Child,
    pi: usize,
    actuals: []const CaptureTree.Node,
    ai: usize,
    anchored: bool,
    predicates: []const ast.Predicate,
) Error!bool {
    var local_pi = pi;
    var local_anchored = anchored;
    while (local_pi < patterns.len) {
        switch (patterns[local_pi]) {
            .anchor => {
                // Leading anchor (pi == 0 with ai > 0) cannot be satisfied;
                // mid-sequence anchor sets the strict-adjacency flag for
                // the next pattern.
                if (local_pi == 0 and ai != 0) return false;
                local_anchored = true;
                local_pi += 1;
            },
            .pattern => |pat| {
                return matchPatternInSeq(
                    ctx,
                    pat,
                    patterns,
                    local_pi + 1,
                    actuals,
                    ai,
                    local_anchored,
                    predicates,
                );
            },
        }
    }
    // End of pattern list: trailing `.` requires that we've consumed
    // every actual child. Predicates apply to the fully-bound match.
    if (local_anchored and ai != actuals.len) return false;
    return runPredicates(ctx, predicates);
}

fn matchPatternInSeq(
    ctx: *MatchCtx,
    pat: ast.Pattern,
    patterns: []const ast.Child,
    next_pi: usize,
    actuals: []const CaptureTree.Node,
    ai: usize,
    anchored: bool,
    predicates: []const ast.Predicate,
) Error!bool {
    switch (pat.quantifier) {
        .one => return matchOneInSeq(ctx, pat, patterns, next_pi, actuals, ai, anchored, false, predicates),
        .optional => return matchOneInSeq(ctx, pat, patterns, next_pi, actuals, ai, anchored, true, predicates),
        .zero_or_more, .one_or_more => {
            const min: usize = if (pat.quantifier == .one_or_more) 1 else 0;
            // Try each starting position. With `anchored`, only `ai` is allowed.
            const start_max = if (anchored) ai else actuals.len;
            var start: usize = ai;
            while (start <= start_max) : (start += 1) {
                if (try matchRun(ctx, pat, min, actuals, start, patterns, next_pi, predicates)) return true;
            }
            return false;
        },
    }
}

/// Helper for `.one` and `.optional`. `optional_skip = true` means a
/// zero-match (skip this pattern) is also permitted.
fn matchOneInSeq(
    ctx: *MatchCtx,
    pat: ast.Pattern,
    patterns: []const ast.Child,
    next_pi: usize,
    actuals: []const CaptureTree.Node,
    ai: usize,
    anchored: bool,
    optional_skip: bool,
    predicates: []const ast.Predicate,
) Error!bool {
    const end = if (anchored) @min(ai + 1, actuals.len) else actuals.len;
    var try_ai = ai;
    while (try_ai < end) : (try_ai += 1) {
        const save = ctx.scratch_captures.items.len;
        if (try matchPattern(ctx, pat, &actuals[try_ai])) {
            if (try matchChildSeq(ctx, patterns, next_pi, actuals, try_ai + 1, false, predicates)) return true;
        }
        ctx.scratch_captures.shrinkRetainingCapacity(save);
    }
    if (optional_skip) {
        // Skip this pattern without consuming. `anchored` is not propagated:
        // an anchor is "consumed" by the optional even when it doesn't match.
        return matchChildSeq(ctx, patterns, next_pi, actuals, ai, false, predicates);
    }
    return false;
}

/// Match a `*`/`+` repetition consecutively from `start_ai`, then try
/// the rest of the sequence with counts from greedy down to `min`.
fn matchRun(
    ctx: *MatchCtx,
    pat: ast.Pattern,
    min: usize,
    actuals: []const CaptureTree.Node,
    start_ai: usize,
    patterns: []const ast.Child,
    next_pi: usize,
    predicates: []const ast.Predicate,
) Error!bool {
    var saves: std.ArrayListUnmanaged(usize) = .empty;
    defer saves.deinit(ctx.scratch_alloc);

    while (start_ai + saves.items.len < actuals.len) {
        const save = ctx.scratch_captures.items.len;
        if (try matchPattern(ctx, pat, &actuals[start_ai + saves.items.len])) {
            try saves.append(ctx.scratch_alloc, save);
        } else {
            ctx.scratch_captures.shrinkRetainingCapacity(save);
            break;
        }
    }

    var count = saves.items.len;
    while (true) {
        if (count >= min) {
            if (try matchChildSeq(ctx, patterns, next_pi, actuals, start_ai + count, false, predicates)) return true;
        }
        if (count == 0) break;
        count -= 1;
        ctx.scratch_captures.shrinkRetainingCapacity(saves.items[count]);
    }
    return false;
}

fn runPredicates(ctx: *MatchCtx, predicates: []const ast.Predicate) bool {
    for (predicates) |pred| {
        if (!evalPredicate(ctx, pred)) return false;
    }
    return true;
}

fn evalPredicate(ctx: *MatchCtx, pred: ast.Predicate) bool {
    // `pred.suffix` (`?` query vs `!` directive) is parsed but not acted
    // on here: every builtin is a pure query. The distinction matters
    // only for side-effecting directives (none yet), so `!` and `?` are
    // currently equivalent.
    if (std.mem.eql(u8, pred.name, "eq")) return evalEq(ctx, pred, false);
    if (std.mem.eql(u8, pred.name, "not-eq")) return evalEq(ctx, pred, true);
    if (std.mem.eql(u8, pred.name, "match")) return evalMatch(ctx, pred, false);
    if (std.mem.eql(u8, pred.name, "not-match")) return evalMatch(ctx, pred, true);
    // Unknown predicate: tree-sitter passes through unknown predicates so
    // queries written for richer matchers degrade gracefully.
    return true;
}

fn evalEq(ctx: *MatchCtx, pred: ast.Predicate, negate: bool) bool {
    if (pred.args.len != 2) return false;
    const left = argText(ctx, pred.args[0]) orelse return false;
    const right = argText(ctx, pred.args[1]) orelse return false;
    const eq = std.mem.eql(u8, left, right);
    return if (negate) !eq else eq;
}

/// Run a precompiled regex against the captured text using substring
/// (unanchored) semantics: try executing the bytecode at every starting
/// offset of the captured text; if any succeeds, the regex matched.
fn evalMatch(ctx: *MatchCtx, pred: ast.Predicate, negate: bool) bool {
    const compiled = pred.compiled_regex orelse return false;
    if (pred.args.len < 1) return false;
    const text = argText(ctx, pred.args[0]) orelse return false;
    const matched = regexMatchesAnywhere(compiled, text) catch return false;
    return if (negate) !matched else matched;
}

fn regexMatchesAnywhere(compiled: ast.CompiledRegex, text: []const u8) !bool {
    var start: usize = 0;
    while (start <= text.len) : (start += 1) {
        var v = vm_mod.Vm.init(compiled.code, compiled.charsets, compiled.string_data, text[start..]);
        if ((try v.execute()) != null) return true;
    }
    return false;
}

fn argText(ctx: *MatchCtx, arg: ast.Arg) ?[]const u8 {
    switch (arg) {
        .string => |s| return s,
        .capture => |idx| {
            // Most-recent binding wins; this matches tree-sitter's behavior
            // when the same name is captured by multiple sibling patterns.
            var i = ctx.scratch_captures.items.len;
            while (i > 0) {
                i -= 1;
                if (ctx.scratch_captures.items[i].name_id == idx) {
                    const node = ctx.scratch_captures.items[i].node;
                    return ctx.input[node.span.start..node.span.end];
                }
            }
            return null;
        },
    }
}

const testing = std.testing;
const parser = @import("Parser.zig");

const calc_names: CaptureTree.Names = .{ .rules = &.{ "Expr", "Term", "Factor" } };

/// Build a small `(Expr (Term (Factor)) (Term (Factor) (Factor)))` tree
/// matching what `examples/calc.peg "1+2*3"` would produce.
fn buildCalcTree() !CaptureTree.Tree {
    const events = [_]CaptureTree.Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } }, // Expr
        .{ .open = .{ .group_id = 1, .pos = 0 } }, //   Term
        .{ .open = .{ .group_id = 2, .pos = 0 } }, //     Factor "1"
        .{ .close = .{ .group_id = 2, .pos = 1 } },
        .{ .close = .{ .group_id = 1, .pos = 1 } },
        .{ .open = .{ .group_id = 1, .pos = 2 } }, //   Term
        .{ .open = .{ .group_id = 2, .pos = 2 } }, //     Factor "2"
        .{ .close = .{ .group_id = 2, .pos = 3 } },
        .{ .open = .{ .group_id = 2, .pos = 4 } }, //     Factor "3"
        .{ .close = .{ .group_id = 2, .pos = 5 } },
        .{ .close = .{ .group_id = 1, .pos = 5 } },
        .{ .close = .{ .group_id = 0, .pos = 5 } },
    };
    return try CaptureTree.buildFromEvents(testing.allocator, &events);
}

test "matches every Term in a tree" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator, "(Term)", calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    var count: usize = 0;
    while (cursor.next()) |m| {
        try testing.expectEqual(@as(u16, 0), m.pattern_id);
        count += 1;
    }
    try testing.expectEqual(@as(usize, 2), count);
}

test "captures the matched node" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator, "(Term) @t", calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    const m1 = cursor.next().?;
    try testing.expectEqual(@as(usize, 1), m1.captures.len);
    try testing.expectEqual(@as(u16, 1), m1.captures[0].node.group_id);
    try testing.expectEqual(@as(u32, 0), m1.captures[0].node.span.start);

    const m2 = cursor.next().?;
    try testing.expectEqual(@as(u32, 2), m2.captures[0].node.span.start);
    try testing.expect(cursor.next() == null);
}

test "captures inner child by structure" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\(Term (Factor) @f)
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    var count: usize = 0;
    while (cursor.next()) |m| {
        try testing.expectEqual(@as(usize, 1), m.captures.len);
        try testing.expectEqual(@as(u16, 2), m.captures[0].node.group_id);
        count += 1;
    }
    // Both Terms have at least one Factor child, so both match.
    try testing.expectEqual(@as(usize, 2), count);
}

test "alternation matches either alternative" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\[(Term) (Factor)] @x
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    var count: usize = 0;
    while (cursor.next()) |_| count += 1;
    // 2 Terms + 3 Factors.
    try testing.expectEqual(@as(usize, 5), count);
}

test "+ quantifier matches consecutive children" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    // Match a Term that has two-or-more consecutive Factor children.
    var query = try parser.compile(testing.allocator,
        \\(Term (Factor)+ @f)
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    var count: usize = 0;
    var last_capture_node: ?*const CaptureTree.Node = null;
    while (cursor.next()) |m| {
        try testing.expect(m.captures.len >= 1);
        last_capture_node = m.captures[m.captures.len - 1].node;
        count += 1;
    }
    // Both Terms match: first Term has 1 Factor (run of 1), second has 2.
    try testing.expectEqual(@as(usize, 2), count);
    // The greedy `+` on the second Term captures the LAST repetition (Factor at [4,5]).
    try testing.expectEqual(@as(u32, 4), last_capture_node.?.span.start);
}

test "anchor enforces first-child position" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    // `(Expr . (Term) @t)` requires the captured Term be the first child of Expr.
    var query = try parser.compile(testing.allocator,
        \\(Expr . (Term) @t)
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqual(@as(u32, 0), m.captures[0].node.span.start);
    try testing.expect(cursor.next() == null);
}

test "predicate #eq? filters by captured text" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\(Term (Factor) @f (#eq? @f "2"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    const m = cursor.next().?;
    // Only the Term containing "2" matches.
    try testing.expectEqual(@as(u32, 2), m.captures[0].node.span.start);
    try testing.expectEqualStrings("2", "1+2*3"[m.captures[0].node.span.start..m.captures[0].node.span.end]);
    try testing.expect(cursor.next() == null);
}

test "predicate #not-eq? filters out matches" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\(Term (Factor) @f (#not-eq? @f "2"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    var count: usize = 0;
    while (cursor.next()) |m| {
        const text = "1+2*3"[m.captures[0].node.span.start..m.captures[0].node.span.end];
        try testing.expect(!std.mem.eql(u8, text, "2"));
        count += 1;
    }
    // First Term's Factor is "1"; second Term has Factors "2" and "3".
    // Backtracking: matcher first tries "2" -> rejected; then "3" -> accepted.
    try testing.expectEqual(@as(usize, 2), count);
}

test "wildcard (_) matches any node" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator, "(_)", .{}, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    var count: usize = 0;
    while (cursor.next()) |_| count += 1;
    // 1 Expr + 2 Terms + 3 Factors = 6 nodes.
    try testing.expectEqual(@as(usize, 6), count);
}

test "ERROR head matches synthesized error nodes" {
    const events = [_]CaptureTree.Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .error_open = .{ .group_id = 1, .pos = 1 } },
        .{ .error_close = .{ .group_id = 1, .pos = 3 } },
        .{ .close = .{ .group_id = 0, .pos = 3 } },
    };
    var tree = try CaptureTree.buildFromEvents(testing.allocator, &events);
    defer tree.deinit();
    var query = try parser.compile(testing.allocator, "(ERROR) @e", .{ .rules = &.{"Stmt"} }, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "abcd");
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqual(@as(u32, 1), m.captures[0].node.span.start);
    try testing.expectEqual(@as(u32, 3), m.captures[0].node.span.end);
    try testing.expect(cursor.next() == null);
}

test "partial modifier filters to rule_partial nodes" {
    // Tree: (Stmt rule [0,1]) (Stmt partial [2,4]).
    const events = [_]CaptureTree.Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .close = .{ .group_id = 0, .pos = 1 } },
        .{ .open = .{ .group_id = 0, .pos = 2 } },
        .{ .partial_close = .{ .group_id = 0, .pos = 4 } },
    };
    var tree = try CaptureTree.buildFromEvents(testing.allocator, &events);
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\(Stmt partial) @p
    , .{ .rules = &.{"Stmt"} }, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "abcd");
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqual(@as(u32, 2), m.captures[0].node.span.start);
    try testing.expect(cursor.next() == null);
}

test "grouping scopes predicate to a captured pattern" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\((Term) @t (#eq? @t "1"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqual(@as(u32, 0), m.captures[0].node.span.start);
    try testing.expectEqual(@as(u32, 1), m.captures[0].node.span.end);
    try testing.expect(cursor.next() == null);
}

test "predicate #match? filters by regex against captured text" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    // Match Terms whose Factor child is a digit -- all should match.
    var query = try parser.compile(testing.allocator,
        \\((Term (Factor) @f) (#match? @f "[0-9]"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    var count: usize = 0;
    while (cursor.next()) |_| count += 1;
    try testing.expectEqual(@as(usize, 2), count);
}

test "predicate #match? rejects non-matching captures" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    // No Factor in the input "1+2*3" matches a letter.
    var query = try parser.compile(testing.allocator,
        \\((Factor) @f (#match? @f "[a-z]"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();
    try testing.expect(cursor.next() == null);
}

test "predicate #not-match? inverts" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\((Factor) @f (#not-match? @f "2"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    var count: usize = 0;
    while (cursor.next()) |m| {
        const text = "1+2*3"[m.captures[0].node.span.start..m.captures[0].node.span.end];
        try testing.expect(!std.mem.eql(u8, text, "2"));
        count += 1;
    }
    try testing.expectEqual(@as(usize, 2), count);
}

test "end-to-end: real calc.peg grammar through PEG parser + VM + matcher" {
    const grammar =
        \\Expr   <- Term ("+" Term)*
        \\Term   <- Factor ("*" Factor)*
        \\Factor <- "(" Expr ")" / [0-9]+
    ;
    const input = "1+2*3";

    var scanner = PegScanner.init(grammar);
    const tokens = scanner.scanTokens();
    var pp = PegParser.init(tokens, grammar);
    const rules = try pp.parse();
    const diags = pp.getDiagnostics();
    try testing.expectEqual(@as(usize, 0), diags.len);

    var compiler = try VmCompiler.compileOpts(rules, .{ .rules_as_captures = true });
    const EventVm = vm_mod.VmWith(.{ .capture_events = true });
    var vm = EventVm.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        input,
    );
    defer vm.deinit();
    try testing.expect((try vm.execute()) != null);

    var tree = try vm.buildCaptureTree(testing.allocator);
    defer tree.deinit();

    // Resolve the names produced by the real compiler -- order isn't
    // guaranteed to match our synthetic tests, so look them up.
    var names_buf: [16][]const u8 = undefined;
    const rule_names = names_buf[0..compiler.rule_count];
    for (0..compiler.rule_count) |i| rule_names[i] = compiler.getRuleName(@intCast(i));
    const names: CaptureTree.Names = .{ .rules = rule_names };

    // Query: capture every Factor node and verify we found three
    // (the digits 1, 2, 3 in "1+2*3").
    var query = try parser.compile(testing.allocator, "(Factor) @f", names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, input);
    defer cursor.deinit();

    var texts: [4][]const u8 = undefined;
    var n: usize = 0;
    while (cursor.next()) |m| : (n += 1) {
        const span = m.captures[0].node.span;
        texts[n] = input[span.start..span.end];
    }
    try testing.expectEqual(@as(usize, 3), n);
    try testing.expectEqualStrings("1", texts[0]);
    try testing.expectEqualStrings("2", texts[1]);
    try testing.expectEqualStrings("3", texts[2]);

    // And a regex predicate filtering to only odd digits.
    var odd_query = try parser.compile(testing.allocator,
        \\((Factor) @f (#match? @f "[13579]"))
    , names, null);
    defer odd_query.deinit();

    var odd_cursor = try Cursor.init(testing.allocator, odd_query, &tree, input);
    defer odd_cursor.deinit();

    var odd_count: usize = 0;
    while (odd_cursor.next()) |m| : (odd_count += 1) {
        const text = input[m.captures[0].node.span.start..m.captures[0].node.span.end];
        try testing.expect(text[0] == '1' or text[0] == '3');
    }
    try testing.expectEqual(@as(usize, 2), odd_count);
}

test "no matches yields empty cursor" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    // ERROR head against a clean tree.
    var query = try parser.compile(testing.allocator, "(ERROR)", .{}, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();
    try testing.expect(cursor.next() == null);
}
