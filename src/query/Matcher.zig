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
            // Two shapes, distinguished by structure:
            //   Single .pattern child, no anchors -> "predicate-scoping"
            //   wrapper: match the inner against the visited node, with
            //   the group's predicates folded into the inner's own
            //   matchChildSeq (so a failing predicate triggers
            //   backtracking through quantified inner children).
            //   Otherwise (multi-pattern, or contains an anchor) ->
            //   sibling-sequence match against the visited node's
            //   children. Predicates fire at the success base case via
            //   the standard matchChildSeq plumbing.
            if (isPredicateScopingGroup(gp)) {
                break :blk try matchScopingGroup(ctx, gp, node);
            }
            break :blk try matchChildSeq(ctx, gp.children, 0, node.children, 0, false, gp.predicates);
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

/// True when a `GroupPattern` is the predicate-scoping shape: exactly
/// one inner child, which is a plain `.pattern` (not an anchor and not
/// field-tagged). Anything else -- multi-pattern, contains an anchor,
/// or a field-tagged child -- is a sibling-sequence pattern over the
/// visited node's children.
fn isPredicateScopingGroup(gp: ast.GroupPattern) bool {
    if (gp.children.len != 1) return false;
    return gp.children[0] == .pattern;
}

/// Match a single-inner ("predicate-scoping") grouping against `node`.
/// The outer pattern's `@capture` is bound speculatively before the
/// inner runs, so the merged predicate set can reference it. When the
/// inner is a node pattern, the group's predicates are folded into the
/// inner's `matchChildSeq` predicates -- a failing predicate then
/// triggers backtracking through any quantified inner children, fixing
/// the case where a greedy `*`/`+` bound a capture the predicate then
/// rejected. For non-node inners (alt / nested group), we fall back to
/// the simpler "match then check" path.
fn matchScopingGroup(
    ctx: *MatchCtx,
    gp: ast.GroupPattern,
    node: *const CaptureTree.Node,
) Error!bool {
    const inner = gp.children[0].pattern;
    const save = ctx.scratch_captures.items.len;

    if (inner.body == .node) {
        const np = inner.body.node;
        if (!kindMatches(ctx, np, node)) return false;
        // Bind the outer capture up front so the merged predicates can
        // reference it (this is the only @cap they could plausibly read
        // since predicates run before matchPattern's normal capture-on-
        // success path).
        if (inner.capture) |cap_id| {
            try ctx.scratch_captures.append(ctx.scratch_alloc, .{
                .name_id = cap_id,
                .node = node,
            });
        }
        // Merge the inner's own predicates with the group's predicates.
        // Single small heap alloc per attempt; bounded by the number of
        // predicates in the query.
        var merged: std.ArrayListUnmanaged(ast.Predicate) = .empty;
        defer merged.deinit(ctx.scratch_alloc);
        try merged.appendSlice(ctx.scratch_alloc, np.predicates);
        try merged.appendSlice(ctx.scratch_alloc, gp.predicates);
        const ok = try matchChildSeq(ctx, np.children, 0, node.children, 0, false, merged.items);
        if (!ok) ctx.scratch_captures.shrinkRetainingCapacity(save);
        return ok;
    }

    // Non-node inner: match-then-check (no inner backtracking dimension
    // for predicates to push against here).
    if (try matchPattern(ctx, inner, node)) {
        if (runPredicates(ctx, gp.predicates)) return true;
        ctx.scratch_captures.shrinkRetainingCapacity(save);
    }
    return false;
}

fn matchNode(
    ctx: *MatchCtx,
    np: ast.NodePattern,
    node: *const CaptureTree.Node,
) Error!bool {
    if (!kindMatches(ctx, np, node)) return false;
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

fn kindMatches(ctx: *const MatchCtx, np: ast.NodePattern, node: *const CaptureTree.Node) bool {
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
        .token_text => |literal| {
            if (node.kind != .token) return false;
            const text = ctx.input[node.span.start..node.span.end];
            return std.mem.eql(u8, text, literal);
        },
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
                    null,
                );
            },
            .field_pattern => |fp| {
                return matchPatternInSeq(
                    ctx,
                    fp.pattern,
                    patterns,
                    local_pi + 1,
                    actuals,
                    ai,
                    local_anchored,
                    predicates,
                    fp.field_id,
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
    required_field: ?u16,
) Error!bool {
    switch (pat.quantifier) {
        .one => return matchOneInSeq(ctx, pat, patterns, next_pi, actuals, ai, anchored, false, predicates, required_field),
        .optional => return matchOneInSeq(ctx, pat, patterns, next_pi, actuals, ai, anchored, true, predicates, required_field),
        .zero_or_more, .one_or_more => {
            const min: usize = if (pat.quantifier == .one_or_more) 1 else 0;
            // Try each starting position. With `anchored`, only `ai` is allowed.
            const start_max = if (anchored) ai else actuals.len;
            var start: usize = ai;
            while (start <= start_max) : (start += 1) {
                if (try matchRun(ctx, pat, min, actuals, start, patterns, next_pi, predicates, required_field)) return true;
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
    required_field: ?u16,
) Error!bool {
    const end = if (anchored) @min(ai + 1, actuals.len) else actuals.len;
    var try_ai = ai;
    while (try_ai < end) : (try_ai += 1) {
        if (required_field) |fid| {
            const node_field = actuals[try_ai].field orelse continue;
            if (node_field != fid) continue;
        }
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
    required_field: ?u16,
) Error!bool {
    var saves: std.ArrayListUnmanaged(usize) = .empty;
    defer saves.deinit(ctx.scratch_alloc);

    while (start_ai + saves.items.len < actuals.len) {
        const idx = start_ai + saves.items.len;
        if (required_field) |fid| {
            const node_field = actuals[idx].field orelse break;
            if (node_field != fid) break;
        }
        const save = ctx.scratch_captures.items.len;
        if (try matchPattern(ctx, pat, &actuals[idx])) {
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
    // `#match?`, `#vim-match?`, `#lua-match?` all dispatch to the same
    // ERE engine. The Vim and Lua flavors aren't ERE-equivalent in their
    // metacharacters, but for the simple ASCII patterns most highlight
    // queries actually use (`[A-Z][a-zA-Z0-9_]*`, `^_`, etc.) the three
    // flavors agree -- aliasing them lets common nvim-treesitter `.scm`
    // files load without per-engine rewrites. Patterns that depend on
    // a specific flavor's quirks will misbehave.
    if (std.mem.eql(u8, pred.name, "match") or
        std.mem.eql(u8, pred.name, "vim-match") or
        std.mem.eql(u8, pred.name, "lua-match")) return evalMatch(ctx, pred, false);
    if (std.mem.eql(u8, pred.name, "not-match") or
        std.mem.eql(u8, pred.name, "not-vim-match") or
        std.mem.eql(u8, pred.name, "not-lua-match")) return evalMatch(ctx, pred, true);
    if (std.mem.eql(u8, pred.name, "any-of")) return evalAnyOf(ctx, pred, false);
    if (std.mem.eql(u8, pred.name, "not-any-of")) return evalAnyOf(ctx, pred, true);
    if (std.mem.eql(u8, pred.name, "contains")) return evalContains(ctx, pred, false);
    if (std.mem.eql(u8, pred.name, "not-contains")) return evalContains(ctx, pred, true);
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

/// `(#any-of? @cap "a" "b" "c")` -- the captured text equals one of the
/// literal string args. Capture must be the first arg; remaining args
/// must all be strings. With no string args, no value can match -- so
/// `any-of` is false (and `not-any-of` is true).
fn evalAnyOf(ctx: *MatchCtx, pred: ast.Predicate, negate: bool) bool {
    if (pred.args.len < 1) return false;
    const text = argText(ctx, pred.args[0]) orelse return false;
    var matched = false;
    for (pred.args[1..]) |arg| {
        const lit = switch (arg) {
            .string => |s| s,
            else => return false, // ill-formed: non-string after the capture
        };
        if (std.mem.eql(u8, text, lit)) {
            matched = true;
            break;
        }
    }
    return if (negate) !matched else matched;
}

/// `(#contains? @cap "needle")` -- true when the captured text contains
/// the literal `needle` as a substring. Multiple needles are ANDed (the
/// capture must contain *every* needle), matching tree-sitter behavior.
fn evalContains(ctx: *MatchCtx, pred: ast.Predicate, negate: bool) bool {
    if (pred.args.len < 2) return false;
    const text = argText(ctx, pred.args[0]) orelse return false;
    var all_present = true;
    for (pred.args[1..]) |arg| {
        const needle = switch (arg) {
            .string => |s| s,
            else => return false,
        };
        if (std.mem.indexOf(u8, text, needle) == null) {
            all_present = false;
            break;
        }
    }
    return if (negate) !all_present else all_present;
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

test "predicate #vim-match? aliases to ERE #match?" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\((Factor) @f (#vim-match? @f "[13579]"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    var count: usize = 0;
    while (cursor.next()) |_| count += 1;
    try testing.expectEqual(@as(usize, 2), count); // "1" and "3"
}

test "single-inner grouping predicate backtracks through quantified inner" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    // Without backtracking, the greedy `(Factor)+` would bind @f to the
    // last Factor of each Term and the predicate `(#eq? @f "2")` would
    // reject Term [2,5]'s greedy bind ("3"). With backtracking through
    // matchChildSeq, the inner retries with shorter counts and finds
    // the run ending at "2".
    var query = try parser.compile(testing.allocator,
        \\((Term (Factor)+ @f) (#eq? @f "2"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    const m = cursor.next().?;
    // The capture should bind to the Factor with text "2", not "3".
    try testing.expectEqualStrings("2", "1+2*3"[m.captures[0].node.span.start..m.captures[0].node.span.end]);
    try testing.expect(cursor.next() == null);
}

test "predicate #any-of? matches captures whose text is in the list" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\((Factor) @f (#any-of? @f "1" "3"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    var seen: [3][]const u8 = undefined;
    var n: usize = 0;
    while (cursor.next()) |m| : (n += 1) {
        const span = m.captures[0].node.span;
        seen[n] = "1+2*3"[span.start..span.end];
    }
    try testing.expectEqual(@as(usize, 2), n);
    try testing.expectEqualStrings("1", seen[0]);
    try testing.expectEqualStrings("3", seen[1]);
}

test "predicate #not-any-of? excludes the listed captures" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\((Factor) @f (#not-any-of? @f "1" "3"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqualStrings("2", "1+2*3"[m.captures[0].node.span.start..m.captures[0].node.span.end]);
    try testing.expect(cursor.next() == null);
}

test "predicate #contains? finds substring in captured text" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    // Capture every Term; "2*3" contains "*", "1" doesn't.
    var query = try parser.compile(testing.allocator,
        \\((Term) @t (#contains? @t "*"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqualStrings("2*3", "1+2*3"[m.captures[0].node.span.start..m.captures[0].node.span.end]);
    try testing.expect(cursor.next() == null);
}

test "predicate #contains? with multiple needles ANDs them" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    // The Expr text "1+2*3" contains both "+" and "*".
    var query = try parser.compile(testing.allocator,
        \\((Expr) @e (#contains? @e "+" "*"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();
    try testing.expect(cursor.next() != null);
    try testing.expect(cursor.next() == null);
}

test "predicate #not-contains? rejects captures with the substring" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\((Term) @t (#not-contains? @t "*"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqualStrings("1", "1+2*3"[m.captures[0].node.span.start..m.captures[0].node.span.end]);
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

test "anonymous-token query: matches keyword via real PEG + tagged tokens" {
    const grammar =
        \\#@ tokens "function" "if"
        \\Stmt   <- "function" Ident / "if" Ident
        \\Ident  <- [a-z]+
    ;
    const input = "functionfoo";

    const RP = @import("../peg/Parser.zig").Parser;
    var scanner = PegScanner.init(grammar);
    const tokens = scanner.scanTokens();
    var pp = @import("../peg/Parser.zig").ParserWith(.{ .recovery = true }).init(tokens, grammar);
    const rules = try pp.parse();
    try testing.expectEqual(@as(usize, 0), pp.getDiagnostics().len);
    _ = RP;

    const tagged = pp.getTaggedTokens();
    try testing.expectEqual(@as(usize, 2), tagged.len);

    var compiler = try VmCompiler.compileOpts(rules, .{
        .rules_as_captures = true,
        .token_events = .tagged,
        .tagged_tokens = tagged,
    });
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

    var names_buf: [16][]const u8 = undefined;
    const rule_names = names_buf[0..compiler.rule_count];
    for (0..compiler.rule_count) |i| rule_names[i] = compiler.getRuleName(@intCast(i));
    const names: CaptureTree.Names = .{ .rules = rule_names };

    // Query: capture the parent Stmt of any "function" token.
    var query = try parser.compile(testing.allocator,
        \\(Stmt "function") @s
    , names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, input);
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqualStrings("functionfoo", input[m.captures[0].node.span.start..m.captures[0].node.span.end]);
    try testing.expect(cursor.next() == null);
}

test "multi-pattern grouping matches a sibling sequence under any parent" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    // The second Term in calc tree has two Factor children; this group
    // matches that Term as the parent and binds both factors.
    var query = try parser.compile(testing.allocator,
        \\((Factor) @a (Factor) @b)
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqual(@as(usize, 2), m.captures.len);
    try testing.expectEqualStrings("2", "1+2*3"[m.captures[0].node.span.start..m.captures[0].node.span.end]);
    try testing.expectEqualStrings("3", "1+2*3"[m.captures[1].node.span.start..m.captures[1].node.span.end]);
    // No other parent has two consecutive Factor children.
    try testing.expect(cursor.next() == null);
}

test "multi-pattern grouping with anchor enforces strict adjacency" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    // `.` between two Factors requires they be adjacent siblings -- which
    // they are inside the second Term, so this still matches.
    var query = try parser.compile(testing.allocator,
        \\((Factor) @a . (Factor) @b)
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqual(@as(usize, 2), m.captures.len);
    try testing.expect(cursor.next() == null);
}

test "multi-pattern grouping with predicate filters captured siblings" {
    var tree = try buildCalcTree();
    defer tree.deinit();
    var query = try parser.compile(testing.allocator,
        \\((Factor) @a (Factor) @b (#eq? @a "2"))
    , calc_names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, "1+2*3");
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqualStrings("2", "1+2*3"[m.captures[0].node.span.start..m.captures[0].node.span.end]);
    try testing.expect(cursor.next() == null);
}

test "anonymous-token query: token_events=.all emits a leaf for every literal" {
    const grammar =
        \\Expr <- Term ("+" Term)*
        \\Term <- [0-9]+
    ;
    const input = "1+2";

    var scanner = PegScanner.init(grammar);
    const tokens = scanner.scanTokens();
    var pp = PegParser.init(tokens, grammar);
    const rules = try pp.parse();

    var compiler = try VmCompiler.compileOpts(rules, .{
        .rules_as_captures = true,
        .token_events = .all,
    });
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

    var names_buf: [16][]const u8 = undefined;
    const rule_names = names_buf[0..compiler.rule_count];
    for (0..compiler.rule_count) |i| rule_names[i] = compiler.getRuleName(@intCast(i));
    const names: CaptureTree.Names = .{ .rules = rule_names };

    var query = try parser.compile(testing.allocator,
        \\"+" @plus
    , names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, input);
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqualStrings("+", input[m.captures[0].node.span.start..m.captures[0].node.span.end]);
    try testing.expect(cursor.next() == null);
}

test "field selector matches only field-tagged children" {
    const grammar =
        \\#@ tokens "function"
        \\#@ field Func name = Ident
        \\#@ field Func body = Body
        \\Func  <- "function" _ Ident _ Body
        \\Ident <- [a-z]+
        \\Body  <- "{" _ "}"
        \\_     <- " "*
    ;
    const input = "function foo{}";

    const RP = @import("../peg/Parser.zig").ParserWith(.{ .recovery = true });
    var scanner = PegScanner.init(grammar);
    const tokens = scanner.scanTokens();
    var pp = RP.init(tokens, grammar);
    const rules = try pp.parse();
    try testing.expectEqual(@as(usize, 0), pp.getDiagnostics().len);

    var compiler = try VmCompiler.compileOpts(rules, .{
        .rules_as_captures = true,
        .field_events = true,
        .token_events = .tagged,
        .tagged_tokens = pp.getTaggedTokens(),
    });
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

    var rname_buf: [16][]const u8 = undefined;
    const rule_names = rname_buf[0..compiler.rule_count];
    for (0..compiler.rule_count) |i| rule_names[i] = compiler.getRuleName(@intCast(i));
    var fname_buf: [16][]const u8 = undefined;
    const field_names = fname_buf[0..compiler.field_count];
    for (0..compiler.field_count) |i| field_names[i] = compiler.getFieldName(@intCast(i));
    const names: CaptureTree.Names = .{ .rules = rule_names, .fields = field_names };

    // `name: (Ident)` should match the Ident field child but NOT the Body child.
    var query = try parser.compile(testing.allocator,
        \\(Func name: (Ident) @n)
    , names, null);
    defer query.deinit();

    var cursor = try Cursor.init(testing.allocator, query, &tree, input);
    defer cursor.deinit();

    const m = cursor.next().?;
    try testing.expectEqualStrings("foo", input[m.captures[0].node.span.start..m.captures[0].node.span.end]);
    try testing.expect(cursor.next() == null);

    // `body: (Body)` should match the Body child captured as @b.
    var body_query = try parser.compile(testing.allocator,
        \\(Func body: (Body) @b)
    , names, null);
    defer body_query.deinit();
    var body_cursor = try Cursor.init(testing.allocator, body_query, &tree, input);
    defer body_cursor.deinit();
    const bm = body_cursor.next().?;
    try testing.expectEqualStrings("{}", input[bm.captures[0].node.span.start..bm.captures[0].node.span.end]);

    // `body: (Ident)` should NOT match -- the Ident child has field=name, not body.
    var miss_query = try parser.compile(testing.allocator,
        \\(Func body: (Ident) @n)
    , names, null);
    defer miss_query.deinit();
    var miss_cursor = try Cursor.init(testing.allocator, miss_query, &tree, input);
    defer miss_cursor.deinit();
    try testing.expect(miss_cursor.next() == null);
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
