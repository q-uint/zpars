/// Parses a tree-sitter-style query source into a compiled `ast.Query`.
///
/// Grammar (informal):
///
///   query     := pattern*
///   pattern   := atom quantifier? capture?
///   atom      := "(" head child* ")"            -- node pattern
///              | "[" pattern+ "]"                -- alternation
///              | "_"                              -- bare wildcard
///   head      := identifier ("partial")?         -- includes "_"/"ERROR"/"MISSING"
///   child     := pattern
///              | "."                              -- sibling anchor
///              | predicate
///   predicate := "(" "#name?" arg* ")"
///   arg       := "@name" | string
///   quantifier:= "?" | "*" | "+"
///   capture   := "@" identifier
///
/// Rule names (e.g. `Term`) are resolved against the `Names` table at
/// parse time and stored as numeric `rule_id`s in the AST so the matcher
/// can do u16 comparisons. Capture names are interned into a per-query
/// table; both `@x` bindings and `@x` predicate arguments index into it.
const std = @import("std");
const ast = @import("Ast.zig");
const Token = @import("Token.zig").Token;
const Scanner = @import("Scanner.zig").Scanner;
const CaptureTree = @import("../vm/CaptureTree.zig");
const EreScanner = @import("../ere/Scanner.zig").Scanner;
const EreParser = @import("../ere/Parser.zig").Parser;
const vm_compiler = @import("../vm/Compiler.zig");

pub const Error = error{
    /// Source did not tokenize / parse.
    InvalidQuery,
    /// Query referenced a rule name that's not in the provided `Names` table.
    UnknownRule,
} || std.mem.Allocator.Error;

pub const Diagnostic = struct {
    /// Human-readable message. Lifetime: borrows from the query source for
    /// names; static for fixed messages. Caller should copy if needed past
    /// the lifetime of the query source.
    message: []const u8 = "",
    /// 1-based line of the offending token. Zero if not available.
    line: usize = 0,
};

/// Compile a query source against the given grammar `Names` table.
/// Returns a heap-allocated `Query` owned through an arena; caller calls
/// `query.deinit()` to release everything.
///
/// On error, fills `diag` (if non-null) with a brief description and
/// returns `error.InvalidQuery` or `error.UnknownRule`.
pub fn compile(
    allocator: std.mem.Allocator,
    source: []const u8,
    names: CaptureTree.Names,
    diag: ?*Diagnostic,
) Error!*ast.Query {
    var scanner = Scanner.init(source);
    defer scanner.deinit(allocator);
    const tokens = try scanner.scanTokens(allocator);

    const arena = try allocator.create(std.heap.ArenaAllocator);
    arena.* = std.heap.ArenaAllocator.init(allocator);
    errdefer {
        arena.deinit();
        allocator.destroy(arena);
    }

    var p = Parser{
        .arena_alloc = arena.allocator(),
        .scratch = allocator,
        .source = source,
        .tokens = tokens,
        .names = names,
        .diag = diag,
    };
    defer p.captures.deinit(p.scratch);
    defer p.capture_list.deinit(p.scratch);

    var top: std.ArrayList(ast.Pattern) = .empty;
    defer top.deinit(p.scratch);
    while (p.peek().tag != .eof) {
        if (p.peek().tag == .comment) {
            p.advance();
            continue;
        }
        const pat = try p.parsePattern();
        try top.append(p.scratch, pat);
    }

    const out = try allocator.create(ast.Query);
    errdefer allocator.destroy(out);
    out.* = .{
        .arena = arena,
        .patterns = try p.arena_alloc.dupe(ast.Pattern, top.items),
        .capture_names = try p.arena_alloc.dupe([]const u8, p.capture_list.items),
    };
    return out;
}

const Parser = struct {
    /// Arena that backs the resulting `Query` -- everything reachable
    /// from the returned `*Query` lives here.
    arena_alloc: std.mem.Allocator,
    /// Scratch allocator for transient ArrayLists during parsing. Items
    /// are copied into `arena_alloc` before being attached to the AST.
    scratch: std.mem.Allocator,
    source: []const u8,
    tokens: []const Token,
    names: CaptureTree.Names,
    pos: usize = 0,
    /// Capture-name interning. Maps name -> index in `capture_list`.
    captures: std.StringHashMapUnmanaged(u16) = .empty,
    capture_list: std.ArrayListUnmanaged([]const u8) = .empty,
    diag: ?*Diagnostic,

    fn peek(self: *const Parser) Token {
        return self.tokens[self.pos];
    }

    fn advance(self: *Parser) void {
        // .eof is the sentinel; never advance past it.
        if (self.tokens[self.pos].tag != .eof) self.pos += 1;
        // Skip comments transparently inside the parser body.
        while (self.tokens[self.pos].tag == .comment) self.pos += 1;
    }

    fn lex(self: *const Parser, tok: Token) []const u8 {
        return tok.lexeme(self.source);
    }

    fn fail(self: *Parser, msg: []const u8, e: Error) Error {
        if (self.diag) |d| d.* = .{ .message = msg, .line = self.peek().line };
        return e;
    }

    fn expect(self: *Parser, tag: Token.Tag, msg: []const u8) Error!Token {
        const t = self.peek();
        if (t.tag != tag) return self.fail(msg, error.InvalidQuery);
        self.advance();
        return t;
    }

    /// pattern := atom quantifier? capture?
    fn parsePattern(self: *Parser) Error!ast.Pattern {
        const body = try self.parseAtom();
        var pat = ast.Pattern{ .body = body };
        switch (self.peek().tag) {
            .question => {
                pat.quantifier = .optional;
                self.advance();
            },
            .star => {
                pat.quantifier = .zero_or_more;
                self.advance();
            },
            .plus => {
                pat.quantifier = .one_or_more;
                self.advance();
            },
            else => {},
        }
        if (self.peek().tag == .at_identifier) {
            const tok = self.peek();
            self.advance();
            // Strip the leading '@'.
            const name = self.lex(tok)[1..];
            pat.capture = try self.internCapture(name);
        }
        return pat;
    }

    /// atom := "(" head child* ")"          -- node pattern
    ///       | "(" pattern predicate* ")"    -- grouping (Tier-1: single inner pattern)
    ///       | "[" pattern+ "]"
    ///       | "_"
    fn parseAtom(self: *Parser) Error!ast.Pattern.Body {
        const t = self.peek();
        switch (t.tag) {
            .lparen => return self.parseLparenAtom(),
            .lbracket => return self.parseAlt(),
            .identifier => {
                if (std.mem.eql(u8, self.lex(t), "_")) {
                    self.advance();
                    return .{ .node = .{ .kind = .any } };
                }
                return self.fail("bare identifier outside parens", error.InvalidQuery);
            },
            else => return self.fail("expected pattern", error.InvalidQuery),
        }
    }

    /// Disambiguate between a node pattern `(Head ...)` and a grouping
    /// `((inner) ...)`. The disambiguator is the token immediately after
    /// `(`: an identifier (rule head, `_`, `ERROR`, `MISSING`) starts a
    /// node pattern; `(` or `[` starts a grouping wrapping a sub-pattern.
    fn parseLparenAtom(self: *Parser) Error!ast.Pattern.Body {
        if (self.pos + 1 >= self.tokens.len) {
            return self.fail("unexpected eof after '('", error.InvalidQuery);
        }
        const next = self.tokens[self.pos + 1];
        switch (next.tag) {
            .lparen, .lbracket => return self.parseGroup(),
            .identifier => return self.parseNodeAtom(),
            else => return self.fail("expected node head or grouped pattern after '('", error.InvalidQuery),
        }
    }

    /// group := "(" pattern predicate* ")"
    /// The capture/quantifier on the inner pattern apply normally; the
    /// group's predicates are evaluated after the inner match succeeds.
    fn parseGroup(self: *Parser) Error!ast.Pattern.Body {
        _ = try self.expect(.lparen, "expected '('");
        const inner_pat = try self.parsePattern();
        const inner_ptr = try self.arena_alloc.create(ast.Pattern);
        inner_ptr.* = inner_pat;

        var preds: std.ArrayListUnmanaged(ast.Predicate) = .empty;
        defer preds.deinit(self.scratch);
        while (self.peek().tag != .rparen and self.peek().tag != .eof) {
            if (self.peek().tag == .lparen and
                self.pos + 1 < self.tokens.len and
                self.tokens[self.pos + 1].tag == .predicate)
            {
                try preds.append(self.scratch, try self.parsePredicate());
            } else {
                return self.fail("group bodies are limited to one inner pattern plus predicates", error.InvalidQuery);
            }
        }
        _ = try self.expect(.rparen, "expected ')'");

        return .{ .group = .{
            .inner = inner_ptr,
            .predicates = try self.arena_alloc.dupe(ast.Predicate, preds.items),
        } };
    }

    /// `(` already pending. Reads head + body + `)`.
    fn parseNodeAtom(self: *Parser) Error!ast.Pattern.Body {
        _ = try self.expect(.lparen, "expected '('");
        const head_tok = self.peek();
        if (head_tok.tag != .identifier) {
            return self.fail("expected node-pattern head", error.InvalidQuery);
        }
        self.advance();
        const head = self.lex(head_tok);

        const kind: ast.NodeKindMatch = blk: {
            if (std.mem.eql(u8, head, "_")) break :blk .any;
            if (std.mem.eql(u8, head, "ERROR")) break :blk .error_kind;
            if (std.mem.eql(u8, head, "MISSING")) break :blk .missing_kind;
            const id = self.resolveRule(head) orelse {
                return self.fail("unknown rule name", error.UnknownRule);
            };
            break :blk .{ .rule_named = id };
        };

        var partial: ?bool = null;
        if (self.peek().tag == .identifier and
            std.mem.eql(u8, self.lex(self.peek()), "partial"))
        {
            // `partial` is only meaningful for rule heads (and `_`, which
            // also matches rules). Reject it on ERROR/MISSING so a typo
            // doesn't get silently accepted as a no-op.
            switch (kind) {
                .rule_named, .any => {},
                .error_kind, .missing_kind => return self.fail(
                    "'partial' modifier is only valid on rule or '_' heads",
                    error.InvalidQuery,
                ),
            }
            self.advance();
            partial = true;
        }

        var children: std.ArrayListUnmanaged(ast.Child) = .empty;
        defer children.deinit(self.scratch);
        var preds: std.ArrayListUnmanaged(ast.Predicate) = .empty;
        defer preds.deinit(self.scratch);

        while (self.peek().tag != .rparen and self.peek().tag != .eof) {
            switch (self.peek().tag) {
                .dot => {
                    self.advance();
                    try children.append(self.scratch, .anchor);
                },
                .lparen => {
                    // Disambiguate: `(#...)` is a predicate, anything else
                    // is a child pattern.
                    if (self.pos + 1 < self.tokens.len and
                        self.tokens[self.pos + 1].tag == .predicate)
                    {
                        try preds.append(self.scratch, try self.parsePredicate());
                    } else {
                        const child = try self.parsePattern();
                        try children.append(self.scratch, .{ .pattern = child });
                    }
                },
                .lbracket, .identifier => {
                    const child = try self.parsePattern();
                    try children.append(self.scratch, .{ .pattern = child });
                },
                else => return self.fail("unexpected token in node body", error.InvalidQuery),
            }
        }
        _ = try self.expect(.rparen, "expected ')'");

        return .{ .node = .{
            .kind = kind,
            .partial = partial,
            .children = try self.arena_alloc.dupe(ast.Child, children.items),
            .predicates = try self.arena_alloc.dupe(ast.Predicate, preds.items),
        } };
    }

    /// alt := "[" pattern+ "]"
    fn parseAlt(self: *Parser) Error!ast.Pattern.Body {
        _ = try self.expect(.lbracket, "expected '['");
        var alts: std.ArrayListUnmanaged(ast.Pattern) = .empty;
        defer alts.deinit(self.scratch);
        while (self.peek().tag != .rbracket and self.peek().tag != .eof) {
            try alts.append(self.scratch, try self.parsePattern());
        }
        _ = try self.expect(.rbracket, "expected ']'");
        if (alts.items.len == 0) return self.fail("empty alternation", error.InvalidQuery);
        return .{ .alt = try self.arena_alloc.dupe(ast.Pattern, alts.items) };
    }

    /// predicate := "(" "#name?"|"#name!" arg* ")"
    /// `(` already pending; head token is at `pos+1`.
    fn parsePredicate(self: *Parser) Error!ast.Predicate {
        _ = try self.expect(.lparen, "expected '('");
        const head = try self.expect(.predicate, "expected predicate head");
        const head_lex = self.lex(head); // includes leading '#' and trailing '?' or '!'.
        const suffix: ast.Predicate.Suffix = switch (head_lex[head_lex.len - 1]) {
            '?' => .question,
            '!' => .bang,
            else => return self.fail("predicate must end in '?' or '!'", error.InvalidQuery),
        };
        const name = head_lex[1 .. head_lex.len - 1];

        var args: std.ArrayListUnmanaged(ast.Arg) = .empty;
        defer args.deinit(self.scratch);
        while (self.peek().tag != .rparen and self.peek().tag != .eof) {
            const t = self.peek();
            switch (t.tag) {
                .at_identifier => {
                    self.advance();
                    const cap_name = self.lex(t)[1..];
                    const idx = try self.internCapture(cap_name);
                    try args.append(self.scratch, .{ .capture = idx });
                },
                .string => {
                    self.advance();
                    const decoded = try decodeString(self.arena_alloc, self.lex(t));
                    try args.append(self.scratch, .{ .string = decoded });
                },
                else => return self.fail("expected capture or string in predicate args", error.InvalidQuery),
            }
        }
        _ = try self.expect(.rparen, "expected ')' to close predicate");

        var pred = ast.Predicate{
            .name = try self.arena_alloc.dupe(u8, name),
            .suffix = suffix,
            .args = try self.arena_alloc.dupe(ast.Arg, args.items),
        };
        if (std.mem.eql(u8, name, "match") or std.mem.eql(u8, name, "not-match")) {
            pred.compiled_regex = try self.compileRegexPredicate(pred);
        }
        return pred;
    }

    /// Parse and compile the regex argument of a `#match?`/`#not-match?`
    /// predicate. Expects exactly two args: a capture and a string. The
    /// resulting bytecode is copied into the query arena so it survives
    /// the transient `vm.Compiler`.
    fn compileRegexPredicate(self: *Parser, pred: ast.Predicate) Error!ast.CompiledRegex {
        if (pred.args.len != 2) return self.fail("#match? takes (capture, regex)", error.InvalidQuery);
        if (pred.args[0] != .capture) return self.fail("#match? first arg must be a capture", error.InvalidQuery);
        const regex_src = switch (pred.args[1]) {
            .string => |s| s,
            else => return self.fail("#match? second arg must be a string", error.InvalidQuery),
        };

        var scanner = EreScanner.init(regex_src);
        const tokens = scanner.scanTokens();
        var parser = EreParser.init(tokens, regex_src);
        const rules = parser.parse() catch {
            return self.fail("invalid regex in #match?", error.InvalidQuery);
        };

        // The compiler is stack-allocated but large (>100KB). Heap it so
        // we don't blow the stack when many regex predicates are present.
        const compiler = self.scratch.create(vm_compiler.Compiler) catch return error.OutOfMemory;
        defer self.scratch.destroy(compiler);
        compiler.* = vm_compiler.Compiler.compile(rules) catch {
            return self.fail("regex compile failed in #match?", error.InvalidQuery);
        };

        return .{
            .code = try self.arena_alloc.dupe(@TypeOf(compiler.getCode()[0]), compiler.getCode()),
            .charsets = try self.arena_alloc.dupe(@TypeOf(compiler.getCharsets()[0]), compiler.getCharsets()),
            .string_data = try self.arena_alloc.dupe(u8, compiler.getStringData()),
        };
    }

    fn resolveRule(self: *const Parser, name: []const u8) ?u16 {
        for (self.names.rules, 0..) |n, i| {
            if (std.mem.eql(u8, n, name)) return @intCast(i);
        }
        return null;
    }

    fn internCapture(self: *Parser, name: []const u8) Error!u16 {
        if (self.captures.get(name)) |idx| return idx;
        const idx: u16 = @intCast(self.capture_list.items.len);
        // Copy into arena so the index table outlives the source if needed.
        const owned = try self.arena_alloc.dupe(u8, name);
        try self.capture_list.append(self.scratch, owned);
        try self.captures.put(self.scratch, owned, idx);
        return idx;
    }
};

/// Decode `"..."` lexeme (with surrounding quotes) into raw bytes,
/// honoring `\\`, `\"`, `\n`, `\t`, `\r`, `\0`, `\xHH`. Allocates from
/// the given allocator only when escapes are present; otherwise returns
/// a slice into the lexeme.
fn decodeString(allocator: std.mem.Allocator, lex: []const u8) ![]const u8 {
    std.debug.assert(lex.len >= 2 and lex[0] == '"' and lex[lex.len - 1] == '"');
    const inner = lex[1 .. lex.len - 1];
    if (std.mem.indexOfScalar(u8, inner, '\\') == null) return inner;

    var buf: std.ArrayListUnmanaged(u8) = .empty;
    errdefer buf.deinit(allocator);
    var i: usize = 0;
    while (i < inner.len) {
        const c = inner[i];
        if (c != '\\') {
            try buf.append(allocator, c);
            i += 1;
            continue;
        }
        if (i + 1 >= inner.len) return error.InvalidQuery;
        const esc = inner[i + 1];
        switch (esc) {
            'n' => try buf.append(allocator, '\n'),
            't' => try buf.append(allocator, '\t'),
            'r' => try buf.append(allocator, '\r'),
            '0' => try buf.append(allocator, 0),
            '\\' => try buf.append(allocator, '\\'),
            '"' => try buf.append(allocator, '"'),
            'x' => {
                if (i + 3 >= inner.len) return error.InvalidQuery;
                const hi = hexDigit(inner[i + 2]) orelse return error.InvalidQuery;
                const lo = hexDigit(inner[i + 3]) orelse return error.InvalidQuery;
                try buf.append(allocator, (hi << 4) | lo);
                i += 4;
                continue;
            },
            else => return error.InvalidQuery,
        }
        i += 2;
    }
    return try buf.toOwnedSlice(allocator);
}

fn hexDigit(c: u8) ?u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => null,
    };
}

const testing = std.testing;

const calc_names: CaptureTree.Names = .{
    .rules = &.{ "Expr", "Term", "Factor" },
};

test "compile single rule pattern" {
    var diag: Diagnostic = .{};
    var query = try compile(testing.allocator, "(Term)", calc_names, &diag);
    defer query.deinit();
    try testing.expectEqual(@as(usize, 1), query.patterns.len);
    const p = query.patterns[0];
    try testing.expectEqual(ast.Quantifier.one, p.quantifier);
    try testing.expect(p.capture == null);
    const node = p.body.node;
    try testing.expectEqual(@as(u16, 1), node.kind.rule_named);
    try testing.expectEqual(@as(usize, 0), node.children.len);
}

test "compile nested with capture" {
    var query = try compile(testing.allocator,
        \\(Expr (Term) @t)
    , calc_names, null);
    defer query.deinit();
    const outer = query.patterns[0].body.node;
    try testing.expectEqual(@as(u16, 0), outer.kind.rule_named);
    try testing.expectEqual(@as(usize, 1), outer.children.len);
    const inner = outer.children[0].pattern;
    try testing.expectEqual(@as(?u16, 0), inner.capture);
    try testing.expectEqualStrings("t", query.captureName(inner.capture.?));
}

test "compile alternation" {
    var query = try compile(testing.allocator,
        \\[(Term) (Factor)]
    , calc_names, null);
    defer query.deinit();
    const alt = query.patterns[0].body.alt;
    try testing.expectEqual(@as(usize, 2), alt.len);
    try testing.expectEqual(@as(u16, 1), alt[0].body.node.kind.rule_named);
    try testing.expectEqual(@as(u16, 2), alt[1].body.node.kind.rule_named);
}

test "compile quantifier and bare wildcard" {
    var query = try compile(testing.allocator, "(Expr (_)+)", calc_names, null);
    defer query.deinit();
    const expr = query.patterns[0].body.node;
    const child = expr.children[0].pattern;
    try testing.expectEqual(ast.Quantifier.one_or_more, child.quantifier);
    try testing.expectEqual(ast.NodeKindMatch.any, child.body.node.kind);
}

test "compile ERROR and MISSING heads" {
    var query = try compile(testing.allocator,
        \\(ERROR) (MISSING)
    , .{}, null);
    defer query.deinit();
    try testing.expectEqual(@as(usize, 2), query.patterns.len);
    try testing.expectEqual(ast.NodeKindMatch.error_kind, query.patterns[0].body.node.kind);
    try testing.expectEqual(ast.NodeKindMatch.missing_kind, query.patterns[1].body.node.kind);
}

test "compile partial modifier" {
    var query = try compile(testing.allocator, "(Expr partial)", calc_names, null);
    defer query.deinit();
    try testing.expectEqual(@as(?bool, true), query.patterns[0].body.node.partial);
}

test "compile rejects partial on ERROR/MISSING heads" {
    var diag: Diagnostic = .{};
    try testing.expectError(error.InvalidQuery, compile(testing.allocator, "(ERROR partial)", .{}, &diag));
    try testing.expectError(error.InvalidQuery, compile(testing.allocator, "(MISSING partial)", .{}, null));
}

test "compile predicate with capture and string args" {
    var query = try compile(testing.allocator,
        \\(Term (Factor) @f (#eq? @f "1"))
    , calc_names, null);
    defer query.deinit();
    const term = query.patterns[0].body.node;
    try testing.expectEqual(@as(usize, 1), term.predicates.len);
    const pred = term.predicates[0];
    try testing.expectEqualStrings("eq", pred.name);
    try testing.expectEqual(ast.Predicate.Suffix.question, pred.suffix);
    try testing.expectEqual(@as(usize, 2), pred.args.len);
    try testing.expectEqual(@as(u16, 0), pred.args[0].capture);
    try testing.expectEqualStrings("1", pred.args[1].string);
    // The predicate's @f references the same capture bound on the (Factor) child.
    const factor = term.children[0].pattern;
    try testing.expectEqual(factor.capture.?, pred.args[0].capture);
}

test "compile anchor in child list" {
    var query = try compile(testing.allocator,
        \\(Expr . (Term) (Term))
    , calc_names, null);
    defer query.deinit();
    const children = query.patterns[0].body.node.children;
    try testing.expectEqual(@as(usize, 3), children.len);
    try testing.expectEqual(@as(ast.Child, .anchor), children[0]);
    try testing.expect(children[1] == .pattern);
    try testing.expect(children[2] == .pattern);
}

test "compile string with escape" {
    var query = try compile(testing.allocator,
        \\(Term (Factor) @f (#eq? @f "a\nb"))
    , calc_names, null);
    defer query.deinit();
    const pred = query.patterns[0].body.node.predicates[0];
    try testing.expectEqualStrings("a\nb", pred.args[1].string);
}

test "compile rejects unknown rule" {
    var diag: Diagnostic = .{};
    const r = compile(testing.allocator, "(Unknown)", calc_names, &diag);
    try testing.expectError(error.UnknownRule, r);
    try testing.expectEqualStrings("unknown rule name", diag.message);
}

test "compile rejects bare identifier outside parens" {
    try testing.expectError(error.InvalidQuery, compile(testing.allocator, "Term", calc_names, null));
}

test "compile rejects empty alternation" {
    try testing.expectError(error.InvalidQuery, compile(testing.allocator, "[]", calc_names, null));
}

test "compile multiple top-level patterns" {
    var query = try compile(testing.allocator,
        \\(Term)
        \\(Factor)
    , calc_names, null);
    defer query.deinit();
    try testing.expectEqual(@as(usize, 2), query.patterns.len);
}

test "compile grouping with single inner and predicate" {
    var query = try compile(testing.allocator,
        \\((Term) @t (#eq? @t "1"))
    , calc_names, null);
    defer query.deinit();
    const group = query.patterns[0].body.group;
    try testing.expectEqual(@as(u16, 1), group.inner.body.node.kind.rule_named);
    try testing.expectEqual(@as(?u16, 0), group.inner.capture);
    try testing.expectEqual(@as(usize, 1), group.predicates.len);
    try testing.expectEqualStrings("eq", group.predicates[0].name);
}

test "compile grouping with no predicate" {
    var query = try compile(testing.allocator, "((Term))", calc_names, null);
    defer query.deinit();
    const group = query.patterns[0].body.group;
    try testing.expectEqual(@as(u16, 1), group.inner.body.node.kind.rule_named);
    try testing.expectEqual(@as(usize, 0), group.predicates.len);
}

test "compile rejects multi-pattern grouping (Tier-1 limitation)" {
    try testing.expectError(error.InvalidQuery, compile(testing.allocator,
        \\((Term) (Factor))
    , calc_names, null));
}

test "compile skips top-level comments" {
    var query = try compile(testing.allocator,
        \\; this is a comment
        \\(Term)
        \\; trailing comment
    , calc_names, null);
    defer query.deinit();
    try testing.expectEqual(@as(usize, 1), query.patterns.len);
}
