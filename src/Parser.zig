const std = @import("std");
const Token = @import("Token.zig");
const Ast = @import("Ast.zig");
const Diagnostic = @import("Diagnostic.zig");

const Parser = @This();

pub const ParseError = error{ SyntaxError, OutOfMemory, Overflow, InvalidCharacter };

tokens: []const Token,
source: []const u8,
allocator: std.mem.Allocator,
pos: usize,
diagnostics: std.ArrayList(Diagnostic) = .empty,

pub fn init(allocator: std.mem.Allocator, tokens: []const Token, source: []const u8) Parser {
    return .{
        .tokens = tokens,
        .source = source,
        .allocator = allocator,
        .pos = 0,
    };
}

/// Parse all rules from the token stream.
pub fn parse(self: *Parser) ![]const Ast.Rule {
    var rules: std.ArrayList(Ast.Rule) = .empty;

    self.skipTrivia();
    while (self.peek().tag != .eof) {
        const rule = self.parseRule() catch |err| switch (err) {
            error.SyntaxError => {
                self.synchronize();
                self.skipTrivia();
                continue;
            },
            else => return err,
        };

        try rules.append(self.allocator, rule);
        self.skipTrivia();
    }

    return try rules.toOwnedSlice(self.allocator);
}

// --- Grammar rules -----------------------------------------------------------

/// rule = rulename ("=" / "=/") alternation
fn parseRule(self: *Parser) !Ast.Rule {
    if (self.peek().tag != .rulename) {
        try self.fail(.rulename, self.peek());
        return error.SyntaxError;
    }
    const name = self.advance().lexeme(self.source);
    self.skipTrivia();
    var incremental = false;
    if (self.peek().tag == .equals or self.peek().tag == .equals_slash) {
        incremental = self.advance().tag == .equals_slash;
    }
    self.skipTrivia();
    return .{ .name = name, .node = try self.parseAlternation(), .incremental = incremental };
}

/// alternation = concatenation *("/" concatenation)
fn parseAlternation(self: *Parser) !Ast.Node {
    var alts: std.ArrayList(Ast.Node) = .empty;
    try alts.append(self.allocator, try self.parseConcatenation());

    while (true) {
        self.skipTrivia();
        if (self.peek().tag != .slash) break;
        _ = self.advance();
        self.skipTrivia();
        try alts.append(self.allocator, try self.parseConcatenation());
    }

    if (alts.items.len == 1) {
        const single = alts.items[0];
        alts.deinit(self.allocator);
        return single;
    }
    return .{ .alternation = try alts.toOwnedSlice(self.allocator) };
}

/// concatenation = repetition *(repetition)
fn parseConcatenation(self: *Parser) !Ast.Node {
    var elems: std.ArrayList(Ast.Node) = .empty;

    while (self.isAtRepetition()) {
        try elems.append(self.allocator, try self.parseRepetition());
        self.skipTrivia();
    }

    if (elems.items.len == 1) {
        const single = elems.items[0];
        elems.deinit(self.allocator);
        return single;
    }
    return .{ .concatenation = try elems.toOwnedSlice(self.allocator) };
}

/// repetition = [repeat] element
/// repeat     = 1*DIGIT / (*DIGIT "*" *DIGIT)
fn parseRepetition(self: *Parser) !Ast.Node {
    var min: usize = 0;
    var max: ?usize = null;
    var has_repeat = false;

    switch (self.peek().tag) {
        .number => {
            if (self.peekAt(1).tag == .star) {
                // number "*" [number]  — e.g. 3*5
                min = try self.parseNumber();
                _ = self.advance(); // consume *
                if (self.peek().tag == .number) max = try self.parseNumber();
            } else {
                // bare number — exactly N
                const n = try self.parseNumber();
                min = n;
                max = n;
            }
            has_repeat = true;
        },
        .star => {
            // "*" [number]  — e.g. *5
            _ = self.advance();
            if (self.peek().tag == .number) max = try self.parseNumber();
            has_repeat = true;
        },
        else => {},
    }

    const element = try self.parseElement();
    if (!has_repeat) return element;

    const ptr = try self.allocator.create(Ast.Node);
    ptr.* = element;
    return .{ .repetition = .{ .min = min, .max = max, .element = ptr } };
}

/// element = rulename / group / option / char-val / num-val / prose-val
fn parseElement(self: *Parser) ParseError!Ast.Node {
    return switch (self.peek().tag) {
        .rulename => .{ .rulename = self.advance().lexeme(self.source) },
        .char_val => {
            const lex = self.advance().lexeme(self.source);
            return .{ .char_val = .{ .value = lex[1 .. lex.len - 1], .case_sensitive = false } };
        },
        .char_val_ci => {
            const lex = self.advance().lexeme(self.source);
            // %i"..." — strip the leading %i and surrounding quotes.
            return .{ .char_val = .{ .value = lex[3 .. lex.len - 1], .case_sensitive = false } };
        },
        .char_val_cs => {
            const lex = self.advance().lexeme(self.source);
            // %s"..." — strip the leading %s and surrounding quotes.
            return .{ .char_val = .{ .value = lex[3 .. lex.len - 1], .case_sensitive = true } };
        },
        .bin_val, .dec_val, .hex_val => .{ .num_val = try self.parseNumVal() },
        .prose_val => {
            const lex = self.advance().lexeme(self.source);
            return .{ .prose_val = lex[1 .. lex.len - 1] };
        },
        .left_paren => try self.parseGroup(),
        .left_bracket => try self.parseOption(),
        else => {
            try self.fail(.element, self.peek());
            return error.SyntaxError;
        },
    };
}

/// group = "(" alternation ")"
fn parseGroup(self: *Parser) !Ast.Node {
    const open = self.advance();
    if (open.tag != .left_paren) {
        try self.fail(.left_paren, open);
        return error.SyntaxError;
    }
    self.skipTrivia();
    const node = try self.parseAlternation();
    self.skipTrivia();
    const close = self.advance();
    if (close.tag != .right_paren) {
        try self.fail(.right_paren, close);
        return error.SyntaxError;
    }
    return node;
}

/// option = "[" alternation "]"  →  *1( alternation )
fn parseOption(self: *Parser) !Ast.Node {
    const open = self.advance();
    if (open.tag != .left_bracket) {
        try self.fail(.left_bracket, open);
        return error.SyntaxError;
    }
    self.skipTrivia();
    const inner = try self.parseAlternation();
    self.skipTrivia();
    const close = self.advance();
    if (close.tag != .right_bracket) {
        try self.fail(.right_bracket, close);
        return error.SyntaxError;
    }

    const ptr = try self.allocator.create(Ast.Node);
    ptr.* = inner;
    return .{ .repetition = .{ .min = 0, .max = 1, .element = ptr } };
}

// --- Numeric values ----------------------------------------------------------

fn parseNumVal(self: *Parser) !Ast.NumVal {
    const lex = self.advance().lexeme(self.source);
    const base: u8 = switch (lex[1]) {
        'b' => 2,
        'd' => 10,
        'x' => 16,
        else => unreachable,
    };
    const digits = lex[2..];

    if (std.mem.indexOfScalar(u8, digits, '-')) |dash| {
        return .{ .range = .{
            .lo = try std.fmt.parseInt(u8, digits[0..dash], base),
            .hi = try std.fmt.parseInt(u8, digits[dash + 1 ..], base),
        } };
    }

    if (std.mem.indexOfScalar(u8, digits, '.')) |_| {
        var vals: std.ArrayList(u8) = .empty;
        var iter = std.mem.splitScalar(u8, digits, '.');
        while (iter.next()) |part| {
            try vals.append(self.allocator, try std.fmt.parseInt(u8, part, base));
        }
        return .{ .concat = try vals.toOwnedSlice(self.allocator) };
    }

    return .{ .single = try std.fmt.parseInt(u8, digits, base) };
}

// --- Diagnostics -------------------------------------------------------------

fn fail(self: *Parser, expected: Diagnostic.Expected, token: Token) error{OutOfMemory}!void {
    try self.diagnostics.append(self.allocator, .{
        .expected = expected,
        .found_tag = token.tag,
        .found_start = token.start,
        .found_len = token.len,
        .line = token.line,
    });
}

// --- Helpers -----------------------------------------------------------------

fn parseNumber(self: *Parser) !usize {
    return std.fmt.parseInt(usize, self.advance().lexeme(self.source), 10);
}

fn peek(self: *Parser) Token {
    return self.tokens[self.pos];
}

fn peekAt(self: *Parser, offset: usize) Token {
    const idx = self.pos + offset;
    if (idx >= self.tokens.len) return .{ .tag = .eof, .start = 0, .len = 0, .line = 0 };
    return self.tokens[idx];
}

fn advance(self: *Parser) Token {
    const tok = self.tokens[self.pos];
    self.pos += 1;
    return tok;
}

fn skipTrivia(self: *Parser) void {
    while (true) {
        switch (self.peek().tag) {
            .comment, .newline => self.pos += 1,
            else => break,
        }
    }
}

/// Skip tokens until the start of the next rule or EOF.
/// A rule boundary is a `rulename` followed by `=` or `=/`.
fn synchronize(self: *Parser) void {
    while (self.peek().tag != .eof) {
        if (self.peek().tag == .rulename) {
            const next = self.peekNextMeaningful();
            if (next == .equals or next == .equals_slash) return;
        }
        self.pos += 1;
    }
}

/// Can the current position start a repetition/element?
fn isAtRepetition(self: *Parser) bool {
    return switch (self.peek().tag) {
        .star,
        .number,
        .left_paren,
        .left_bracket,
        .char_val,
        .char_val_ci,
        .char_val_cs,
        .bin_val,
        .dec_val,
        .hex_val,
        .prose_val,
        => true,
        // A rulename followed by = or =/ starts a new rule, not an element.
        .rulename => {
            const next = self.peekNextMeaningful();
            return next != .equals and next != .equals_slash;
        },
        else => false,
    };
}

/// Next non-trivia token tag after the current position.
fn peekNextMeaningful(self: *Parser) Token.Tag {
    var i = self.pos + 1;
    while (i < self.tokens.len) : (i += 1) {
        const tag = self.tokens[i].tag;
        if (tag != .comment and tag != .newline) return tag;
    }
    return .eof;
}

// --- Tests -------------------------------------------------------------------

const Scanner = @import("Scanner.zig");

fn parseSource(allocator: std.mem.Allocator, source: []const u8) ![]const Ast.Rule {
    var scanner = Scanner.init(allocator, source);
    defer scanner.deinit();
    const tokens = try scanner.scanTokens();
    var parser = Parser.init(allocator, tokens, source);
    defer parser.diagnostics.deinit(allocator);
    const rules = try parser.parse();
    try std.testing.expectEqual(0, parser.diagnostics.items.len);
    return rules;
}

test "single rule with rulename reference" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = bar");
    defer allocator.free(rules);
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("foo", rules[0].name);
    try std.testing.expectEqualStrings("bar", rules[0].node.rulename);
}

test "alternation" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = a / b / c");
    defer allocator.free(rules);
    try std.testing.expectEqual(1, rules.len);
    const alts = rules[0].node.alternation;
    try std.testing.expectEqual(3, alts.len);
    try std.testing.expectEqualStrings("a", alts[0].rulename);
    try std.testing.expectEqualStrings("b", alts[1].rulename);
    try std.testing.expectEqualStrings("c", alts[2].rulename);
}

test "concatenation" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = a b c");
    defer allocator.free(rules);
    const cat = rules[0].node.concatenation;
    try std.testing.expectEqual(3, cat.len);
    try std.testing.expectEqualStrings("a", cat[0].rulename);
    try std.testing.expectEqualStrings("b", cat[1].rulename);
    try std.testing.expectEqualStrings("c", cat[2].rulename);
}

test "repetition star" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = *bar");
    defer allocator.free(rules);
    const rep = rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(null, rep.max);
    try std.testing.expectEqualStrings("bar", rep.element.rulename);
}

test "repetition bounded" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = 3*5bar");
    defer allocator.free(rules);
    const rep = rules[0].node.repetition;
    try std.testing.expectEqual(3, rep.min);
    try std.testing.expectEqual(5, rep.max.?);
    try std.testing.expectEqualStrings("bar", rep.element.rulename);
}

test "repetition exact" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = 3bar");
    defer allocator.free(rules);
    const rep = rules[0].node.repetition;
    try std.testing.expectEqual(3, rep.min);
    try std.testing.expectEqual(3, rep.max.?);
}

test "char val strips quotes" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = \"hello\"");
    defer allocator.free(rules);
    try std.testing.expectEqualStrings("hello", rules[0].node.char_val.value);
    try std.testing.expectEqual(false, rules[0].node.char_val.case_sensitive);
}

test "case-sensitive char val (RFC 7405)" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = %s\"Hello\"");
    defer allocator.free(rules);
    try std.testing.expectEqualStrings("Hello", rules[0].node.char_val.value);
    try std.testing.expectEqual(true, rules[0].node.char_val.case_sensitive);
}

test "explicit case-insensitive char val (RFC 7405)" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = %i\"Hello\"");
    defer allocator.free(rules);
    try std.testing.expectEqualStrings("Hello", rules[0].node.char_val.value);
    try std.testing.expectEqual(false, rules[0].node.char_val.case_sensitive);
}

test "hex val range" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = %x41-5A");
    defer allocator.free(rules);
    const range = rules[0].node.num_val.range;
    try std.testing.expectEqual(0x41, range.lo);
    try std.testing.expectEqual(0x5A, range.hi);
}

test "hex val concat" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = %x48.65.6C");
    defer allocator.free(rules);
    const concat = rules[0].node.num_val.concat;
    try std.testing.expectEqualStrings("Hel", concat);
}

test "option desugars to repetition 0..1" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = [bar]");
    defer allocator.free(rules);
    const rep = rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(1, rep.max.?);
    try std.testing.expectEqualStrings("bar", rep.element.rulename);
}

test "multiple rules" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = a\nbar = b");
    defer allocator.free(rules);
    try std.testing.expectEqual(2, rules.len);
    try std.testing.expectEqualStrings("foo", rules[0].name);
    try std.testing.expectEqualStrings("bar", rules[1].name);
}

test "incremental alternation preserved unmerged" {
    const allocator = std.testing.allocator;
    const rules = try parseSource(allocator, "foo = a\nfoo =/ b");
    defer allocator.free(rules);
    try std.testing.expectEqual(2, rules.len);
    try std.testing.expectEqualStrings("foo", rules[0].name);
    try std.testing.expectEqual(false, rules[0].incremental);
    try std.testing.expectEqualStrings("a", rules[0].node.rulename);
    try std.testing.expectEqualStrings("foo", rules[1].name);
    try std.testing.expectEqual(true, rules[1].incremental);
    try std.testing.expectEqualStrings("b", rules[1].node.rulename);
}

fn expectSyntaxError(source: []const u8, expected: Diagnostic.Expected, found_tag: Token.Tag) !void {
    const allocator = std.testing.allocator;
    var scanner = Scanner.init(allocator, source);
    defer scanner.deinit();
    const tokens = try scanner.scanTokens();
    var parser = Parser.init(allocator, tokens, source);
    defer parser.diagnostics.deinit(allocator);
    const rules = try parser.parse();
    allocator.free(rules);
    try std.testing.expect(parser.diagnostics.items.len > 0);
    const diag = parser.diagnostics.items[0];
    try std.testing.expectEqual(expected, diag.expected);
    try std.testing.expectEqual(found_tag, diag.found_tag);
}

test "diagnostic: unexpected token in element position" {
    try expectSyntaxError("foo = (a / )", .element, .right_paren);
}

test "diagnostic: missing closing paren" {
    try expectSyntaxError("foo = (a", .right_paren, .eof);
}

test "diagnostic: missing closing bracket" {
    try expectSyntaxError("foo = [bar", .right_bracket, .eof);
}

// --- Synchronization / recovery tests ----------------------------------------

test "recovery: error in first rule, second rule parsed" {
    const allocator = std.testing.allocator;
    const source = "foo = )\nbar = baz";
    var scanner = Scanner.init(allocator, source);
    defer scanner.deinit();
    const tokens = try scanner.scanTokens();
    var parser = Parser.init(allocator, tokens, source);
    defer parser.diagnostics.deinit(allocator);
    const rules = try parser.parse();
    defer allocator.free(rules);
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("bar", rules[0].name);
    try std.testing.expectEqual(1, parser.diagnostics.items.len);
}

test "recovery: multiple errors accumulate" {
    const allocator = std.testing.allocator;
    const source = "a = )\nb = ]\nc = d";
    var scanner = Scanner.init(allocator, source);
    defer scanner.deinit();
    const tokens = try scanner.scanTokens();
    var parser = Parser.init(allocator, tokens, source);
    defer parser.diagnostics.deinit(allocator);
    const rules = try parser.parse();
    defer allocator.free(rules);
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("c", rules[0].name);
    try std.testing.expectEqual(2, parser.diagnostics.items.len);
}

test "recovery: all rules have errors" {
    const allocator = std.testing.allocator;
    const source = "a = )\nb = ]";
    var scanner = Scanner.init(allocator, source);
    defer scanner.deinit();
    const tokens = try scanner.scanTokens();
    var parser = Parser.init(allocator, tokens, source);
    defer parser.diagnostics.deinit(allocator);
    const rules = try parser.parse();
    defer allocator.free(rules);
    try std.testing.expectEqual(0, rules.len);
    try std.testing.expectEqual(2, parser.diagnostics.items.len);
}

test "recovery: unclosed group, next rule still parsed" {
    const allocator = std.testing.allocator;
    const source = "a = (b /\nc = d";
    var scanner = Scanner.init(allocator, source);
    defer scanner.deinit();
    const tokens = try scanner.scanTokens();
    var parser = Parser.init(allocator, tokens, source);
    defer parser.diagnostics.deinit(allocator);
    const rules = try parser.parse();
    defer allocator.free(rules);
    try std.testing.expect(parser.diagnostics.items.len > 0);
    // The second rule "c = d" should be recovered.
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("c", rules[0].name);
}
