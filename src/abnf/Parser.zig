const std = @import("std");
const Token = @import("Token.zig").Token;
const Ast = @import("../Ast.zig");
const Diagnostic = @import("Diagnostic.zig").Diagnostic;

const Parser = @This();

pub const ParseError = error{ SyntaxError, Overflow, InvalidCharacter };

pub const max_rules = 256;
pub const max_nodes = 4096;
pub const max_bytes = 1024;
pub const max_diagnostics = 64;

tokens: []const Token,
source: []const u8,
pos: usize = 0,

/// Pool for AST nodes (alternation/concatenation slices and repetition elements).
nodes: [max_nodes]Ast.Node = undefined,
node_count: usize = 0,

/// Pool for num_val concat byte sequences.
bytes: [max_bytes]u8 = undefined,
byte_count: usize = 0,

/// Parsed rules.
rules: [max_rules]Ast.Rule = undefined,
rule_count: usize = 0,

/// Accumulated parse diagnostics.
diagnostics_buf: [max_diagnostics]Diagnostic = undefined,
diagnostics_count: usize = 0,

pub fn init(tokens: []const Token, source: []const u8) Parser {
    return .{
        .tokens = tokens,
        .source = source,
    };
}

/// Parse all rules from the token stream.
pub fn parse(self: *Parser) ParseError![]const Ast.Rule {
    self.skipTrivia();
    while (self.peek().tag != .eof) {
        const rule = self.parseRule() catch |err| switch (err) {
            error.SyntaxError => {
                self.synchronize();
                self.skipTrivia();
                continue;
            },
            else => |e| return e,
        };

        self.rules[self.rule_count] = rule;
        self.rule_count += 1;
        self.skipTrivia();
    }

    return self.rules[0..self.rule_count];
}

pub fn getDiagnostics(self: *const Parser) []const Diagnostic {
    return self.diagnostics_buf[0..self.diagnostics_count];
}

// --- Grammar rules -----------------------------------------------------------

/// rule = rulename ("=" / "=/") alternation
fn parseRule(self: *Parser) ParseError!Ast.Rule {
    if (self.peek().tag != .rulename) {
        self.fail(.rulename, self.peek());
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
fn parseAlternation(self: *Parser) ParseError!Ast.Node {
    var buf: [256]Ast.Node = undefined;
    var count: usize = 0;

    buf[0] = try self.parseConcatenation();
    count = 1;

    while (true) {
        self.skipTrivia();
        if (self.peek().tag != .slash) break;
        _ = self.advance();
        self.skipTrivia();
        buf[count] = try self.parseConcatenation();
        count += 1;
    }

    if (count == 1) return buf[0];
    return .{ .alternation = self.allocSlice(buf[0..count]) };
}

/// concatenation = repetition *(repetition)
fn parseConcatenation(self: *Parser) ParseError!Ast.Node {
    var buf: [256]Ast.Node = undefined;
    var count: usize = 0;

    while (self.isAtRepetition()) {
        buf[count] = try self.parseRepetition();
        count += 1;
        self.skipTrivia();
    }

    if (count == 1) return buf[0];
    return .{ .concatenation = self.allocSlice(buf[0..count]) };
}

/// repetition = [repeat] element
/// repeat     = 1*DIGIT / (*DIGIT "*" *DIGIT)
fn parseRepetition(self: *Parser) ParseError!Ast.Node {
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

    const ptr = self.allocNode(element);
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
            self.fail(.element, self.peek());
            return error.SyntaxError;
        },
    };
}

/// group = "(" alternation ")"
fn parseGroup(self: *Parser) ParseError!Ast.Node {
    const open = self.advance();
    if (open.tag != .left_paren) {
        self.fail(.left_paren, open);
        return error.SyntaxError;
    }
    self.skipTrivia();
    const node = try self.parseAlternation();
    self.skipTrivia();
    const close = self.advance();
    if (close.tag != .right_paren) {
        self.fail(.right_paren, close);
        return error.SyntaxError;
    }
    return node;
}

/// option = "[" alternation "]"  →  *1( alternation )
fn parseOption(self: *Parser) ParseError!Ast.Node {
    const open = self.advance();
    if (open.tag != .left_bracket) {
        self.fail(.left_bracket, open);
        return error.SyntaxError;
    }
    self.skipTrivia();
    const inner = try self.parseAlternation();
    self.skipTrivia();
    const close = self.advance();
    if (close.tag != .right_bracket) {
        self.fail(.right_bracket, close);
        return error.SyntaxError;
    }

    const ptr = self.allocNode(inner);
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
        var buf: [256]u8 = undefined;
        var count: usize = 0;
        var iter = std.mem.splitScalar(u8, digits, '.');
        while (iter.next()) |part| {
            buf[count] = try std.fmt.parseInt(u8, part, base);
            count += 1;
        }
        return .{ .concat = self.allocBytes(buf[0..count]) };
    }

    return .{ .single = try std.fmt.parseInt(u8, digits, base) };
}

// --- Pool allocators ---------------------------------------------------------

fn allocNode(self: *Parser, node: Ast.Node) *const Ast.Node {
    self.nodes[self.node_count] = node;
    const ptr = &self.nodes[self.node_count];
    self.node_count += 1;
    return ptr;
}

fn allocSlice(self: *Parser, items: []const Ast.Node) []const Ast.Node {
    const start = self.node_count;
    for (items) |item| {
        self.nodes[self.node_count] = item;
        self.node_count += 1;
    }
    return self.nodes[start..self.node_count];
}

fn allocBytes(self: *Parser, items: []const u8) []const u8 {
    const start = self.byte_count;
    for (items) |b| {
        self.bytes[self.byte_count] = b;
        self.byte_count += 1;
    }
    return self.bytes[start..self.byte_count];
}

// --- Diagnostics -------------------------------------------------------------

fn fail(self: *Parser, expected: Diagnostic.Expected, token: Token) void {
    self.diagnostics_buf[self.diagnostics_count] = .{
        .expected = expected,
        .found_tag = token.tag,
        .found_start = token.start,
        .found_len = token.len,
        .line = token.line,
    };
    self.diagnostics_count += 1;
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

fn parseSource(source: []const u8) ParseError!struct { parser: Parser, rules: []const Ast.Rule } {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    if (parser.diagnostics_count != 0) return error.SyntaxError;
    return .{ .parser = parser, .rules = rules };
}

test "single rule with rulename reference" {
    const result = try parseSource("foo = bar");
    try std.testing.expectEqual(1, result.rules.len);
    try std.testing.expectEqualStrings("foo", result.rules[0].name);
    try std.testing.expectEqualStrings("bar", result.rules[0].node.rulename);
}

test "alternation" {
    const result = try parseSource("foo = a / b / c");
    try std.testing.expectEqual(1, result.rules.len);
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(3, alts.len);
    try std.testing.expectEqualStrings("a", alts[0].rulename);
    try std.testing.expectEqualStrings("b", alts[1].rulename);
    try std.testing.expectEqualStrings("c", alts[2].rulename);
}

test "concatenation" {
    const result = try parseSource("foo = a b c");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(3, cat.len);
    try std.testing.expectEqualStrings("a", cat[0].rulename);
    try std.testing.expectEqualStrings("b", cat[1].rulename);
    try std.testing.expectEqualStrings("c", cat[2].rulename);
}

test "repetition star" {
    const result = try parseSource("foo = *bar");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(null, rep.max);
    try std.testing.expectEqualStrings("bar", rep.element.rulename);
}

test "repetition bounded" {
    const result = try parseSource("foo = 3*5bar");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(3, rep.min);
    try std.testing.expectEqual(5, rep.max.?);
    try std.testing.expectEqualStrings("bar", rep.element.rulename);
}

test "repetition exact" {
    const result = try parseSource("foo = 3bar");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(3, rep.min);
    try std.testing.expectEqual(3, rep.max.?);
}

test "char val strips quotes" {
    const result = try parseSource("foo = \"hello\"");
    try std.testing.expectEqualStrings("hello", result.rules[0].node.char_val.value);
    try std.testing.expectEqual(false, result.rules[0].node.char_val.case_sensitive);
}

test "case-sensitive char val (RFC 7405)" {
    const result = try parseSource("foo = %s\"Hello\"");
    try std.testing.expectEqualStrings("Hello", result.rules[0].node.char_val.value);
    try std.testing.expectEqual(true, result.rules[0].node.char_val.case_sensitive);
}

test "explicit case-insensitive char val (RFC 7405)" {
    const result = try parseSource("foo = %i\"Hello\"");
    try std.testing.expectEqualStrings("Hello", result.rules[0].node.char_val.value);
    try std.testing.expectEqual(false, result.rules[0].node.char_val.case_sensitive);
}

test "hex val range" {
    const result = try parseSource("foo = %x41-5A");
    const range = result.rules[0].node.num_val.range;
    try std.testing.expectEqual(0x41, range.lo);
    try std.testing.expectEqual(0x5A, range.hi);
}

test "hex val concat" {
    const result = try parseSource("foo = %x48.65.6C");
    const concat = result.rules[0].node.num_val.concat;
    try std.testing.expectEqualStrings("Hel", concat);
}

test "option desugars to repetition 0..1" {
    const result = try parseSource("foo = [bar]");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(1, rep.max.?);
    try std.testing.expectEqualStrings("bar", rep.element.rulename);
}

test "multiple rules" {
    const result = try parseSource("foo = a\nbar = b");
    try std.testing.expectEqual(2, result.rules.len);
    try std.testing.expectEqualStrings("foo", result.rules[0].name);
    try std.testing.expectEqualStrings("bar", result.rules[1].name);
}

test "incremental alternation preserved unmerged" {
    const result = try parseSource("foo = a\nfoo =/ b");
    try std.testing.expectEqual(2, result.rules.len);
    try std.testing.expectEqualStrings("foo", result.rules[0].name);
    try std.testing.expectEqual(false, result.rules[0].incremental);
    try std.testing.expectEqualStrings("a", result.rules[0].node.rulename);
    try std.testing.expectEqualStrings("foo", result.rules[1].name);
    try std.testing.expectEqual(true, result.rules[1].incremental);
    try std.testing.expectEqualStrings("b", result.rules[1].node.rulename);
}

fn expectSyntaxError(source: []const u8, expected: Diagnostic.Expected, found_tag: Token.Tag) !void {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    _ = try parser.parse();
    const diags = parser.getDiagnostics();
    try std.testing.expect(diags.len > 0);
    try std.testing.expectEqual(expected, diags[0].expected);
    try std.testing.expectEqual(found_tag, diags[0].found_tag);
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
    var scanner = Scanner.init("foo = )\nbar = baz");
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, "foo = )\nbar = baz");
    const rules = try parser.parse();
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("bar", rules[0].name);
    try std.testing.expectEqual(1, parser.getDiagnostics().len);
}

test "recovery: multiple errors accumulate" {
    const source = "a = )\nb = ]\nc = d";
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("c", rules[0].name);
    try std.testing.expectEqual(2, parser.getDiagnostics().len);
}

test "recovery: all rules have errors" {
    const source = "a = )\nb = ]";
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    try std.testing.expectEqual(0, rules.len);
    try std.testing.expectEqual(2, parser.getDiagnostics().len);
}

test "recovery: unclosed group, next rule still parsed" {
    const source = "a = (b /\nc = d";
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    try std.testing.expect(parser.getDiagnostics().len > 0);
    // The second rule "c = d" should be recovered.
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("c", rules[0].name);
}
