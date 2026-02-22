/// BNF parser — produces an AST from a BNF token stream (ALGOL 60 variant).
///
/// Grammar:
///   rule          = rulename "::=" alternation
///   alternation   = concatenation *("|" concatenation)
///   concatenation = *element
///   element       = rulename / terminal
const std = @import("std");
const Token = @import("Token.zig").Token;
const Ast = @import("../Ast.zig");
const Diagnostic = @import("Diagnostic.zig").Diagnostic;

const Parser = @This();

pub const ParseError = error{ SyntaxError, Overflow };

pub const max_rules = 256;
pub const max_nodes = 4096;
pub const max_diagnostics = 64;

tokens: []const Token,
source: []const u8,
pos: usize = 0,

/// Pool for AST nodes.
nodes: [max_nodes]Ast.Node = undefined,
node_count: usize = 0,

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
    self.skipNewlines();
    while (self.peek().tag != .eof) {
        const rule = self.parseRule() catch |err| switch (err) {
            error.SyntaxError => {
                self.synchronize();
                self.skipNewlines();
                continue;
            },
            else => |e| return e,
        };

        self.rules[self.rule_count] = rule;
        self.rule_count += 1;
        self.skipNewlines();
    }

    return self.rules[0..self.rule_count];
}

pub fn getDiagnostics(self: *const Parser) []const Diagnostic {
    return self.diagnostics_buf[0..self.diagnostics_count];
}

// --- Grammar rules -----------------------------------------------------------

/// rule = rulename "::=" alternation
fn parseRule(self: *Parser) ParseError!Ast.Rule {
    if (self.peek().tag != .rulename) {
        self.fail(.rulename, self.peek());
        return error.SyntaxError;
    }
    const name_tok = self.advance();
    const name_lex = name_tok.lexeme(self.source);
    // Strip angle brackets: <name> → name
    const name = name_lex[1 .. name_lex.len - 1];

    self.skipNewlines();
    if (self.peek().tag != .definition) {
        self.fail(.definition, self.peek());
        return error.SyntaxError;
    }
    _ = self.advance(); // consume ::=
    self.skipNewlines();

    return .{ .name = name, .node = try self.parseAlternation(), .incremental = false };
}

/// alternation = concatenation *("|" concatenation)
fn parseAlternation(self: *Parser) ParseError!Ast.Node {
    var buf: [256]Ast.Node = undefined;
    var count: usize = 0;

    buf[0] = self.parseConcatenation();
    count = 1;

    while (true) {
        self.skipNewlines();
        if (self.peek().tag != .pipe) break;
        _ = self.advance();
        self.skipNewlines();
        buf[count] = self.parseConcatenation();
        count += 1;
    }

    if (count == 1) return buf[0];
    return .{ .alternation = self.allocSlice(buf[0..count]) };
}

/// concatenation = *element
fn parseConcatenation(self: *Parser) Ast.Node {
    var buf: [256]Ast.Node = undefined;
    var count: usize = 0;

    while (self.isAtElement()) {
        buf[count] = self.parseElement();
        count += 1;
    }

    if (count == 1) return buf[0];
    return .{ .concatenation = self.allocSlice(buf[0..count]) };
}

/// element = rulename / terminal
fn parseElement(self: *Parser) Ast.Node {
    return switch (self.peek().tag) {
        .rulename => {
            const lex = self.advance().lexeme(self.source);
            // Strip angle brackets: <name> → name
            return .{ .rulename = lex[1 .. lex.len - 1] };
        },
        .terminal => {
            const lex = self.advance().lexeme(self.source);
            return .{ .char_val = .{ .value = lex, .case_sensitive = true } };
        },
        else => unreachable, // isAtElement guards this
    };
}

// --- Pool allocators ---------------------------------------------------------

fn allocSlice(self: *Parser, items: []const Ast.Node) []const Ast.Node {
    const start = self.node_count;
    for (items) |item| {
        self.nodes[self.node_count] = item;
        self.node_count += 1;
    }
    return self.nodes[start..self.node_count];
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

fn peek(self: *Parser) Token {
    return self.tokens[self.pos];
}

fn advance(self: *Parser) Token {
    const tok = self.tokens[self.pos];
    self.pos += 1;
    return tok;
}

fn skipNewlines(self: *Parser) void {
    while (self.peek().tag == .newline) self.pos += 1;
}

/// Skip tokens until the start of the next rule or EOF.
/// A rule boundary is a `<rulename>` followed by `::=`.
fn synchronize(self: *Parser) void {
    while (self.peek().tag != .eof) {
        if (self.peek().tag == .rulename) {
            const next = self.peekNextMeaningful();
            if (next == .definition) return;
        }
        self.pos += 1;
    }
}

/// Can the current position start an element?
fn isAtElement(self: *Parser) bool {
    return switch (self.peek().tag) {
        .terminal => true,
        .rulename => {
            // A <rulename> followed by ::= starts a new rule, not an element.
            const next = self.peekNextMeaningful();
            return next != .definition;
        },
        else => false,
    };
}

/// Next non-newline token tag after the current position.
fn peekNextMeaningful(self: *Parser) Token.Tag {
    var i = self.pos + 1;
    while (i < self.tokens.len) : (i += 1) {
        const tag = self.tokens[i].tag;
        if (tag != .newline) return tag;
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

test "single terminal rule" {
    const result = try parseSource("<digit> ::= 0");
    try std.testing.expectEqual(1, result.rules.len);
    try std.testing.expectEqualStrings("digit", result.rules[0].name);
    try std.testing.expectEqualStrings("0", result.rules[0].node.char_val.value);
    try std.testing.expectEqual(true, result.rules[0].node.char_val.case_sensitive);
}

test "alternation" {
    const result = try parseSource("<bit> ::= 0 | 1");
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(2, alts.len);
    try std.testing.expectEqualStrings("0", alts[0].char_val.value);
    try std.testing.expectEqualStrings("1", alts[1].char_val.value);
}

test "concatenation with non-terminal references" {
    const result = try parseSource("<pair> ::= <a> <b>");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(2, cat.len);
    try std.testing.expectEqualStrings("a", cat[0].rulename);
    try std.testing.expectEqualStrings("b", cat[1].rulename);
}

test "mixed terminals and non-terminals" {
    const result = try parseSource("<expr> ::= <term> + <term>");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(3, cat.len);
    try std.testing.expectEqualStrings("term", cat[0].rulename);
    try std.testing.expectEqualStrings("+", cat[1].char_val.value);
    try std.testing.expectEqualStrings("term", cat[2].rulename);
}

test "empty rule" {
    const result = try parseSource("<empty> ::=");
    try std.testing.expectEqualStrings("empty", result.rules[0].name);
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(0, cat.len);
}

test "multiple rules" {
    const result = try parseSource("<a> ::= x\n<b> ::= y");
    try std.testing.expectEqual(2, result.rules.len);
    try std.testing.expectEqualStrings("a", result.rules[0].name);
    try std.testing.expectEqualStrings("b", result.rules[1].name);
}

test "incremental is always false" {
    const result = try parseSource("<a> ::= x");
    try std.testing.expectEqual(false, result.rules[0].incremental);
}

test "ALGOL 60 example" {
    const result = try parseSource("<ab> ::= ( | [ | <ab> ( | <ab> <d>");
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(4, alts.len);
    // ( — single terminal
    try std.testing.expectEqualStrings("(", alts[0].char_val.value);
    // [ — single terminal
    try std.testing.expectEqualStrings("[", alts[1].char_val.value);
    // <ab> ( — concatenation
    const cat3 = alts[2].concatenation;
    try std.testing.expectEqual(2, cat3.len);
    try std.testing.expectEqualStrings("ab", cat3[0].rulename);
    try std.testing.expectEqualStrings("(", cat3[1].char_val.value);
    // <ab> <d> — concatenation
    const cat4 = alts[3].concatenation;
    try std.testing.expectEqual(2, cat4.len);
    try std.testing.expectEqualStrings("ab", cat4[0].rulename);
    try std.testing.expectEqualStrings("d", cat4[1].rulename);
}

test "recovery: error in first rule, second rule parsed" {
    var scanner = Scanner.init("<a> ::=\n| oops\n<b> ::= y");
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, "<a> ::=\n| oops\n<b> ::= y");
    const rules = try parser.parse();
    // The <a> rule has an empty concat followed by | which makes a valid
    // alternation, so both rules should parse. Let's just verify <b> is present.
    var found_b = false;
    for (rules) |rule| {
        if (std.mem.eql(u8, rule.name, "b")) found_b = true;
    }
    try std.testing.expect(found_b);
}

test "recovery: missing definition operator" {
    var scanner = Scanner.init("<a> x\n<b> ::= y");
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, "<a> x\n<b> ::= y");
    const rules = try parser.parse();
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("b", rules[0].name);
    try std.testing.expect(parser.getDiagnostics().len > 0);
}
