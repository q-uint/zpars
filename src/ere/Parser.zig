/// ERE parser — produces an AST from a POSIX Extended Regular Expression.
///
/// Grammar (IEEE Std 1003.1, Section 9.4):
///   ERE         = alternation
///   alternation = branch ('|' branch)*
///   branch      = piece*
///   piece       = atom quantifier?
///   quantifier  = '*' | '+' | '?' | '{' interval '}'
///   interval    = number | number ',' | number ',' number
///   atom        = char | '.' | '^' | '$' | '(' alternation ')' | bracket_expr
///
/// Produces a single `Ast.Rule` with an empty name.
const std = @import("std");
const Token = @import("Token.zig").Token;
const Ast = @import("../Ast.zig");
const Diagnostic = @import("Diagnostic.zig").Diagnostic;
const Pool = @import("../pool.zig").Pool;

const Parser = @This();

pub const ParseError = error{ SyntaxError, Overflow };

pub const max_rules = 1;
pub const max_nodes = 4096;
pub const max_ranges = 1024;
pub const max_bytes = 1024;
pub const max_diagnostics = 64;

tokens: []const Token,
source: []const u8,
pos: usize = 0,

nodes: Pool(Ast.Node, max_nodes) = .{},
ranges: Pool(Ast.ClassRange, max_ranges) = .{},
bytes: Pool(u8, max_bytes) = .{},
rules: Pool(Ast.Rule, max_rules) = .{},
diagnostics: Pool(Diagnostic, max_diagnostics) = .{},

pub fn init(tokens: []const Token, source: []const u8) Parser {
    return .{ .tokens = tokens, .source = source };
}

/// Parse the ERE token stream into a single rule.
pub fn parse(self: *Parser) ParseError![]const Ast.Rule {
    const node = try self.parseAlternation();

    if (self.peek().tag != .eof) {
        self.fail(.eof, self.peek());
        return error.SyntaxError;
    }

    _ = self.rules.addOne(.{ .name = "", .node = node, .incremental = false });
    return self.rules.slice();
}

pub fn getDiagnostics(self: *const Parser) []const Diagnostic {
    return self.diagnostics.slice();
}

/// alternation = branch ('|' branch)*
fn parseAlternation(self: *Parser) ParseError!Ast.Node {
    var buf: [256]Ast.Node = undefined;
    buf[0] = try self.parseBranch();
    var count: usize = 1;

    while (self.peek().tag == .pipe) {
        _ = self.advance();
        buf[count] = try self.parseBranch();
        count += 1;
    }

    if (count == 1) return buf[0];
    return .{ .alternation = self.nodes.addSlice(buf[0..count]) };
}

/// branch = piece*
fn parseBranch(self: *Parser) ParseError!Ast.Node {
    var buf: [256]Ast.Node = undefined;
    var count: usize = 0;

    while (self.isAtAtom()) {
        buf[count] = try self.parsePiece();
        count += 1;
    }

    if (count == 0) {
        // Empty branch — matches empty string.
        return .{ .concatenation = self.nodes.addSlice(buf[0..0]) };
    }
    if (count == 1) return buf[0];
    return .{ .concatenation = self.nodes.addSlice(buf[0..count]) };
}

/// piece = atom quantifier?
fn parsePiece(self: *Parser) ParseError!Ast.Node {
    const atom = try self.parseAtom();
    return self.parseQuantifier(atom);
}

/// quantifier = '*' | '+' | '?' | '{' interval '}'
fn parseQuantifier(self: *Parser, atom: Ast.Node) ParseError!Ast.Node {
    return switch (self.peek().tag) {
        .star => {
            _ = self.advance();
            return .{ .repetition = .{ .min = 0, .max = null, .element = self.nodes.addOne(atom) } };
        },
        .plus => {
            _ = self.advance();
            return .{ .repetition = .{ .min = 1, .max = null, .element = self.nodes.addOne(atom) } };
        },
        .question => {
            _ = self.advance();
            return .{ .repetition = .{ .min = 0, .max = 1, .element = self.nodes.addOne(atom) } };
        },
        .lbrace => self.parseInterval(atom),
        else => atom,
    };
}

/// Parse `{ number [, [number]] }` — already tokenized by the scanner.
fn parseInterval(self: *Parser, atom: Ast.Node) ParseError!Ast.Node {
    _ = self.advance(); // consume {
    const min = self.parseNumber() orelse {
        self.fail(.number, self.peek());
        return error.SyntaxError;
    };

    if (self.peek().tag == .rbrace) {
        // {m} — exact
        _ = self.advance();
        return .{ .repetition = .{ .min = min, .max = min, .element = self.nodes.addOne(atom) } };
    }

    if (self.peek().tag != .comma) {
        self.fail(.right_brace, self.peek());
        return error.SyntaxError;
    }
    _ = self.advance(); // consume ,

    if (self.peek().tag == .rbrace) {
        // {m,} — unbounded
        _ = self.advance();
        return .{ .repetition = .{ .min = min, .max = null, .element = self.nodes.addOne(atom) } };
    }

    const max = self.parseNumber() orelse {
        self.fail(.number, self.peek());
        return error.SyntaxError;
    };

    if (self.peek().tag != .rbrace) {
        self.fail(.right_brace, self.peek());
        return error.SyntaxError;
    }
    _ = self.advance();

    return .{ .repetition = .{ .min = min, .max = max, .element = self.nodes.addOne(atom) } };
}

fn parseNumber(self: *Parser) ?usize {
    if (self.peek().tag != .number) return null;
    const lex = self.advance().lexeme(self.source);
    return std.fmt.parseInt(usize, lex, 10) catch null;
}

/// atom = char | '.' | '^' | '$' | '(' alternation ')' | bracket_expr
fn parseAtom(self: *Parser) ParseError!Ast.Node {
    return switch (self.peek().tag) {
        .char => self.parseChar(),
        .dot => {
            _ = self.advance();
            return .any;
        },
        .caret => {
            _ = self.advance();
            return .anchor_start;
        },
        .dollar => {
            _ = self.advance();
            return .anchor_end;
        },
        .left_paren => {
            _ = self.advance();
            const expr = try self.parseAlternation();
            if (self.peek().tag != .right_paren) {
                self.fail(.right_paren, self.peek());
                return error.SyntaxError;
            }
            _ = self.advance();
            return expr;
        },
        .bracket_expr => self.parseBracketExpr(),
        else => {
            self.fail(.expression, self.peek());
            return error.SyntaxError;
        },
    };
}

fn parseChar(self: *Parser) Ast.Node {
    const lex = self.advance().lexeme(self.source);
    const ch: u8 = if (lex.len == 2 and lex[0] == '\\')
        lex[1]
    else
        lex[0];
    const value = self.bytes.items[self.bytes.count .. self.bytes.count + 1];
    _ = self.bytes.addOne(ch);
    return .{ .char_val = .{ .value = value, .case_sensitive = true } };
}

fn parseBracketExpr(self: *Parser) Ast.Node {
    const lex = self.advance().lexeme(self.source);
    // Strip outer brackets.
    var inner = lex[1 .. lex.len - 1];

    // Check for negation.
    const negated = inner.len > 0 and inner[0] == '^';
    if (negated) inner = inner[1..];

    // Per POSIX: ] immediately after [ or [^ is a literal.
    // The scanner already handled this by including it in the token,
    // so if inner starts with ] it's a literal.

    const start = self.ranges.count;
    var i: usize = 0;
    while (i < inner.len) {
        // Check for POSIX class [:name:]
        if (i + 2 < inner.len and inner[i] == '[' and inner[i + 1] == ':') {
            if (self.parsePosixClass(inner, &i)) continue;
        }

        const lo = decodeBracketChar(inner, &i) orelse break;

        // Check for range: char '-' char (but not trailing '-').
        if (i + 1 < inner.len and inner[i] == '-' and i + 1 < inner.len) {
            const after_dash = i + 1;
            if (after_dash < inner.len and inner[after_dash] != ']') {
                i += 1; // skip '-'
                const hi = decodeBracketChar(inner, &i) orelse {
                    _ = self.ranges.addOne(.{ .lo = lo, .hi = lo });
                    _ = self.ranges.addOne(.{ .lo = '-', .hi = '-' });
                    break;
                };
                _ = self.ranges.addOne(.{ .lo = lo, .hi = hi });
                continue;
            }
        }

        _ = self.ranges.addOne(.{ .lo = lo, .hi = lo });
    }

    const ranges = self.ranges.items[start..self.ranges.count];
    if (negated) return .{ .neg_char_class = ranges };
    return .{ .char_class = ranges };
}

/// Try to parse a POSIX character class like [:alpha:] starting at inner[*i].
/// Returns true on success, advancing *i past the closing :].
fn parsePosixClass(self: *Parser, inner: []const u8, i: *usize) bool {
    // Find closing :]
    const class_start = i.* + 2; // skip [:
    var j = class_start;
    while (j + 1 < inner.len) : (j += 1) {
        if (inner[j] == ':' and inner[j + 1] == ']') {
            const name = inner[class_start..j];
            if (self.addPosixClassRanges(name)) {
                i.* = j + 2; // skip :]
                return true;
            }
            return false;
        }
    }
    return false;
}

/// Add ClassRange entries for a POSIX character class name (C/POSIX locale).
fn addPosixClassRanges(self: *Parser, name: []const u8) bool {
    const classes = .{
        .{ "alpha", &[_][2]u8{ .{ 'A', 'Z' }, .{ 'a', 'z' } } },
        .{ "digit", &[_][2]u8{.{ '0', '9' }} },
        .{ "alnum", &[_][2]u8{ .{ 'A', 'Z' }, .{ 'a', 'z' }, .{ '0', '9' } } },
        .{ "upper", &[_][2]u8{.{ 'A', 'Z' }} },
        .{ "lower", &[_][2]u8{.{ 'a', 'z' }} },
        .{ "space", &[_][2]u8{ .{ '\t', '\r' }, .{ ' ', ' ' } } },
        .{ "blank", &[_][2]u8{ .{ '\t', '\t' }, .{ ' ', ' ' } } },
        .{ "print", &[_][2]u8{.{ 0x20, 0x7E }} },
        .{ "graph", &[_][2]u8{.{ 0x21, 0x7E }} },
        .{ "cntrl", &[_][2]u8{ .{ 0x00, 0x1F }, .{ 0x7F, 0x7F } } },
        .{ "xdigit", &[_][2]u8{ .{ '0', '9' }, .{ 'A', 'F' }, .{ 'a', 'f' } } },
        .{ "punct", &[_][2]u8{ .{ '!', '/' }, .{ ':', '@' }, .{ '[', '`' }, .{ '{', '~' } } },
    };

    inline for (classes) |entry| {
        if (std.mem.eql(u8, name, entry[0])) {
            for (entry[1]) |r| {
                _ = self.ranges.addOne(.{ .lo = r[0], .hi = r[1] });
            }
            return true;
        }
    }
    return false;
}

fn decodeBracketChar(raw: []const u8, i: *usize) ?u8 {
    if (i.* >= raw.len) return null;
    if (raw[i.*] == '\\' and i.* + 1 < raw.len) {
        i.* += 1;
        const c = raw[i.*];
        i.* += 1;
        return switch (c) {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            else => c,
        };
    }
    const c = raw[i.*];
    i.* += 1;
    return c;
}

/// Can the current token start an atom?
fn isAtAtom(self: *Parser) bool {
    return switch (self.peek().tag) {
        .char, .dot, .caret, .dollar, .left_paren, .bracket_expr => true,
        else => false,
    };
}

fn peek(self: *Parser) Token {
    return self.tokens[self.pos];
}

fn advance(self: *Parser) Token {
    const tok = self.tokens[self.pos];
    self.pos += 1;
    return tok;
}

fn fail(self: *Parser, expected: Diagnostic.Expected, tok: Token) void {
    _ = self.diagnostics.addOne(.{
        .expected = expected,
        .found_tag = tok.tag,
        .found_start = tok.start,
        .found_len = tok.len,
        .line = tok.line,
    });
}

const Scanner = @import("Scanner.zig");

fn parseSource(source: []const u8) ParseError!struct { parser: Parser, rules: []const Ast.Rule } {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    if (parser.diagnostics.count != 0) return error.SyntaxError;
    return .{ .parser = parser, .rules = rules };
}

test "simple literal" {
    const result = try parseSource("abc");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(3, cat.len);
    try std.testing.expectEqualStrings("a", cat[0].char_val.value);
    try std.testing.expectEqualStrings("b", cat[1].char_val.value);
    try std.testing.expectEqualStrings("c", cat[2].char_val.value);
}

test "alternation" {
    const result = try parseSource("a|b|c");
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(3, alts.len);
    try std.testing.expectEqualStrings("a", alts[0].char_val.value);
    try std.testing.expectEqualStrings("b", alts[1].char_val.value);
    try std.testing.expectEqualStrings("c", alts[2].char_val.value);
}

test "quantifier star" {
    const result = try parseSource("a*");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(null, rep.max);
    try std.testing.expectEqualStrings("a", rep.element.char_val.value);
}

test "quantifier plus" {
    const result = try parseSource("a+");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(1, rep.min);
    try std.testing.expectEqual(null, rep.max);
}

test "quantifier question" {
    const result = try parseSource("a?");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(1, rep.max.?);
}

test "interval {m,n}" {
    const result = try parseSource("a{2,5}");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(2, rep.min);
    try std.testing.expectEqual(5, rep.max.?);
}

test "interval {m}" {
    const result = try parseSource("a{3}");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(3, rep.min);
    try std.testing.expectEqual(3, rep.max.?);
}

test "interval {m,}" {
    const result = try parseSource("a{3,}");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(3, rep.min);
    try std.testing.expectEqual(null, rep.max);
}

test "dot wildcard" {
    const result = try parseSource(".");
    try std.testing.expectEqual(.any, result.rules[0].node);
}

test "anchors" {
    const result = try parseSource("^a$");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(3, cat.len);
    try std.testing.expectEqual(.anchor_start, cat[0]);
    try std.testing.expectEqualStrings("a", cat[1].char_val.value);
    try std.testing.expectEqual(.anchor_end, cat[2]);
}

test "grouping" {
    const result = try parseSource("(a|b)+");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(1, rep.min);
    const alts = rep.element.alternation;
    try std.testing.expectEqual(2, alts.len);
}

test "character class" {
    const result = try parseSource("[a-z]");
    const ranges = result.rules[0].node.char_class;
    try std.testing.expectEqual(1, ranges.len);
    try std.testing.expectEqual('a', ranges[0].lo);
    try std.testing.expectEqual('z', ranges[0].hi);
}

test "negated character class" {
    const result = try parseSource("[^0-9]");
    const ranges = result.rules[0].node.neg_char_class;
    try std.testing.expectEqual(1, ranges.len);
    try std.testing.expectEqual('0', ranges[0].lo);
    try std.testing.expectEqual('9', ranges[0].hi);
}

test "POSIX class [:alpha:]" {
    const result = try parseSource("[[:alpha:]]");
    const ranges = result.rules[0].node.char_class;
    try std.testing.expectEqual(2, ranges.len);
    try std.testing.expectEqual('A', ranges[0].lo);
    try std.testing.expectEqual('Z', ranges[0].hi);
    try std.testing.expectEqual('a', ranges[1].lo);
    try std.testing.expectEqual('z', ranges[1].hi);
}

test "POSIX class [:digit:]" {
    const result = try parseSource("[[:digit:]]");
    const ranges = result.rules[0].node.char_class;
    try std.testing.expectEqual(1, ranges.len);
    try std.testing.expectEqual('0', ranges[0].lo);
    try std.testing.expectEqual('9', ranges[0].hi);
}

test "escaped special" {
    const result = try parseSource("\\.");
    try std.testing.expectEqualStrings(".", result.rules[0].node.char_val.value);
}

test "complex: identifier pattern" {
    const result = try parseSource("^[a-zA-Z_][a-zA-Z0-9_]*$");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(4, cat.len);
    try std.testing.expectEqual(.anchor_start, cat[0]);
    // [a-zA-Z_]
    try std.testing.expectEqual(3, cat[1].char_class.len);
    // [a-zA-Z0-9_]*
    try std.testing.expectEqual(0, cat[2].repetition.min);
    try std.testing.expectEqual(.anchor_end, cat[3]);
}

test "empty alternation branch" {
    const result = try parseSource("a|");
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(2, alts.len);
    try std.testing.expectEqualStrings("a", alts[0].char_val.value);
    try std.testing.expectEqual(0, alts[1].concatenation.len);
}

test "mixed class and POSIX" {
    const result = try parseSource("[[:digit:]a-f]");
    const ranges = result.rules[0].node.char_class;
    // [:digit:] expands to 0-9, plus a-f
    try std.testing.expectEqual(2, ranges.len);
    try std.testing.expectEqual('0', ranges[0].lo);
    try std.testing.expectEqual('9', ranges[0].hi);
    try std.testing.expectEqual('a', ranges[1].lo);
    try std.testing.expectEqual('f', ranges[1].hi);
}

test "single rule with empty name" {
    const result = try parseSource("a");
    try std.testing.expectEqualStrings("", result.rules[0].name);
    try std.testing.expectEqual(false, result.rules[0].incremental);
}
