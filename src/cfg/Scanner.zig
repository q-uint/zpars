/// CFG scanner — tokenizes CFG grammar notation.
///
/// Syntax overview:
///   - `->` production arrow
///   - `|` alternation
///   - `"text"` string literal (case-sensitive)
///   - `%s"text"` explicit case-sensitive string
///   - `%i"text"` case-insensitive string
///   - `%x41` hex byte, `%x41-5A` hex range
///   - Bare identifiers are nonterminal references
///   - `//` line comments
///   - Blank lines and whitespace are skipped
const std = @import("std");
const Token = @import("Token.zig").Token;

const Scanner = @This();

pub const max_tokens = 4096;

/// The full source text being scanned.
source: []const u8,
/// Collected tokens (bounded).
tokens: [max_tokens]Token = undefined,
/// Number of tokens collected so far.
token_count: usize = 0,
/// Start of the current lexeme being scanned.
start: usize = 0,
/// Current position in source (next character to read).
current: usize = 0,
/// Current line number (1-based).
line: usize = 1,

pub fn init(source: []const u8) Scanner {
    return .{ .source = source };
}

/// Scan the entire source and return the token list.
pub fn scanTokens(self: *Scanner) []const Token {
    while (!self.isAtEnd()) {
        self.start = self.current;
        self.scanToken();
    }

    self.addToken(.eof);
    return self.tokens[0..self.token_count];
}

fn scanToken(self: *Scanner) void {
    const c = self.advance();
    switch (c) {
        // Line comment
        '/' => {
            if (self.peek() == '/') {
                // Skip rest of line
                while (!self.isAtEnd() and self.peek() != '\n') _ = self.advance();
            } else {
                self.addToken(.invalid);
            }
        },

        // Production arrow
        '-' => {
            if (self.peek() == '>') {
                _ = self.advance();
                self.addToken(.arrow);
            } else {
                self.addToken(.invalid);
            }
        },

        // Alternation
        '|' => self.addToken(.pipe),

        // String literal
        '"' => self.scanString(.string),

        // Percent-prefixed tokens: %x, %s, %i
        '%' => self.scanPercent(),

        // Whitespace — skip
        ' ', '\t', '\r' => {},

        // Newlines
        '\n' => {
            self.line += 1;
            self.addToken(.newline);
        },

        // Identifier (nonterminal)
        else => {
            if (isIdentStart(c)) {
                self.scanIdentifier();
            } else {
                self.addToken(.invalid);
            }
        },
    }
}

fn scanString(self: *Scanner, tag: Token.Tag) void {
    while (!self.isAtEnd() and self.peek() != '"') {
        if (self.peek() == '\n' or self.peek() == '\r') {
            self.addToken(.invalid);
            return;
        }
        _ = self.advance();
    }
    if (self.isAtEnd()) {
        self.addToken(.invalid);
    } else {
        _ = self.advance(); // closing "
        self.addToken(tag);
    }
}

fn scanPercent(self: *Scanner) void {
    if (self.isAtEnd()) {
        self.addToken(.invalid);
        return;
    }
    const c = self.peek();
    if (c == 'x' or c == 'X') {
        _ = self.advance(); // skip x
        self.scanHex();
    } else if (c == 's' and self.peekNext() == '"') {
        _ = self.advance(); // skip s
        _ = self.advance(); // skip "
        self.scanString(.string_cs);
    } else if (c == 'i' and self.peekNext() == '"') {
        _ = self.advance(); // skip i
        _ = self.advance(); // skip "
        self.scanString(.string_ci);
    } else {
        self.addToken(.invalid);
    }
}

fn scanHex(self: *Scanner) void {
    if (self.isAtEnd() or !isHexDigit(self.peek())) {
        self.addToken(.invalid);
        return;
    }
    while (!self.isAtEnd() and isHexDigit(self.peek())) _ = self.advance();

    if (!self.isAtEnd() and self.peek() == '-') {
        _ = self.advance(); // skip -
        if (self.isAtEnd() or !isHexDigit(self.peek())) {
            self.addToken(.invalid);
            return;
        }
        while (!self.isAtEnd() and isHexDigit(self.peek())) _ = self.advance();
        self.addToken(.hex_range);
    } else {
        self.addToken(.hex_byte);
    }
}

fn scanIdentifier(self: *Scanner) void {
    while (!self.isAtEnd() and isIdentCont(self.peek())) _ = self.advance();
    self.addToken(.identifier);
}

// === Primitive operations ===

fn advance(self: *Scanner) u8 {
    const c = self.source[self.current];
    self.current += 1;
    return c;
}

fn peek(self: *Scanner) u8 {
    if (self.isAtEnd()) return 0;
    return self.source[self.current];
}

fn peekNext(self: *Scanner) u8 {
    if (self.current + 1 >= self.source.len) return 0;
    return self.source[self.current + 1];
}

fn isAtEnd(self: *Scanner) bool {
    return self.current >= self.source.len;
}

fn addToken(self: *Scanner, tag: Token.Tag) void {
    self.tokens[self.token_count] = .{
        .tag = tag,
        .start = self.start,
        .len = self.current - self.start,
        .line = self.line,
    };
    self.token_count += 1;
}

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isIdentCont(c: u8) bool {
    return isIdentStart(c) or (c >= '0' and c <= '9') or c == '-';
}

fn isHexDigit(c: u8) bool {
    return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

// --- Tests -------------------------------------------------------------------

fn expectTags(source: []const u8, expected: []const Token.Tag) !void {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    const allocator = std.testing.allocator;
    const actual = try allocator.alloc(Token.Tag, tokens.len);
    defer allocator.free(actual);
    for (tokens, 0..) |tok, i| actual[i] = tok.tag;
    try std.testing.expectEqualSlices(Token.Tag, expected, actual);
}

test "simple rule" {
    try expectTags("S -> \"x\"", &.{ .identifier, .arrow, .string, .eof });
}

test "alternation" {
    try expectTags("S -> \"a\" | \"b\"", &.{
        .identifier, .arrow, .string, .pipe, .string, .eof,
    });
}

test "nonterminal reference" {
    try expectTags("S -> A B", &.{
        .identifier, .arrow, .identifier, .identifier, .eof,
    });
}

test "hex byte and range" {
    try expectTags("S -> %x41 %x61-7A", &.{
        .identifier, .arrow, .hex_byte, .hex_range, .eof,
    });
}

test "case-sensitive and case-insensitive strings" {
    try expectTags(
        \\S -> %s"GET" %i"hello"
    , &.{
        .identifier, .arrow, .string_cs, .string_ci, .eof,
    });
}

test "line comment" {
    try expectTags("// comment\nS -> \"x\"", &.{
        .newline, .identifier, .arrow, .string, .eof,
    });
}

test "multiline" {
    try expectTags("S -> A\nA -> \"x\"", &.{
        .identifier, .arrow, .identifier, .newline,
        .identifier, .arrow, .string, .eof,
    });
}

test "empty input" {
    try expectTags("", &.{.eof});
}

test "unterminated string" {
    try expectTags("S -> \"oops", &.{
        .identifier, .arrow, .invalid, .eof,
    });
}

test "invalid percent" {
    try expectTags("S -> %z", &.{
        .identifier, .arrow, .invalid, .identifier, .eof,
    });
}

test "lexeme extraction" {
    const source = "S -> \"hello\"";
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    // tokens: identifier("S"), arrow("->"), string("\"hello\""), eof
    try std.testing.expectEqualStrings("S", tokens[0].lexeme(source));
    try std.testing.expectEqualStrings("->", tokens[1].lexeme(source));
    try std.testing.expectEqualStrings("\"hello\"", tokens[2].lexeme(source));
}
