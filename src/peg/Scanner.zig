/// PEG scanner — tokenizes Parsing Expression Grammars.
///
/// Based on Bryan Ford's "Parsing Expression Grammars: A Recognition-Based
/// Syntactic Foundation" (2004). Handles identifiers, the `<-` operator,
/// single/double quoted literals with escapes, character classes `[...]`,
/// `#` line comments, and all PEG operators.
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
        // Left arrow: <-
        '<' => {
            if (!self.isAtEnd() and self.peek() == '-') {
                _ = self.advance();
                self.addToken(.left_arrow);
            } else {
                self.addToken(.invalid);
            }
        },

        // Operators
        '/' => self.addToken(.slash),
        '&' => self.addToken(.@"and"),
        '!' => self.addToken(.not),
        '?' => self.addToken(.question),
        '*' => self.addToken(.star),
        '+' => self.addToken(.plus),
        '(' => self.addToken(.left_paren),
        ')' => self.addToken(.right_paren),
        '.' => self.addToken(.dot),

        // Literals
        '\'' => self.scanLiteral('\''),
        '"' => self.scanLiteral('"'),

        // Character class
        '[' => self.scanCharClass(),

        // Comment
        '#' => self.scanComment(),

        // Newlines
        '\r' => {
            _ = self.match('\n');
            self.line += 1;
            self.addToken(.newline);
        },
        '\n' => {
            self.line += 1;
            self.addToken(.newline);
        },

        // Whitespace — skip.
        ' ', '\t' => {},

        // Identifier
        else => {
            if (isIdentStart(c)) {
                self.scanIdentifier();
            } else {
                self.addToken(.invalid);
            }
        },
    }
}

fn scanLiteral(self: *Scanner, quote: u8) void {
    if (!self.isAtEnd() and self.peek() == quote) {
        _ = self.advance(); // consume closing quote
        self.addToken(.invalid); // empty literal
        return;
    }
    while (!self.isAtEnd() and self.peek() != quote) {
        if (self.peek() == '\n' or self.peek() == '\r') {
            self.addToken(.invalid);
            return;
        }
        if (self.peek() == '\\') {
            _ = self.advance(); // consume backslash
            if (!self.isAtEnd()) _ = self.advance(); // consume escaped char
        } else {
            _ = self.advance();
        }
    }
    if (self.isAtEnd()) {
        self.addToken(.invalid);
    } else {
        _ = self.advance(); // consume closing quote
        self.addToken(.literal);
    }
}

fn scanCharClass(self: *Scanner) void {
    while (!self.isAtEnd() and self.peek() != ']') {
        if (self.peek() == '\n' or self.peek() == '\r') {
            self.addToken(.invalid);
            return;
        }
        if (self.peek() == '\\') {
            _ = self.advance(); // consume backslash
            if (!self.isAtEnd()) _ = self.advance(); // consume escaped char
        } else {
            _ = self.advance();
        }
    }
    if (self.isAtEnd()) {
        self.addToken(.invalid);
    } else {
        _ = self.advance(); // consume ]
        self.addToken(.char_class);
    }
}

fn scanComment(self: *Scanner) void {
    while (!self.isAtEnd() and self.peek() != '\n' and self.peek() != '\r') {
        _ = self.advance();
    }
    self.addToken(.comment);
}

fn scanIdentifier(self: *Scanner) void {
    while (!self.isAtEnd() and isIdentCont(self.peek())) {
        _ = self.advance();
    }
    self.addToken(.identifier);
}

// === Predicates ===

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isIdentCont(c: u8) bool {
    return isIdentStart(c) or (c >= '0' and c <= '9');
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

fn match(self: *Scanner, expected: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.source[self.current] != expected) return false;
    self.current += 1;
    return true;
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

test "simple definition" {
    try expectTags("A <- B", &.{ .identifier, .left_arrow, .identifier, .eof });
}

test "ordered choice" {
    try expectTags("A <- B / C", &.{
        .identifier, .left_arrow, .identifier, .slash, .identifier, .eof,
    });
}

test "predicates and suffixes" {
    try expectTags("A <- &B !C D? E* F+", &.{
        .identifier, .left_arrow,
        .@"and",     .identifier,
        .not,        .identifier,
        .identifier, .question,
        .identifier, .star,
        .identifier, .plus,
        .eof,
    });
}

test "grouping" {
    try expectTags("A <- (B C)", &.{
        .identifier, .left_arrow, .left_paren, .identifier, .identifier, .right_paren, .eof,
    });
}

test "dot wildcard" {
    try expectTags("A <- .", &.{ .identifier, .left_arrow, .dot, .eof });
}

test "single-quoted literal" {
    try expectTags("A <- 'hello'", &.{ .identifier, .left_arrow, .literal, .eof });
}

test "double-quoted literal" {
    try expectTags("A <- \"hello\"", &.{ .identifier, .left_arrow, .literal, .eof });
}

test "literal with escape" {
    try expectTags("A <- 'he\\'llo'", &.{ .identifier, .left_arrow, .literal, .eof });
}

test "character class" {
    try expectTags("A <- [a-zA-Z_]", &.{ .identifier, .left_arrow, .char_class, .eof });
}

test "character class with escape" {
    try expectTags("A <- [\\]]", &.{ .identifier, .left_arrow, .char_class, .eof });
}

test "comment" {
    try expectTags("# a comment\nA <- B", &.{
        .comment, .newline, .identifier, .left_arrow, .identifier, .eof,
    });
}

test "multiline" {
    try expectTags("A <- B\nC <- D", &.{
        .identifier, .left_arrow, .identifier, .newline,
        .identifier, .left_arrow, .identifier, .eof,
    });
}

test "unterminated literal" {
    try expectTags("A <- 'oops", &.{ .identifier, .left_arrow, .invalid, .eof });
}

test "unterminated char class" {
    try expectTags("A <- [abc", &.{ .identifier, .left_arrow, .invalid, .eof });
}

test "bare < is invalid" {
    try expectTags("A < B", &.{ .identifier, .invalid, .identifier, .eof });
}

test "quoted bracket literal" {
    try expectTags("A <- '[' B ']'", &.{
        .identifier, .left_arrow, .literal, .identifier, .literal, .eof,
    });
}

test "char class with single quote" {
    try expectTags("A <- ['] B", &.{
        .identifier, .left_arrow, .char_class, .identifier, .eof,
    });
}

test "PEG grammar Class rule" {
    try expectTags(
        "Class <- '[' (!']' Range)* ']' Spacing",
        &.{
            .identifier, .left_arrow,
            .literal, // '['
            .left_paren, .not, .literal, // (!']'
            .identifier, .right_paren, .star, // Range)*
            .literal, // ']'
            .identifier, // Spacing
            .eof,
        },
    );
}
