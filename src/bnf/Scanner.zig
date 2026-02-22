/// BNF scanner — tokenizes BNF grammars (ALGOL 60 variant).
///
/// Based on the formalism in Section 1.1 of the Revised Report on the
/// Algorithmic Language ALGOL 60 (1963). The notation uses three
/// meta-symbols: `<>` (metalinguistic variable), `::=` (definition),
/// and `|` (alternation). All other marks are terminal literals.
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
        // Non-terminal: <name>
        '<' => {
            while (!self.isAtEnd() and self.peek() != '>') {
                if (self.peek() == '\n' or self.peek() == '\r') {
                    self.addToken(.invalid);
                    return;
                }
                _ = self.advance();
            }
            if (self.isAtEnd()) {
                self.addToken(.invalid);
            } else {
                _ = self.advance(); // consume >
                self.addToken(.rulename);
            }
        },

        // Alternation
        '|' => self.addToken(.pipe),

        // Definition: ::=
        ':' => {
            if (self.current + 1 < self.source.len and
                self.source[self.current] == ':' and
                self.source[self.current + 1] == '=')
            {
                self.current += 2;
                self.addToken(.definition);
            } else {
                // Bare colon — treat as terminal
                self.scanTerminal();
            }
        },

        // Whitespace — skip silently.
        ' ', '\t' => {},

        // Newlines
        '\r' => {
            _ = self.match('\n'); // consume LF after CR (CRLF)
            self.line += 1;
            self.addToken(.newline);
        },
        '\n' => {
            self.line += 1;
            self.addToken(.newline);
        },

        // Everything else is a terminal character.
        else => self.scanTerminal(),
    }
}

/// Continue scanning terminal characters from the current position.
/// The first character has already been consumed by scanToken.
fn scanTerminal(self: *Scanner) void {
    while (!self.isAtEnd()) {
        const c = self.peek();
        // Stop at meta-symbols, whitespace, and newlines.
        if (c == '<' or c == '|' or c == ' ' or c == '\t' or c == '\n' or c == '\r') break;
        // Stop at ::= (definition).
        if (c == ':' and self.current + 2 <= self.source.len and
            self.source[self.current + 1] == ':' and
            self.source[self.current + 2] == '=') break;
        _ = self.advance();
    }
    self.addToken(.terminal);
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

test "simple rule" {
    try expectTags("<digit> ::= 0", &.{ .rulename, .definition, .terminal, .eof });
}

test "alternation" {
    try expectTags("<bit> ::= 0 | 1", &.{
        .rulename, .definition, .terminal, .pipe, .terminal, .eof,
    });
}

test "non-terminal reference" {
    try expectTags("<ab> ::= <a> <b>", &.{
        .rulename, .definition, .rulename, .rulename, .eof,
    });
}

test "multi-char terminal" {
    try expectTags("<kw> ::= while", &.{ .rulename, .definition, .terminal, .eof });
}

test "multiline" {
    try expectTags("<a> ::= x\n<b> ::= y", &.{
        .rulename, .definition, .terminal, .newline,
        .rulename, .definition, .terminal, .newline,
        .eof,
    });
}

test "empty rule" {
    try expectTags("<empty> ::=", &.{ .rulename, .definition, .eof });
}

test "ALGOL 60 example" {
    try expectTags("<ab> ::= ( | [ | <ab> ( | <ab> <d>", &.{
        .rulename,  .definition,
        .terminal,  .pipe,
        .terminal,  .pipe,
        .rulename,  .terminal, .pipe,
        .rulename,  .rulename,
        .eof,
    });
}

test "bare colon as terminal" {
    try expectTags("<x> ::= a:b", &.{ .rulename, .definition, .terminal, .eof });
}

test "unterminated rulename" {
    try expectTags("<oops", &.{ .invalid, .eof });
}
