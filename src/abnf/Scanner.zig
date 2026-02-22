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

    // Append final EOF token.
    self.addToken(.eof);

    return self.tokens[0..self.token_count];
}

fn scanToken(self: *Scanner) void {
    const c = self.advance();
    switch (c) {
        '(' => self.addToken(.left_paren),
        ')' => self.addToken(.right_paren),
        '[' => self.addToken(.left_bracket),
        ']' => self.addToken(.right_bracket),
        '*' => self.addToken(.star),
        '/' => self.addToken(.slash),
        '=' => self.addToken(if (self.match('/')) .equals_slash else .equals),

        // String literals — "..."
        '"' => {
            while (self.peek() != '"' and !self.isAtEnd()) {
                if (self.peek() == '\n') self.line += 1;
                _ = self.advance();
            }
            if (self.isAtEnd()) {
                self.addToken(.invalid); // unterminated string
            } else {
                _ = self.advance(); // consume closing "
                self.addToken(.char_val);
            }
        },

        // Prose values — <...>
        '<' => {
            while (self.peek() != '>' and !self.isAtEnd()) {
                _ = self.advance();
            }
            if (self.isAtEnd()) {
                self.addToken(.invalid); // unterminated prose
            } else {
                _ = self.advance(); // consume closing >
                self.addToken(.prose_val);
            }
        },

        // Numeric values (%b, %d, %x) and case-sensitive/insensitive strings (%s, %i) per RFC 7405.
        '%' => {
            const base = self.peek();
            switch (base) {
                'b' => {
                    _ = self.advance(); // consume 'b'
                    self.consumeDigits(isBit);
                    self.addToken(.bin_val);
                },
                'd' => {
                    _ = self.advance(); // consume 'd'
                    self.consumeDigits(isDigit);
                    self.addToken(.dec_val);
                },
                'x' => {
                    _ = self.advance(); // consume 'x'
                    self.consumeDigits(isHexDigit);
                    self.addToken(.hex_val);
                },
                's', 'i' => {
                    _ = self.advance(); // consume 's' or 'i'
                    if (self.peek() != '"') {
                        self.addToken(.invalid); // %s or %i without opening quote
                    } else {
                        _ = self.advance(); // consume opening "
                        while (self.peek() != '"' and !self.isAtEnd()) {
                            if (self.peek() == '\n') self.line += 1;
                            _ = self.advance();
                        }
                        if (self.isAtEnd()) {
                            self.addToken(.invalid); // unterminated string
                        } else {
                            _ = self.advance(); // consume closing "
                            self.addToken(if (base == 's') .char_val_cs else .char_val_ci);
                        }
                    }
                },
                else => self.addToken(.invalid), // bare % with no base
            }
        },

        // Comments — consume until end of line.
        ';' => {
            while (self.peek() != '\n' and self.peek() != '\r' and !self.isAtEnd()) {
                _ = self.advance();
            }
            self.addToken(.comment);
        },

        // Whitespace — skip silently.
        ' ', '\t' => {},

        // Newlines — emit token and bump line counter.
        '\r' => {
            _ = self.match('\n'); // consume LF after CR (CRLF)
            self.line += 1;
            self.addToken(.newline);
        },
        '\n' => {
            self.line += 1;
            self.addToken(.newline);
        },

        else => {
            if (isAlpha(c)) {
                // Rulename: ALPHA *(ALPHA / DIGIT / "-")
                while (isAlpha(self.peek()) or isDigit(self.peek()) or self.peek() == '-') {
                    _ = self.advance();
                }
                self.addToken(.rulename);
            } else if (isDigit(c)) {
                // Number: 1*DIGIT (used in repetition)
                while (isDigit(self.peek())) _ = self.advance();
                self.addToken(.number);
            } else {
                self.addToken(.invalid);
            }
        },
    }
}

// === Primitive operations ===

/// Consume the current character and return it.
fn advance(self: *Scanner) u8 {
    const c = self.source[self.current];
    self.current += 1;
    return c;
}

/// Look at the current character without consuming it.
/// Returns 0 if at end.
fn peek(self: *Scanner) u8 {
    if (self.isAtEnd()) return 0;
    return self.source[self.current];
}

/// Conditional advance: if current char matches `expected`, consume it
/// and return true. Otherwise return false.
fn match(self: *Scanner, expected: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.source[self.current] != expected) return false;
    self.current += 1;
    return true;
}

/// Consume digits for a numeric value, including "." and "-" continuations.
/// e.g. for hex: "41" or "41.42.43" or "41-5A"
fn consumeDigits(self: *Scanner, isValidDigit: *const fn (u8) bool) void {
    // Consume first group of digits.
    while (isValidDigit(self.peek())) _ = self.advance();

    // Check for "." (concatenation) or "-" (range) continuation.
    if (self.peek() == '.') {
        // Dot-separated: %x41.42.43
        while (self.peek() == '.') {
            _ = self.advance(); // consume '.'
            while (isValidDigit(self.peek())) _ = self.advance();
        }
    } else if (self.peek() == '-') {
        // Range: %x41-5A
        _ = self.advance(); // consume '-'
        while (isValidDigit(self.peek())) _ = self.advance();
    }
}

fn isAtEnd(self: *Scanner) bool {
    return self.current >= self.source.len;
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isHexDigit(c: u8) bool {
    return isDigit(c) or (c >= 'A' and c <= 'F') or (c >= 'a' and c <= 'f');
}

fn isBit(c: u8) bool {
    return c == '0' or c == '1';
}

fn isAlpha(c: u8) bool {
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z');
}

/// Append a token with the current lexeme span.
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
    try expectTags("foo = bar", &.{ .rulename, .equals, .rulename, .eof });
}

test "alternation" {
    try expectTags("a = b / c", &.{
        .rulename, .equals, .rulename, .slash, .rulename, .eof,
    });
}

test "repetition prefixes" {
    try expectTags("a = 3*5b", &.{
        .rulename, .equals, .number, .star, .number, .rulename, .eof,
    });
}

test "numeric values" {
    try expectTags("a = %x41-5A / %d65.66 / %b0101", &.{
        .rulename, .equals, .hex_val, .slash, .dec_val, .slash, .bin_val, .eof,
    });
}

test "groups and options" {
    try expectTags("a = (b / c) [d]", &.{
        .rulename,    .equals,       .left_paren, .rulename,      .slash, .rulename,
        .right_paren, .left_bracket, .rulename,   .right_bracket, .eof,
    });
}

test "char val and prose val" {
    try expectTags(
        \\a = "hello" <world>
    , &.{ .rulename, .equals, .char_val, .prose_val, .eof });
}

test "comment" {
    try expectTags("a = b ; comment", &.{
        .rulename, .equals, .rulename, .comment, .eof,
    });
}

test "case-sensitive string (RFC 7405)" {
    try expectTags(
        \\a = %s"Hello"
    , &.{ .rulename, .equals, .char_val_cs, .eof });
}

test "case-insensitive string (RFC 7405)" {
    try expectTags(
        \\a = %i"Hello"
    , &.{ .rulename, .equals, .char_val_ci, .eof });
}

test "incremental alternation" {
    try expectTags("a =/ b", &.{ .rulename, .equals_slash, .rulename, .eof });
}

test "multiline" {
    try expectTags("a = b\nc = d", &.{
        .rulename, .equals, .rulename, .newline,
        .rulename, .equals, .rulename, .newline,
        .eof,
    });
}
