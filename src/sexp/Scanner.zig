/// S-expression scanner - tokenizes RFC 9804 S-expressions.
///
/// Supports the advanced transport representation which is a superset
/// of canonical and basic transport. Handles verbatim (length-prefixed),
/// quoted strings with C-style escapes, tokens (barewords), hexadecimal
/// `#...#`, base-64 `|...|`, display hints `[...]`, and base-64 wrapped
/// S-expressions `{...}`.
const std = @import("std");
const Token = @import("Token.zig").Token;

const Scanner = @This();

source: []const u8,
current: usize = 0,
line: usize = 1,

pub fn init(source: []const u8) Scanner {
    return .{ .source = source };
}

/// Returns the next token from the source.
pub fn next(self: *Scanner) Token {
    self.skipWhitespace();
    if (self.isAtEnd()) return self.makeToken(.eof, self.current, 0);

    const start = self.current;
    const c = self.advance();

    return switch (c) {
        '(' => self.makeToken(.lparen, start, 1),
        ')' => self.makeToken(.rparen, start, 1),
        '[' => self.makeToken(.lbracket, start, 1),
        ']' => self.makeToken(.rbracket, start, 1),
        '{' => self.scanBraceBase64(start),
        '"' => self.scanQuotedString(start),
        '#' => self.scanHexadecimal(start),
        '|' => self.scanBase64(start),
        '0' => self.scanAfterZero(start),
        '1'...'9' => self.scanAfterDigit(start),
        else => if (isTokenStart(c))
            self.scanToken(start)
        else
            self.makeToken(.invalid, start, 1),
    };
}

/// After seeing '0': either `0:` (empty verbatim) or `0` followed by
/// a non-digit (which is invalid as a standalone decimal in most contexts).
fn scanAfterZero(self: *Scanner, start: usize) Token {
    if (!self.isAtEnd() and self.peek() == ':') {
        _ = self.advance(); // consume ':'
        return self.scanVerbatimData(start, 0);
    }
    // Bare '0' - emit as decimal.
    return self.makeToken(.decimal, start, 1);
}

/// After seeing '1'-'9': scan remaining digits, then decide based on
/// what follows: `:` -> verbatim, `"` -> length-prefixed quoted,
/// `#` -> length-prefixed hex, `|` -> length-prefixed base64.
fn scanAfterDigit(self: *Scanner, start: usize) Token {
    while (!self.isAtEnd() and isDigit(self.peek())) _ = self.advance();

    if (self.isAtEnd()) return self.makeToken(.decimal, start, self.current - start);

    switch (self.peek()) {
        ':' => {
            const len = self.parseDecimal(start);
            _ = self.advance(); // consume ':'
            return self.scanVerbatimData(start, len);
        },
        '"' => {
            // Length-prefixed quoted string: emit the whole thing as quoted_string.
            _ = self.advance(); // consume '"'
            return self.scanQuotedStringBody(start);
        },
        '#' => {
            _ = self.advance(); // consume '#'
            return self.scanHexBody(start);
        },
        '|' => {
            _ = self.advance(); // consume '|'
            return self.scanBase64Body(start);
        },
        else => return self.makeToken(.decimal, start, self.current - start),
    }
}

/// Consume exactly `len` bytes of verbatim data after the colon.
fn scanVerbatimData(self: *Scanner, start: usize, len: usize) Token {
    const avail = self.source.len - self.current;
    if (avail < len) {
        // Not enough data - consume what's left, mark invalid.
        self.current = self.source.len;
        return self.makeToken(.invalid, start, self.current - start);
    }
    // Count newlines in verbatim data for line tracking.
    for (self.source[self.current .. self.current + len]) |b| {
        if (b == '\n') self.line += 1;
    }
    self.current += len;
    return self.makeToken(.verbatim, start, self.current - start);
}

/// Scan a quoted string starting after the opening `"`.
fn scanQuotedString(self: *Scanner, start: usize) Token {
    return self.scanQuotedStringBody(start);
}

/// Shared body for quoted strings (with or without length prefix).
fn scanQuotedStringBody(self: *Scanner, start: usize) Token {
    while (!self.isAtEnd()) {
        const c = self.peek();
        if (c == '"') {
            _ = self.advance();
            return self.makeToken(.quoted_string, start, self.current - start);
        }
        if (c == '\\') {
            _ = self.advance(); // consume backslash
            if (!self.isAtEnd()) {
                const esc = self.advance();
                // Line continuation: \<newline> sequences.
                if (esc == '\r' and !self.isAtEnd() and self.peek() == '\n') {
                    self.line += 1;
                    _ = self.advance();
                } else if (esc == '\n') {
                    self.line += 1;
                    if (!self.isAtEnd() and self.peek() == '\r') _ = self.advance();
                } else if (esc == '\r') {
                    self.line += 1;
                }
            }
        } else {
            if (c == '\n') self.line += 1;
            _ = self.advance();
        }
    }
    // Unterminated.
    return self.makeToken(.invalid, start, self.current - start);
}

/// Scan hexadecimal `#...#` (opening `#` already consumed).
fn scanHexadecimal(self: *Scanner, start: usize) Token {
    return self.scanHexBody(start);
}

/// Shared body for hex (with or without length prefix).
fn scanHexBody(self: *Scanner, start: usize) Token {
    var valid = true;
    while (!self.isAtEnd()) {
        const c = self.peek();
        if (c == '#') {
            _ = self.advance();
            return self.makeToken(if (valid) .hexadecimal else .invalid, start, self.current - start);
        }
        if (isHexDigit(c) or isWhitespaceChar(c)) {
            if (c == '\n') self.line += 1;
        } else {
            valid = false;
        }
        _ = self.advance();
    }
    return self.makeToken(.invalid, start, self.current - start);
}

/// Scan base-64 `|...|` (opening `|` already consumed).
fn scanBase64(self: *Scanner, start: usize) Token {
    return self.scanBase64Body(start);
}

/// Shared body for base64 (with or without length prefix).
fn scanBase64Body(self: *Scanner, start: usize) Token {
    while (!self.isAtEnd()) {
        const c = self.peek();
        if (c == '|') {
            _ = self.advance();
            return self.makeToken(.base64, start, self.current - start);
        }
        if (isBase64Char(c) or c == '=' or isWhitespaceChar(c)) {
            if (c == '\n') self.line += 1;
            _ = self.advance();
        } else {
            _ = self.advance();
            return self.makeToken(.invalid, start, self.current - start);
        }
    }
    return self.makeToken(.invalid, start, self.current - start);
}

/// Scan base-64 wrapped S-expression `{...}` (opening `{` already consumed).
fn scanBraceBase64(self: *Scanner, start: usize) Token {
    while (!self.isAtEnd()) {
        const c = self.peek();
        if (c == '}') {
            _ = self.advance();
            return self.makeToken(.lbrace, start, self.current - start);
        }
        if (isBase64Char(c) or c == '=' or isWhitespaceChar(c)) {
            if (c == '\n') self.line += 1;
            _ = self.advance();
        } else {
            _ = self.advance();
            return self.makeToken(.invalid, start, self.current - start);
        }
    }
    return self.makeToken(.invalid, start, self.current - start);
}

/// Scan a token (bareword). First character already consumed.
fn scanToken(self: *Scanner, start: usize) Token {
    while (!self.isAtEnd() and isTokenCont(self.peek())) _ = self.advance();
    return self.makeToken(.sexp_token, start, self.current - start);
}

pub fn skipWhitespace(self: *Scanner) void {
    while (!self.isAtEnd()) {
        const c = self.peek();
        switch (c) {
            ' ', '\t', '\x0B', '\x0C', '\r' => self.current += 1,
            '\n' => {
                self.current += 1;
                self.line += 1;
            },
            else => return,
        }
    }
}

fn advance(self: *Scanner) u8 {
    const c = self.source[self.current];
    self.current += 1;
    return c;
}

fn peek(self: *Scanner) u8 {
    if (self.isAtEnd()) return 0;
    return self.source[self.current];
}

fn isAtEnd(self: *Scanner) bool {
    return self.current >= self.source.len;
}

fn makeToken(self: *Scanner, tag: Token.Tag, start: usize, len: usize) Token {
    return .{ .tag = tag, .start = start, .len = len, .line = self.line };
}

fn parseDecimal(self: *Scanner, start: usize) usize {
    const digits = self.source[start..self.current];
    var result: usize = 0;
    for (digits) |d| {
        result = result * 10 + (d - '0');
    }
    return result;
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

pub fn isHexDigit(c: u8) bool {
    return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

fn isBase64Char(c: u8) bool {
    return (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z') or (c >= '0' and c <= '9') or c == '+' or c == '/';
}

pub fn isWhitespaceChar(c: u8) bool {
    return c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == '\x0B' or c == '\x0C';
}

/// RFC 9804: token starts with ALPHA or simple-punc.
pub fn isTokenStart(c: u8) bool {
    return isAlpha(c) or isSimplePunc(c);
}

/// RFC 9804: token continues with ALPHA, DIGIT, or simple-punc.
pub fn isTokenCont(c: u8) bool {
    return isAlpha(c) or isDigit(c) or isSimplePunc(c);
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
}

/// RFC 9804: simple-punc = "-" / "." / "/" / "_" / ":" / "*" / "+" / "="
fn isSimplePunc(c: u8) bool {
    return switch (c) {
        '-', '.', '/', '_', ':', '*', '+', '=' => true,
        else => false,
    };
}

fn expectTokens(source: []const u8, expected: []const Token.Tag) !void {
    var scanner = Scanner.init(source);
    const allocator = std.testing.allocator;
    var actual: std.ArrayList(Token.Tag) = .empty;
    defer actual.deinit(allocator);
    while (true) {
        const tok = scanner.next();
        try actual.append(allocator, tok.tag);
        if (tok.tag == .eof) break;
    }
    try std.testing.expectEqualSlices(Token.Tag, expected, actual.items);
}

test "empty input" {
    try expectTokens("", &.{.eof});
}

test "parentheses" {
    try expectTokens("()", &.{ .lparen, .rparen, .eof });
}

test "nested parens" {
    try expectTokens("(())", &.{ .lparen, .lparen, .rparen, .rparen, .eof });
}

test "token (bareword)" {
    try expectTokens("subject", &.{ .sexp_token, .eof });
}

test "token with punc" {
    try expectTokens("not-before", &.{ .sexp_token, .eof });
}

test "tokens in list" {
    try expectTokens("(a b c)", &.{ .lparen, .sexp_token, .sexp_token, .sexp_token, .rparen, .eof });
}

test "quoted string" {
    try expectTokens("\"hello\"", &.{ .quoted_string, .eof });
}

test "quoted string with escape" {
    try expectTokens("\"hello\\nworld\"", &.{ .quoted_string, .eof });
}

test "verbatim" {
    try expectTokens("3:abc", &.{ .verbatim, .eof });
}

test "verbatim empty" {
    try expectTokens("0:", &.{ .verbatim, .eof });
}

test "verbatim in list" {
    try expectTokens("(1:a1:b1:c)", &.{ .lparen, .verbatim, .verbatim, .verbatim, .rparen, .eof });
}

test "hexadecimal" {
    try expectTokens("#616263#", &.{ .hexadecimal, .eof });
}

test "hexadecimal empty" {
    try expectTokens("##", &.{ .hexadecimal, .eof });
}

test "hexadecimal with whitespace" {
    try expectTokens("# 61 62 63 #", &.{ .hexadecimal, .eof });
}

test "base64" {
    try expectTokens("|YWJj|", &.{ .base64, .eof });
}

test "base64 with padding" {
    try expectTokens("|YWJjZA==|", &.{ .base64, .eof });
}

test "display hint" {
    try expectTokens("[3:abc]\"data\"", &.{ .lbracket, .verbatim, .rbracket, .quoted_string, .eof });
}

test "length-prefixed quoted" {
    try expectTokens("7\"subject\"", &.{ .quoted_string, .eof });
}

test "length-prefixed hex" {
    try expectTokens("3#616263#", &.{ .hexadecimal, .eof });
}

test "length-prefixed base64" {
    try expectTokens("3|YWJj|", &.{ .base64, .eof });
}

test "base64 wrapped sexp" {
    try expectTokens("{KDE6YTE6YjE6Yyk=}", &.{ .lbrace, .eof });
}

test "verbatim lexeme" {
    var scanner = Scanner.init("3:abc rest");
    const tok = scanner.next();
    try std.testing.expectEqual(Token.Tag.verbatim, tok.tag);
    try std.testing.expectEqualStrings("3:abc", tok.lexeme(scanner.source));
}

test "mixed expression" {
    try expectTokens(
        \\(issuer "bob" #616263# |YWJj|)
    , &.{
        .lparen,
        .sexp_token, // issuer
        .quoted_string, // "bob"
        .hexadecimal, // #616263#
        .base64, // |YWJj|
        .rparen,
        .eof,
    });
}

// §4.2 Token - pseudo-alphabetic start characters
test "token starting with colon" {
    try expectTokens(":=..", &.{ .sexp_token, .eof });
}

test "token starting with slash" {
    try expectTokens("//example.net/names/smith", &.{ .sexp_token, .eof });
}

test "token single star" {
    try expectTokens("*", &.{ .sexp_token, .eof });
}

test "token class-of-1997 (digit after non-digit start)" {
    try expectTokens("class-of-1997", &.{ .sexp_token, .eof });
}

// §4.2 Token - digits cannot start a token (they start verbatim/decimal)
test "digit start is not a token" {
    // '1abc' - '1' starts a decimal scan, not a token
    try expectTokens("1abc", &.{ .decimal, .sexp_token, .eof });
}

// §4.1 Verbatim - binary data pass-through
test "verbatim with special characters" {
    // 4:::": is verbatim for ::\"
    var scanner = Scanner.init("4:::\":");
    const tok = scanner.next();
    try std.testing.expectEqual(Token.Tag.verbatim, tok.tag);
    try std.testing.expectEqualStrings("4:::\":", tok.lexeme(scanner.source));
}

test "verbatim multi-digit length" {
    var scanner = Scanner.init("10:abcdefghij");
    const tok = scanner.next();
    try std.testing.expectEqual(Token.Tag.verbatim, tok.tag);
    try std.testing.expectEqualStrings("10:abcdefghij", tok.lexeme(scanner.source));
}

test "verbatim truncated input" {
    // Claims 5 bytes but only 3 available
    try expectTokens("5:abc", &.{ .invalid, .eof });
}

// §4.3 Quoted string - empty
test "quoted string empty" {
    try expectTokens("\"\"", &.{ .quoted_string, .eof });
}

// §4.3 Quoted string - unterminated
test "quoted string unterminated" {
    try expectTokens("\"oops", &.{ .invalid, .eof });
}

// §4.3 Quoted string - line continuation
test "quoted string with line continuation" {
    try expectTokens("\"hello\\\nworld\"", &.{ .quoted_string, .eof });
}

test "quoted string with CRLF continuation" {
    try expectTokens("\"hello\\\r\nworld\"", &.{ .quoted_string, .eof });
}

// §4.4 Hexadecimal - newlines as whitespace inside hex
test "hexadecimal with newlines" {
    try expectTokens("# 61\n62\n63 #", &.{ .hexadecimal, .eof });
}

// §4.4 Hexadecimal - unterminated
test "hexadecimal unterminated" {
    try expectTokens("#6162", &.{ .invalid, .eof });
}

// §4.4 Hexadecimal - invalid character inside
test "hexadecimal invalid char" {
    try expectTokens("#61GZ#", &.{ .invalid, .eof });
}

// §4.5 Base-64 - with whitespace
test "base64 with internal whitespace" {
    try expectTokens("| Y W J j |", &.{ .base64, .eof });
}

// §4.5 Base-64 - empty
test "base64 empty" {
    try expectTokens("||", &.{ .base64, .eof });
}

// §4.5 Base-64 - unterminated
test "base64 unterminated" {
    try expectTokens("|YWJj", &.{ .invalid, .eof });
}

// §4.5 Base-64 - padding dropped on input (MAY accept)
test "base64 without padding" {
    try expectTokens("|YWJjZA|", &.{ .base64, .eof });
}

// §6 Basic transport - brace-wrapped with internal whitespace
test "brace base64 with whitespace" {
    try expectTokens("{ KDE6YTE6YjE6Yyk= }", &.{ .lbrace, .eof });
}

// §6 Brace - unterminated
test "brace base64 unterminated" {
    try expectTokens("{KDE6YTE6", &.{ .invalid, .eof });
}

// §3 Display hints - whitespace around hint
test "display hint with whitespace" {
    try expectTokens("[ 3:gif ]\"data\"", &.{ .lbracket, .verbatim, .rbracket, .quoted_string, .eof });
}

// §3 Display hints - quoted hint
test "display hint with quoted string" {
    try expectTokens("[\"image/gif\"]\"data\"", &.{ .lbracket, .quoted_string, .rbracket, .quoted_string, .eof });
}

// Whitespace - all RFC 9804 whitespace characters
test "all whitespace types skipped" {
    // SP, HTAB, VTAB, FF, CR, LF
    try expectTokens(" \t\x0B\x0C\r\nabc", &.{ .sexp_token, .eof });
}

// Canonical - no whitespace between elements
test "canonical list no spaces" {
    try expectTokens("(6:issuer3:bob)", &.{
        .lparen,
        .verbatim, // 6:issuer
        .verbatim, // 3:bob
        .rparen,
        .eof,
    });
}

// Complex RFC example
test "RFC example: snicker abc list" {
    try expectTokens(
        \\(snicker "abc" (#03# |YWJj|))
    , &.{
        .lparen,
        .sexp_token, // snicker
        .quoted_string, // "abc"
        .lparen,
        .hexadecimal, // #03#
        .base64, // |YWJj|
        .rparen,
        .rparen,
        .eof,
    });
}
