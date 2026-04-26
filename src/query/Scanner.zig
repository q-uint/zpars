/// Scanner for tree-sitter-style query files.
///
/// Token shapes:
///   `(` `)` `[` `]` `.` `?` `*` `+`
///   `@ident`            capture
///   `#ident` / `#ident?` / `#ident!`   predicate head
///   `"..."`             quoted string (supports `\\`, `\"`, `\n`, `\t`, `\r`, `\0`)
///   `bare-ident`        rule name / reserved word (`_`, `ERROR`, `MISSING`, `partial`)
///   `; ...`             line comment, terminated by newline
///
/// Identifiers accept `[A-Za-z0-9_-]`; the first character must be alpha
/// or `_`. Whitespace separates tokens but is otherwise ignored.
const std = @import("std");
const Token = @import("Token.zig").Token;
const char_flags = @import("../char_flags.zig");

pub const Error = error{OutOfMemory};

pub const Scanner = struct {
    source: []const u8,
    tokens: std.ArrayList(Token) = .empty,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    pub fn init(source: []const u8) Scanner {
        return .{ .source = source };
    }

    pub fn deinit(self: *Scanner, allocator: std.mem.Allocator) void {
        self.tokens.deinit(allocator);
    }

    /// Scan the entire source and return the resulting token slice. The
    /// scanner owns the slice via `self.tokens` until `deinit` is called.
    pub fn scanTokens(self: *Scanner, allocator: std.mem.Allocator) Error![]const Token {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken(allocator);
        }
        try self.addToken(allocator, .eof);
        return self.tokens.items;
    }

    fn scanToken(self: *Scanner, allocator: std.mem.Allocator) Error!void {
        const c = self.advance();
        switch (c) {
            ' ', '\t', '\r' => {},
            '\n' => self.line += 1,
            '(' => try self.addToken(allocator, .lparen),
            ')' => try self.addToken(allocator, .rparen),
            '[' => try self.addToken(allocator, .lbracket),
            ']' => try self.addToken(allocator, .rbracket),
            '.' => try self.addToken(allocator, .dot),
            '?' => try self.addToken(allocator, .question),
            '*' => try self.addToken(allocator, .star),
            '+' => try self.addToken(allocator, .plus),
            ':' => try self.addToken(allocator, .colon),
            ';' => try self.scanComment(allocator),
            '"' => try self.scanString(allocator),
            '@' => try self.scanSigil(allocator, .at_identifier),
            '#' => try self.scanPredicate(allocator),
            '_' => try self.scanIdentifier(allocator),
            else => {
                if (char_flags.isIdentStart(c)) {
                    try self.scanIdentifier(allocator);
                } else {
                    try self.addToken(allocator, .invalid);
                }
            },
        }
    }

    fn scanComment(self: *Scanner, allocator: std.mem.Allocator) Error!void {
        while (!self.isAtEnd() and self.peek() != '\n') _ = self.advance();
        try self.addToken(allocator, .comment);
    }

    fn scanString(self: *Scanner, allocator: std.mem.Allocator) Error!void {
        while (!self.isAtEnd()) {
            const c = self.peek();
            if (c == '"') {
                _ = self.advance();
                try self.addToken(allocator, .string);
                return;
            }
            if (c == '\n') {
                try self.addToken(allocator, .invalid);
                return;
            }
            if (c == '\\') {
                _ = self.advance();
                if (self.isAtEnd()) {
                    try self.addToken(allocator, .invalid);
                    return;
                }
                _ = self.advance();
                continue;
            }
            _ = self.advance();
        }
        try self.addToken(allocator, .invalid);
    }

    /// Scan `@ident` -- the leading sigil has already been consumed.
    fn scanSigil(self: *Scanner, allocator: std.mem.Allocator, tag: Token.Tag) Error!void {
        if (self.isAtEnd() or !isIdentBodyStart(self.peek())) {
            try self.addToken(allocator, .invalid);
            return;
        }
        while (!self.isAtEnd() and isIdentBody(self.peek())) _ = self.advance();
        try self.addToken(allocator, tag);
    }

    /// Scan `#ident`, `#ident?`, or `#ident!` -- the `#` has already been consumed.
    fn scanPredicate(self: *Scanner, allocator: std.mem.Allocator) Error!void {
        if (self.isAtEnd() or !isIdentBodyStart(self.peek())) {
            try self.addToken(allocator, .invalid);
            return;
        }
        while (!self.isAtEnd() and isIdentBody(self.peek())) _ = self.advance();
        if (!self.isAtEnd()) {
            const c = self.peek();
            if (c == '?' or c == '!') _ = self.advance();
        }
        try self.addToken(allocator, .predicate);
    }

    fn scanIdentifier(self: *Scanner, allocator: std.mem.Allocator) Error!void {
        while (!self.isAtEnd() and isIdentBody(self.peek())) _ = self.advance();
        try self.addToken(allocator, .identifier);
    }

    fn isIdentBodyStart(c: u8) bool {
        return char_flags.isIdentStart(c) or c == '_';
    }

    fn isIdentBody(c: u8) bool {
        return char_flags.isIdentCont(c) or c == '-';
    }

    fn advance(self: *Scanner) u8 {
        const c = self.source[self.current];
        self.current += 1;
        return c;
    }

    fn peek(self: *const Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn isAtEnd(self: *const Scanner) bool {
        return self.current >= self.source.len;
    }

    fn addToken(self: *Scanner, allocator: std.mem.Allocator, tag: Token.Tag) Error!void {
        try self.tokens.append(allocator, .{
            .tag = tag,
            .start = self.start,
            .len = self.current - self.start,
            .line = self.line,
        });
    }
};

const testing = std.testing;

fn expectTags(source: []const u8, expected: []const Token.Tag) !void {
    var scanner = Scanner.init(source);
    defer scanner.deinit(testing.allocator);
    const toks = try scanner.scanTokens(testing.allocator);
    try testing.expectEqual(expected.len, toks.len);
    for (toks, expected) |got, want| try testing.expectEqual(want, got.tag);
}

test "punctuation and quantifiers" {
    try expectTags("()[]. ? * +", &.{
        .lparen, .rparen, .lbracket, .rbracket, .dot, .question, .star, .plus, .eof,
    });
}

test "identifier, capture, predicate, string" {
    try expectTags(
        \\(Foo @bar (#eq? @bar "x"))
    , &.{
        .lparen, .identifier, .at_identifier,
        .lparen, .predicate,  .at_identifier,
        .string, .rparen,     .rparen,
        .eof,
    });
}

test "underscore wildcard tokenizes as identifier" {
    var scanner = Scanner.init("_");
    defer scanner.deinit(testing.allocator);
    const toks = try scanner.scanTokens(testing.allocator);
    try testing.expectEqual(Token.Tag.identifier, toks[0].tag);
    try testing.expectEqualStrings("_", toks[0].lexeme("_"));
}

test "predicate keeps trailing ? in lexeme" {
    const src = "#match?";
    var scanner = Scanner.init(src);
    defer scanner.deinit(testing.allocator);
    const toks = try scanner.scanTokens(testing.allocator);
    try testing.expectEqual(Token.Tag.predicate, toks[0].tag);
    try testing.expectEqualStrings("#match?", toks[0].lexeme(src));
}

test "capture keeps leading @ in lexeme" {
    const src = "@name";
    var scanner = Scanner.init(src);
    defer scanner.deinit(testing.allocator);
    const toks = try scanner.scanTokens(testing.allocator);
    try testing.expectEqualStrings("@name", toks[0].lexeme(src));
}

test "string with escapes" {
    const src =
        \\"a\"b\nc"
    ;
    var scanner = Scanner.init(src);
    defer scanner.deinit(testing.allocator);
    const toks = try scanner.scanTokens(testing.allocator);
    try testing.expectEqual(Token.Tag.string, toks[0].tag);
    try testing.expectEqualStrings(src, toks[0].lexeme(src));
}

test "unterminated string is invalid" {
    try expectTags("\"abc", &.{ .invalid, .eof });
}

test "string with literal newline is invalid" {
    try expectTags("\"a\nb\"", &.{ .invalid, .identifier, .invalid, .eof });
}

test "comment runs to end of line" {
    try expectTags("(Foo) ; trailing comment\n(Bar)", &.{
        .lparen, .identifier, .rparen, .comment,
        .lparen, .identifier, .rparen, .eof,
    });
}

test "stray @ without name is invalid" {
    try expectTags("@", &.{ .invalid, .eof });
}

test "identifier with hyphen" {
    try expectTags("not-eq", &.{ .identifier, .eof });
}

test "line tracking" {
    const src = "(\n  Foo\n)";
    var scanner = Scanner.init(src);
    defer scanner.deinit(testing.allocator);
    const toks = try scanner.scanTokens(testing.allocator);
    try testing.expectEqual(@as(usize, 1), toks[0].line); // (
    try testing.expectEqual(@as(usize, 2), toks[1].line); // Foo
    try testing.expectEqual(@as(usize, 3), toks[2].line); // )
}
