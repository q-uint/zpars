/// CFG scanner -- tokenizes CFG grammar notation.
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
const char_flags = @import("../char_flags.zig");

pub const Config = struct {
    max_tokens: usize = 4096,
};

pub const Scanner = ScannerWith(.{});

pub fn ScannerWith(comptime config: Config) type {
    return struct {
        const Self = @This();

        source: []const u8,
        tokens: [config.max_tokens]Token = undefined,
        token_count: usize = 0,
        start: usize = 0,
        current: usize = 0,
        line: usize = 1,

        pub fn init(source: []const u8) Self {
            return .{ .source = source };
        }

        pub fn scanTokens(self: *Self) []const Token {
            while (!self.isAtEnd()) {
                self.start = self.current;
                self.scanToken();
            }
            self.addToken(.eof);
            return self.tokens[0..self.token_count];
        }

        const Action = union(enum) {
            single: Token.Tag,
            skip,
            handler: *const fn (*Self) void,
            invalid,
        };

        const dispatch_table: [256]Action = blk: {
            var t: [256]Action = @splat(.invalid);

            t['|'] = .{ .single = .pipe };

            t[' '] = .skip;
            t['\t'] = .skip;
            t['\r'] = .{ .handler = scanNewlineCR };

            t['/'] = .{ .handler = scanSlash };
            t['-'] = .{ .handler = scanDash };
            t['"'] = .{ .handler = scanDoubleQuote };
            t['%'] = .{ .handler = scanPercent };
            t['\n'] = .{ .handler = scanNewlineLF };

            for ('A'..('Z' + 1)) |c| t[c] = .{ .handler = scanIdentifier };
            for ('a'..('z' + 1)) |c| t[c] = .{ .handler = scanIdentifier };
            t['_'] = .{ .handler = scanIdentifier };

            break :blk t;
        };

        fn scanToken(self: *Self) void {
            const c = self.advance();
            switch (dispatch_table[c]) {
                .single => |tag| self.addToken(tag),
                .skip => {},
                .handler => |func| func(self),
                .invalid => self.addToken(.invalid),
            }
        }

        fn scanSlash(self: *Self) void {
            if (self.peek() == '/') {
                while (!self.isAtEnd() and self.peek() != '\n') _ = self.advance();
            } else {
                self.addToken(.invalid);
            }
        }

        fn scanDash(self: *Self) void {
            if (self.peek() == '>') {
                _ = self.advance();
                self.addToken(.arrow);
            } else {
                self.addToken(.invalid);
            }
        }

        fn scanDoubleQuote(self: *Self) void {
            self.scanString(.string);
        }

        fn scanString(self: *Self, tag: Token.Tag) void {
            if (!self.isAtEnd() and self.peek() == '"') {
                _ = self.advance();
                self.addToken(.invalid);
                return;
            }
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
                _ = self.advance();
                self.addToken(tag);
            }
        }

        fn scanPercent(self: *Self) void {
            if (self.isAtEnd()) {
                self.addToken(.invalid);
                return;
            }
            const c = self.peek();
            if (c == 'x' or c == 'X') {
                _ = self.advance();
                self.scanHex();
            } else if (c == 's' and self.peekNext() == '"') {
                _ = self.advance();
                _ = self.advance();
                self.scanString(.string_cs);
            } else if (c == 'i' and self.peekNext() == '"') {
                _ = self.advance();
                _ = self.advance();
                self.scanString(.string_ci);
            } else {
                self.addToken(.invalid);
            }
        }

        fn scanHex(self: *Self) void {
            if (self.isAtEnd() or !char_flags.isHexDigit(self.peek())) {
                self.addToken(.invalid);
                return;
            }
            while (!self.isAtEnd() and char_flags.isHexDigit(self.peek())) _ = self.advance();

            if (!self.isAtEnd() and self.peek() == '-') {
                _ = self.advance();
                if (self.isAtEnd() or !char_flags.isHexDigit(self.peek())) {
                    self.addToken(.invalid);
                    return;
                }
                while (!self.isAtEnd() and char_flags.isHexDigit(self.peek())) _ = self.advance();
                self.addToken(.hex_range);
            } else {
                self.addToken(.hex_byte);
            }
        }

        fn scanIdentifier(self: *Self) void {
            while (!self.isAtEnd() and (char_flags.isIdentCont(self.peek()) or self.peek() == '-')) _ = self.advance();
            self.addToken(.identifier);
        }

        fn scanNewlineCR(self: *Self) void {
            if (!self.isAtEnd() and self.peek() == '\n') self.current += 1;
            self.line += 1;
            self.addToken(.newline);
        }

        fn scanNewlineLF(self: *Self) void {
            self.line += 1;
            self.addToken(.newline);
        }

        fn advance(self: *Self) u8 {
            const c = self.source[self.current];
            self.current += 1;
            return c;
        }

        fn peek(self: *Self) u8 {
            if (self.isAtEnd()) return 0;
            return self.source[self.current];
        }

        fn peekNext(self: *Self) u8 {
            if (self.current + 1 >= self.source.len) return 0;
            return self.source[self.current + 1];
        }

        fn isAtEnd(self: *Self) bool {
            return self.current >= self.source.len;
        }

        fn addToken(self: *Self, tag: Token.Tag) void {
            if (self.token_count >= config.max_tokens)
                @panic("scanner token buffer exhausted");
            self.tokens[self.token_count] = .{
                .tag = tag,
                .start = self.start,
                .len = self.current - self.start,
                .line = self.line,
            };
            self.token_count += 1;
        }
    };
}

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
        .identifier, .arrow, .string,     .eof,
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
    try std.testing.expectEqualStrings("S", tokens[0].lexeme(source));
    try std.testing.expectEqualStrings("->", tokens[1].lexeme(source));
    try std.testing.expectEqualStrings("\"hello\"", tokens[2].lexeme(source));
}
