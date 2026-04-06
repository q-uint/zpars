/// PEG scanner -- tokenizes Parsing Expression Grammars.
///
/// Based on Bryan Ford's "Parsing Expression Grammars: A Recognition-Based
/// Syntactic Foundation" (2004). Handles identifiers, the `<-` operator,
/// single/double quoted literals with escapes, character classes `[...]`,
/// `#` line comments, and all PEG operators.
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

            t['/'] = .{ .single = .slash };
            t['&'] = .{ .single = .@"and" };
            t['!'] = .{ .single = .not };
            t['?'] = .{ .single = .question };
            t['*'] = .{ .single = .star };
            t['+'] = .{ .single = .plus };
            t['('] = .{ .single = .left_paren };
            t[')'] = .{ .single = .right_paren };
            t['.'] = .{ .single = .dot };

            t[' '] = .skip;
            t['\t'] = .skip;

            t['<'] = .{ .handler = scanLeftArrow };
            t['\''] = .{ .handler = scanSingleQuote };
            t['"'] = .{ .handler = scanDoubleQuote };
            t['['] = .{ .handler = scanCharClass };
            t['#'] = .{ .handler = scanComment };
            t['\r'] = .{ .handler = scanNewlineCR };
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

        fn scanLeftArrow(self: *Self) void {
            if (!self.isAtEnd() and self.peek() == '-') {
                _ = self.advance();
                self.addToken(.left_arrow);
            } else {
                self.addToken(.invalid);
            }
        }

        fn scanSingleQuote(self: *Self) void {
            self.scanLiteral('\'');
        }

        fn scanDoubleQuote(self: *Self) void {
            self.scanLiteral('"');
        }

        fn scanLiteral(self: *Self, quote: u8) void {
            if (!self.isAtEnd() and self.peek() == quote) {
                _ = self.advance();
                self.addToken(.invalid);
                return;
            }
            while (!self.isAtEnd() and self.peek() != quote) {
                if (self.peek() == '\n' or self.peek() == '\r') {
                    self.addToken(.invalid);
                    return;
                }
                if (self.peek() == '\\') {
                    _ = self.advance();
                    if (!self.isAtEnd()) _ = self.advance();
                } else {
                    _ = self.advance();
                }
            }
            if (self.isAtEnd()) {
                self.addToken(.invalid);
            } else {
                _ = self.advance();
                self.addToken(.literal);
            }
        }

        fn scanCharClass(self: *Self) void {
            while (!self.isAtEnd() and self.peek() != ']') {
                if (self.peek() == '\n' or self.peek() == '\r') {
                    self.addToken(.invalid);
                    return;
                }
                if (self.peek() == '\\') {
                    _ = self.advance();
                    if (!self.isAtEnd()) _ = self.advance();
                } else {
                    _ = self.advance();
                }
            }
            if (self.isAtEnd()) {
                self.addToken(.invalid);
            } else {
                _ = self.advance();
                self.addToken(.char_class);
            }
        }

        fn scanComment(self: *Self) void {
            while (!self.isAtEnd() and self.peek() != '\n' and self.peek() != '\r') {
                _ = self.advance();
            }
            self.addToken(.comment);
        }

        fn scanIdentifier(self: *Self) void {
            while (!self.isAtEnd() and char_flags.isIdentCont(self.peek())) {
                _ = self.advance();
            }
            self.addToken(.identifier);
        }

        fn scanNewlineCR(self: *Self) void {
            _ = self.match('\n');
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

        fn match(self: *Self, expected: u8) bool {
            if (self.isAtEnd()) return false;
            if (self.source[self.current] != expected) return false;
            self.current += 1;
            return true;
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
