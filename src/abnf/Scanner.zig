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

        pub const max_tokens = config.max_tokens;

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

            t['('] = .{ .single = .left_paren };
            t[')'] = .{ .single = .right_paren };
            t['['] = .{ .single = .left_bracket };
            t[']'] = .{ .single = .right_bracket };
            t['*'] = .{ .single = .star };
            t['/'] = .{ .single = .slash };

            t[' '] = .skip;
            t['\t'] = .skip;

            t['='] = .{ .handler = scanEquals };
            t['"'] = .{ .handler = scanString };
            t['<'] = .{ .handler = scanProse };
            t['%'] = .{ .handler = scanPercent };
            t[';'] = .{ .handler = scanLineComment };
            t['\r'] = .{ .handler = scanNewlineCR };
            t['\n'] = .{ .handler = scanNewlineLF };

            for ('A'..('Z' + 1)) |c| t[c] = .{ .handler = scanRulename };
            for ('a'..('z' + 1)) |c| t[c] = .{ .handler = scanRulename };
            for ('0'..('9' + 1)) |c| t[c] = .{ .handler = scanNumber };

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

        fn scanEquals(self: *Self) void {
            self.addToken(if (self.match('/')) .equals_slash else .equals);
        }

        fn scanString(self: *Self) void {
            if (self.peek() == '"') {
                _ = self.advance();
                self.addToken(.invalid);
            } else {
                while (self.peek() != '"' and !self.isAtEnd()) {
                    if (self.peek() == '\n') self.line += 1;
                    _ = self.advance();
                }
                if (self.isAtEnd()) {
                    self.addToken(.invalid);
                } else {
                    _ = self.advance();
                    self.addToken(.char_val);
                }
            }
        }

        fn scanProse(self: *Self) void {
            while (self.peek() != '>' and !self.isAtEnd()) {
                _ = self.advance();
            }
            if (self.isAtEnd()) {
                self.addToken(.invalid);
            } else {
                _ = self.advance();
                self.addToken(.prose_val);
            }
        }

        fn scanPercent(self: *Self) void {
            const base = self.peek();
            switch (base) {
                'b' => {
                    _ = self.advance();
                    self.consumeDigits(char_flags.isBit);
                    self.addToken(.bin_val);
                },
                'd' => {
                    _ = self.advance();
                    self.consumeDigits(char_flags.isDigit);
                    self.addToken(.dec_val);
                },
                'x' => {
                    _ = self.advance();
                    self.consumeDigits(char_flags.isHexDigit);
                    self.addToken(.hex_val);
                },
                's', 'i' => {
                    _ = self.advance();
                    if (self.peek() != '"') {
                        self.addToken(.invalid);
                    } else {
                        _ = self.advance();
                        if (self.peek() == '"') {
                            _ = self.advance();
                            self.addToken(.invalid);
                        } else {
                            while (self.peek() != '"' and !self.isAtEnd()) {
                                if (self.peek() == '\n') self.line += 1;
                                _ = self.advance();
                            }
                            if (self.isAtEnd()) {
                                self.addToken(.invalid);
                            } else {
                                _ = self.advance();
                                self.addToken(if (base == 's') .char_val_cs else .char_val_ci);
                            }
                        }
                    }
                },
                else => self.addToken(.invalid),
            }
        }

        fn scanLineComment(self: *Self) void {
            while (self.peek() != '\n' and self.peek() != '\r' and !self.isAtEnd()) {
                _ = self.advance();
            }
            self.addToken(.comment);
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

        fn scanRulename(self: *Self) void {
            while (char_flags.isAlpha(self.peek()) or char_flags.isDigit(self.peek()) or self.peek() == '-') {
                _ = self.advance();
            }
            self.addToken(.rulename);
        }

        fn scanNumber(self: *Self) void {
            while (char_flags.isDigit(self.peek())) _ = self.advance();
            self.addToken(.number);
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

        fn consumeDigits(self: *Self, isValidDigit: *const fn (u8) bool) void {
            while (isValidDigit(self.peek())) _ = self.advance();

            if (self.peek() == '.') {
                while (self.peek() == '.') {
                    _ = self.advance();
                    while (isValidDigit(self.peek())) _ = self.advance();
                }
            } else if (self.peek() == '-') {
                _ = self.advance();
                while (isValidDigit(self.peek())) _ = self.advance();
            }
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
        .rulename, .equals, .rulename, .eof,
    });
}
