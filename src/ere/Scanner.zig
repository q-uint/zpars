/// ERE scanner — tokenizes POSIX Extended Regular Expressions.
///
/// Based on IEEE Std 1003.1, Section 9.4 (Extended Regular Expressions).
/// Every character is syntactically meaningful — there is no trivia.
const std = @import("std");
const Token = @import("Token.zig").Token;

const Scanner = @This();

pub const max_tokens = 4096;

source: []const u8,
tokens: [max_tokens]Token = undefined,
token_count: usize = 0,
start: usize = 0,
current: usize = 0,
line: usize = 1,

pub fn init(source: []const u8) Scanner {
    return .{ .source = source };
}

pub fn scanTokens(self: *Scanner) []const Token {
    while (!self.isAtEnd()) {
        self.start = self.current;
        self.scanToken();
    }

    self.addToken(.eof);
    return self.tokens[0..self.token_count];
}

const Action = union(enum) {
    single: Token.Tag,
    handler: *const fn (*Scanner) void,
};

const dispatch_table: [256]Action = blk: {
    var t: [256]Action = @splat(.{ .handler = scanOrdinaryChar });

    t['.'] = .{ .single = .dot };
    t['^'] = .{ .single = .caret };
    t['$'] = .{ .single = .dollar };
    t['*'] = .{ .single = .star };
    t['+'] = .{ .single = .plus };
    t['?'] = .{ .single = .question };
    t['('] = .{ .single = .left_paren };
    t[')'] = .{ .single = .right_paren };
    t['|'] = .{ .single = .pipe };

    t['\\'] = .{ .handler = scanBackslash };
    t['['] = .{ .handler = scanBracketExpr };
    t['{'] = .{ .handler = scanLbrace };

    break :blk t;
};

fn scanToken(self: *Scanner) void {
    const c = self.advance();
    switch (dispatch_table[c]) {
        .single => |tag| self.addToken(tag),
        .handler => |func| func(self),
    }
}

fn scanBackslash(self: *Scanner) void {
    if (self.isAtEnd()) {
        self.addToken(.invalid);
        return;
    }
    // Consume the escaped character.
    _ = self.advance();
    self.addToken(.char);
}

fn scanBracketExpr(self: *Scanner) void {
    // Handle [^ at start (negation) — still part of the bracket_expr.
    if (!self.isAtEnd() and self.peek() == '^') _ = self.advance();

    // Per POSIX: ] immediately after [ or [^ is a literal ].
    if (!self.isAtEnd() and self.peek() == ']') _ = self.advance();

    while (!self.isAtEnd() and self.peek() != ']') {
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
        _ = self.advance(); // consume ]
        self.addToken(.bracket_expr);
    }
}

/// Try to scan an interval expression `{m}`, `{m,}`, or `{m,n}`.
/// If the content doesn't match, treat the `{` as a literal char.
fn scanLbrace(self: *Scanner) void {
    const save = self.current;

    // Try to scan a valid interval expression.
    if (self.tryInterval()) return;

    // Not a valid interval — rewind and emit `{` as char.
    self.current = save;
    self.addToken(.char);
}

fn tryInterval(self: *Scanner) bool {
    // Expect at least one digit.
    if (self.isAtEnd() or !std.ascii.isDigit(self.peek())) return false;

    // Emit the opening brace.
    self.addToken(.lbrace);

    // Scan the first number.
    self.start = self.current;
    while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) _ = self.advance();
    self.addToken(.number);

    if (self.isAtEnd()) return false;

    if (self.peek() == '}') {
        // {m} — exact
        self.start = self.current;
        _ = self.advance();
        self.addToken(.rbrace);
        return true;
    }

    if (self.peek() == ',') {
        self.start = self.current;
        _ = self.advance();
        self.addToken(.comma);

        // {m,} or {m,n}
        if (self.isAtEnd()) return false;

        if (self.peek() == '}') {
            self.start = self.current;
            _ = self.advance();
            self.addToken(.rbrace);
            return true;
        }

        if (std.ascii.isDigit(self.peek())) {
            self.start = self.current;
            while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) _ = self.advance();
            self.addToken(.number);

            if (!self.isAtEnd() and self.peek() == '}') {
                self.start = self.current;
                _ = self.advance();
                self.addToken(.rbrace);
                return true;
            }
        }
    }

    return false;
}

fn scanOrdinaryChar(self: *Scanner) void {
    self.addToken(.char);
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

fn addToken(self: *Scanner, tag: Token.Tag) void {
    self.tokens[self.token_count] = .{
        .tag = tag,
        .start = self.start,
        .len = self.current - self.start,
        .line = self.line,
    };
    self.token_count += 1;
}

// ── Tests ───────────────────────────────────────────────────────────

fn expectTags(source: []const u8, expected: []const Token.Tag) !void {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    const allocator = std.testing.allocator;
    const actual = try allocator.alloc(Token.Tag, tokens.len);
    defer allocator.free(actual);
    for (tokens, 0..) |tok, i| actual[i] = tok.tag;
    try std.testing.expectEqualSlices(Token.Tag, expected, actual);
}

test "ordinary characters" {
    try expectTags("abc", &.{ .char, .char, .char, .eof });
}

test "metacharacters" {
    try expectTags(".*+?|", &.{ .dot, .star, .plus, .question, .pipe, .eof });
}

test "anchors" {
    try expectTags("^foo$", &.{ .caret, .char, .char, .char, .dollar, .eof });
}

test "escaped special" {
    try expectTags("\\.", &.{ .char, .eof });
}

test "escaped backslash" {
    try expectTags("\\\\", &.{ .char, .eof });
}

test "grouping" {
    try expectTags("(a|b)", &.{ .left_paren, .char, .pipe, .char, .right_paren, .eof });
}

test "bracket expression" {
    try expectTags("[abc]", &.{ .bracket_expr, .eof });
}

test "negated bracket expression" {
    try expectTags("[^abc]", &.{ .bracket_expr, .eof });
}

test "bracket with leading ]" {
    try expectTags("[]abc]", &.{ .bracket_expr, .eof });
}

test "bracket with escape" {
    try expectTags("[\\]]", &.{ .bracket_expr, .eof });
}

test "unterminated bracket" {
    try expectTags("[abc", &.{ .invalid, .eof });
}

test "interval {m,n}" {
    try expectTags("a{2,5}", &.{ .char, .lbrace, .number, .comma, .number, .rbrace, .eof });
}

test "interval {m}" {
    try expectTags("a{3}", &.{ .char, .lbrace, .number, .rbrace, .eof });
}

test "interval {m,}" {
    try expectTags("a{3,}", &.{ .char, .lbrace, .number, .comma, .rbrace, .eof });
}

test "invalid interval is literal" {
    try expectTags("a{b}", &.{ .char, .char, .char, .char, .eof });
}

test "POSIX class in bracket" {
    try expectTags("[[:alpha:]]", &.{ .bracket_expr, .eof });
}

test "complex pattern" {
    try expectTags("^[a-z]+$", &.{ .caret, .bracket_expr, .plus, .dollar, .eof });
}

test "trailing backslash" {
    try expectTags("a\\", &.{ .char, .invalid, .eof });
}
