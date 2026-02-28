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
    skip,
    handler: *const fn (*Scanner) void,
};

const dispatch_table: [256]Action = blk: {
    // BNF: everything not recognized is a terminal character.
    var t: [256]Action = @splat(.{ .handler = scanTerminal });

    t['<'] = .{ .handler = scanAngleBracket };
    t['|'] = .{ .single = .pipe };
    t[':'] = .{ .handler = scanColon };

    t[' '] = .skip;
    t['\t'] = .skip;

    t['\r'] = .{ .handler = scanNewlineCR };
    t['\n'] = .{ .handler = scanNewlineLF };

    break :blk t;
};

fn scanToken(self: *Scanner) void {
    const c = self.advance();
    switch (dispatch_table[c]) {
        .single => |tag| self.addToken(tag),
        .skip => {},
        .handler => |func| func(self),
    }
}

fn scanAngleBracket(self: *Scanner) void {
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
        _ = self.advance();
        self.addToken(.rulename);
    }
}

fn scanColon(self: *Scanner) void {
    if (self.current + 1 < self.source.len and
        self.source[self.current] == ':' and
        self.source[self.current + 1] == '=')
    {
        self.current += 2;
        self.addToken(.definition);
    } else {
        self.scanTerminal();
    }
}

fn scanTerminal(self: *Scanner) void {
    while (!self.isAtEnd()) {
        const c = self.peek();
        if (c == '<' or c == '|' or c == ' ' or c == '\t' or c == '\n' or c == '\r') break;
        if (c == ':' and self.current + 2 <= self.source.len and
            self.source[self.current + 1] == ':' and
            self.source[self.current + 2] == '=') break;
        _ = self.advance();
    }
    self.addToken(.terminal);
}

fn scanNewlineCR(self: *Scanner) void {
    _ = self.match('\n');
    self.line += 1;
    self.addToken(.newline);
}

fn scanNewlineLF(self: *Scanner) void {
    self.line += 1;
    self.addToken(.newline);
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
        .rulename, .definition,
        .terminal, .pipe,
        .terminal, .pipe,
        .rulename, .terminal,
        .pipe,     .rulename,
        .rulename, .eof,
    });
}

test "bare colon as terminal" {
    try expectTags("<x> ::= a:b", &.{ .rulename, .definition, .terminal, .eof });
}

test "unterminated rulename" {
    try expectTags("<oops", &.{ .invalid, .eof });
}
