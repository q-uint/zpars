/// CFG parser — produces a `Cfg` from a CFG token stream.
///
/// Grammar:
///   rule          = identifier "->" alternation
///   alternation   = sequence *("|" sequence)
///   sequence      = *symbol
///   symbol        = identifier / string / hex_byte / hex_range
///                 / string_cs / string_ci
const std = @import("std");
const Token = @import("Token.zig").Token;
const Cfg = @import("../Cfg.zig");
const Diagnostic = @import("Diagnostic.zig").Diagnostic;

const Parser = @This();

pub const ParseError = error{ SyntaxError, Overflow };

pub const max_rules = 256;
pub const max_prods = 1024;
pub const max_symbols = 4096;
pub const max_diagnostics = 64;

tokens: []const Token,
source: []const u8,
pos: usize = 0,

/// Nonterminal name table.
nts: [max_rules][]const u8 = undefined,
nt_count: usize = 0,

/// Symbol pool shared across all productions.
sym_pool: [max_symbols]Cfg.Symbol = undefined,
sym_total: usize = 0,

/// Parsed productions.
prods: [max_prods]Cfg.Production = undefined,
prod_count: usize = 0,

/// Accumulated parse diagnostics.
diagnostics_buf: [max_diagnostics]Diagnostic = undefined,
diagnostics_count: usize = 0,

pub fn init(tokens: []const Token, source: []const u8) Parser {
    return .{
        .tokens = tokens,
        .source = source,
    };
}

/// Parse all rules and return a `Cfg`.
///
/// Productions are grouped by LHS so that `Cfg.productionsFor` works.
pub fn parse(self: *Parser) ParseError!Cfg {
    self.skipNewlines();
    while (self.peek().tag != .eof) {
        self.parseRule() catch |err| switch (err) {
            error.SyntaxError => {
                self.synchronize();
                self.skipNewlines();
                continue;
            },
            else => |e| return e,
        };
        self.skipNewlines();
    }

    if (self.prod_count == 0) return error.SyntaxError;

    // Group productions by LHS.
    var sorted: [max_prods]Cfg.Production = undefined;
    var sorted_count: usize = 0;
    for (0..self.nt_count) |nt_id| {
        for (self.prods[0..self.prod_count]) |prod| {
            if (prod.lhs == @as(u32, @intCast(nt_id))) {
                sorted[sorted_count] = prod;
                sorted_count += 1;
            }
        }
    }

    return .{
        .nonterminals = self.nts[0..self.nt_count],
        .productions = sorted[0..sorted_count],
        .start = 0,
    };
}

pub fn getDiagnostics(self: *const Parser) []const Diagnostic {
    return self.diagnostics_buf[0..self.diagnostics_count];
}

// --- Grammar rules -----------------------------------------------------------

/// rule = identifier "->" alternation
fn parseRule(self: *Parser) ParseError!void {
    if (self.peek().tag != .identifier) {
        self.fail(.identifier, self.peek());
        return error.SyntaxError;
    }
    const name_tok = self.advance();
    const lhs_name = name_tok.lexeme(self.source);

    self.skipNewlines();
    if (self.peek().tag != .arrow) {
        self.fail(.arrow, self.peek());
        return error.SyntaxError;
    }
    _ = self.advance(); // consume ->

    const lhs_id = self.findOrAddNt(lhs_name);

    // Parse alternatives: sequence *("|" sequence)
    self.parseSequence(lhs_id);

    while (self.peek().tag == .pipe) {
        _ = self.advance(); // consume |
        self.parseSequence(lhs_id);
    }
}

/// Parse a sequence of symbols and append as a production.
fn parseSequence(self: *Parser, lhs_id: u32) void {
    const sym_start = self.sym_total;

    while (isSymbolTag(self.peek().tag)) {
        self.sym_pool[self.sym_total] = self.parseSymbol();
        self.sym_total += 1;
    }

    self.prods[self.prod_count] = .{
        .lhs = lhs_id,
        .rhs = self.sym_pool[sym_start..self.sym_total],
    };
    self.prod_count += 1;
}

fn parseSymbol(self: *Parser) Cfg.Symbol {
    const tok = self.advance();
    const lex = tok.lexeme(self.source);
    return switch (tok.tag) {
        .identifier => .{ .nonterminal = self.findOrAddNt(lex) },
        .string => .{ .terminal = .{ .string = stripQuotes(lex) } },
        .string_cs => .{ .terminal = .{ .string = stripPrefixedQuotes(lex) } },
        .string_ci => .{ .terminal = .{ .string_ci = stripPrefixedQuotes(lex) } },
        .hex_byte => .{ .terminal = .{ .byte = parseHexByte(lex) } },
        .hex_range => .{ .terminal = parseHexRange(lex) },
        else => unreachable, // guarded by isSymbolTag
    };
}

fn isSymbolTag(tag: Token.Tag) bool {
    return switch (tag) {
        .identifier, .string, .string_cs, .string_ci, .hex_byte, .hex_range => true,
        else => false,
    };
}

// --- Nonterminal table -------------------------------------------------------

fn findOrAddNt(self: *Parser, name: []const u8) u32 {
    for (self.nts[0..self.nt_count], 0..) |nt, i| {
        if (std.mem.eql(u8, nt, name)) return @intCast(i);
    }
    self.nts[self.nt_count] = name;
    self.nt_count += 1;
    return @intCast(self.nt_count - 1);
}

// --- Lexeme helpers ----------------------------------------------------------

/// `"text"` → `text`
fn stripQuotes(lex: []const u8) []const u8 {
    return lex[1 .. lex.len - 1];
}

/// `%s"text"` or `%i"text"` → `text`
fn stripPrefixedQuotes(lex: []const u8) []const u8 {
    return lex[3 .. lex.len - 1];
}

/// `%x41` → 0x41
fn parseHexByte(lex: []const u8) u8 {
    return parseHex(lex[2..]);
}

/// `%x41-5A` → Terminal.range
fn parseHexRange(lex: []const u8) Cfg.Terminal {
    // Skip "%x", then split on '-'
    const hex_part = lex[2..];
    const dash = std.mem.indexOf(u8, hex_part, "-").?;
    return .{ .range = .{
        .lo = parseHex(hex_part[0..dash]),
        .hi = parseHex(hex_part[dash + 1 ..]),
    } };
}

fn parseHex(s: []const u8) u8 {
    var result: u16 = 0;
    for (s) |c| {
        const digit: u16 = if (c >= '0' and c <= '9')
            c - '0'
        else if (c >= 'a' and c <= 'f')
            c - 'a' + 10
        else
            c - 'A' + 10;
        result = result * 16 + digit;
    }
    return @intCast(result & 0xFF);
}

// --- Diagnostics -------------------------------------------------------------

fn fail(self: *Parser, expected: Diagnostic.Expected, tok: Token) void {
    self.diagnostics_buf[self.diagnostics_count] = .{
        .expected = expected,
        .found_tag = tok.tag,
        .found_start = tok.start,
        .found_len = tok.len,
        .line = tok.line,
    };
    self.diagnostics_count += 1;
}

// --- Helpers -----------------------------------------------------------------

fn peek(self: *Parser) Token {
    return self.tokens[self.pos];
}

fn advance(self: *Parser) Token {
    const tok = self.tokens[self.pos];
    self.pos += 1;
    return tok;
}

fn skipNewlines(self: *Parser) void {
    while (self.peek().tag == .newline) self.pos += 1;
}

fn synchronize(self: *Parser) void {
    while (self.peek().tag != .eof) {
        if (self.peek().tag == .identifier) {
            const next = self.peekNextMeaningful();
            if (next == .arrow) return;
        }
        self.pos += 1;
    }
}

fn peekNextMeaningful(self: *Parser) Token.Tag {
    var i = self.pos + 1;
    while (i < self.tokens.len) : (i += 1) {
        const tag = self.tokens[i].tag;
        if (tag != .newline) return tag;
    }
    return .eof;
}

// --- Tests -------------------------------------------------------------------

const Scanner = @import("Scanner.zig");

fn parseSource(source: []const u8) ParseError!struct { parser: Parser, cfg: Cfg } {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const cfg = try parser.parse();
    if (parser.diagnostics_count != 0) return error.SyntaxError;
    return .{ .parser = parser, .cfg = cfg };
}

test "basic nonterminal and string terminal" {
    const result = try parseSource(
        \\S -> A "x"
        \\A -> "hello"
    );
    const cfg = result.cfg;

    try std.testing.expectEqual(2, cfg.nonterminals.len);
    try std.testing.expectEqualStrings("S", cfg.nonterminalName(0));
    try std.testing.expectEqualStrings("A", cfg.nonterminalName(1));
    try std.testing.expectEqual(@as(u32, 0), cfg.start);
    try std.testing.expectEqual(2, cfg.productions.len);

    const p0 = cfg.productions[0];
    try std.testing.expectEqual(@as(u32, 0), p0.lhs);
    try std.testing.expect(p0.rhs[0].eql(.{ .nonterminal = 1 }));
    try std.testing.expect(p0.rhs[1].eql(.{ .terminal = .{ .string = "x" } }));
}

test "alternation with pipe" {
    const result = try parseSource(
        \\S -> "a" | "b" | "c"
    );
    try std.testing.expectEqual(3, result.cfg.productions.len);
    try std.testing.expect(result.cfg.productions[0].rhs[0].eql(.{ .terminal = .{ .string = "a" } }));
    try std.testing.expect(result.cfg.productions[1].rhs[0].eql(.{ .terminal = .{ .string = "b" } }));
    try std.testing.expect(result.cfg.productions[2].rhs[0].eql(.{ .terminal = .{ .string = "c" } }));
}

test "epsilon production" {
    const result = try parseSource(
        \\S -> "x" |
    );
    try std.testing.expectEqual(2, result.cfg.productions.len);
    try std.testing.expectEqual(1, result.cfg.productions[0].rhs.len);
    try std.testing.expectEqual(0, result.cfg.productions[1].rhs.len);
}

test "hex byte and range" {
    const result = try parseSource(
        \\S -> %x41 %x61-7A
    );
    try std.testing.expect(result.cfg.productions[0].rhs[0].eql(.{ .terminal = .{ .byte = 0x41 } }));
    try std.testing.expect(result.cfg.productions[0].rhs[1].eql(.{ .terminal = .{ .range = .{ .lo = 0x61, .hi = 0x7A } } }));
}

test "case-sensitive and case-insensitive strings" {
    const result = try parseSource(
        \\S -> %s"GET" %i"hello"
    );
    try std.testing.expect(result.cfg.productions[0].rhs[0].eql(.{ .terminal = .{ .string = "GET" } }));
    try std.testing.expect(result.cfg.productions[0].rhs[1].eql(.{ .terminal = .{ .string_ci = "hello" } }));
}

test "productions grouped by LHS" {
    const result = try parseSource(
        \\S -> A
        \\A -> "a"
        \\S -> "b"
    );
    const s_prods = result.cfg.productionsFor(0);
    try std.testing.expectEqual(2, s_prods.len);
    try std.testing.expectEqual(@as(u32, 0), s_prods[0].lhs);
    try std.testing.expectEqual(@as(u32, 0), s_prods[1].lhs);
}

test "comments and blank lines" {
    const result = try parseSource(
        \\// This is a comment
        \\
        \\S -> "x"
        \\// Another comment
    );
    try std.testing.expectEqual(1, result.cfg.productions.len);
}

test "format round-trip" {
    const result = try parseSource(
        \\S -> A %x78
        \\A -> %s"hello"
        \\A ->
    );

    var buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try std.fmt.format(fbs.writer(), "{}", .{result.cfg});
    try std.testing.expectEqualStrings(
        \\S → A %x78
        \\A → %s"hello"
        \\A → ε
    , fbs.getWritten());
}

test "recovery: missing arrow" {
    var scanner = Scanner.init("S oops\nA -> \"x\"");
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, "S oops\nA -> \"x\"");
    const cfg = try parser.parse();
    try std.testing.expectEqual(1, cfg.productions.len);
    try std.testing.expectEqualStrings("A", cfg.nonterminalName(cfg.productions[0].lhs));
    try std.testing.expect(parser.getDiagnostics().len > 0);
}
