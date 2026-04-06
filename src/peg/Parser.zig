/// PEG parser — produces an AST from a PEG token stream.
///
/// Grammar (Bryan Ford, 2004):
///   Grammar    <- Spacing Definition+ EndOfFile
///   Definition <- Identifier LEFTARROW Expression
///   Expression <- Sequence (SLASH Sequence)*
///   Sequence   <- Prefix*
///   Prefix     <- (AND / NOT)? Suffix
///   Suffix     <- Primary (QUESTION / STAR / PLUS)?
///   Primary    <- Identifier !LEFTARROW
///              /  OPEN Expression CLOSE
///              /  Literal / Class / DOT
const std = @import("std");
const Token = @import("Token.zig").Token;
const Ast = @import("../Ast.zig");
const Diagnostic = @import("Diagnostic.zig").Diagnostic;
const parser_base = @import("../parser.zig");
const Pool = @import("../pool.zig").Pool;

const Parser = @This();

const primitives = parser_base.ParserBase(Parser, Token, Diagnostic, &.{ .comment, .newline }, .{
    .name_tag = .identifier,
    .def_tags = &.{.left_arrow},
});
pub const peek = primitives.peek;
const advance = primitives.advance;
pub const skipTrivia = primitives.skipTrivia;
pub const peekNextMeaningful = primitives.peekNextMeaningful;
pub const synchronize = primitives.synchronize;
const fail = primitives.fail;

pub const ParseError = error{ SyntaxError, Overflow };

pub const max_rules = 256;
pub const max_nodes = 4096;
pub const max_ranges = 1024;
pub const max_bytes = 1024;
pub const max_diagnostics = 64;

tokens: []const Token,
source: []const u8,
pos: usize = 0,

/// Pool for AST nodes.
nodes: Pool(Ast.Node, max_nodes) = .{},

/// Pool for ClassRange entries.
ranges: Pool(Ast.ClassRange, max_ranges) = .{},

/// Pool for decoded literal bytes.
bytes: Pool(u8, max_bytes) = .{},

/// Parsed rules.
rules: Pool(Ast.Rule, max_rules) = .{},

/// Accumulated parse diagnostics.
diagnostics: Pool(Diagnostic, max_diagnostics) = .{},

pub fn init(tokens: []const Token, source: []const u8) Parser {
    return .{
        .tokens = tokens,
        .source = source,
    };
}

/// Parse all definitions from the token stream.
pub fn parse(self: *Parser) ParseError![]const Ast.Rule {
    self.skipTrivia();
    while (self.peek().tag != .eof) {
        const rule = self.parseDefinition() catch |err| switch (err) {
            error.SyntaxError => {
                self.synchronize();
                self.skipTrivia();
                continue;
            },
            else => |e| return e,
        };

        _ = self.rules.addOne(rule);
        self.skipTrivia();
    }

    return self.rules.slice();
}

pub fn getDiagnostics(self: *const Parser) []const Diagnostic {
    return self.diagnostics.slice();
}

/// Definition <- Identifier LEFTARROW Expression
fn parseDefinition(self: *Parser) ParseError!Ast.Rule {
    if (self.peek().tag != .identifier) {
        self.fail(.identifier, self.peek());
        return error.SyntaxError;
    }
    const name = self.advance().lexeme(self.source);
    self.skipTrivia();

    if (self.peek().tag != .left_arrow) {
        self.fail(.left_arrow, self.peek());
        return error.SyntaxError;
    }
    _ = self.advance(); // consume <-
    self.skipTrivia();

    const node = try self.parseExpression();
    // After the expression we must be at eof or the start of the next
    // definition (`identifier <-`). Anything else (stray `)`, `/`
    // without a follow-up sequence, etc.) is a syntax error that
    // should abort this definition so the error-recovery path in
    // `parse()` can sync to the next rule.
    self.skipTrivia();
    if (!self.isDefinitionBoundary()) {
        self.fail(.expression, self.peek());
        return error.SyntaxError;
    }
    return .{ .name = name, .node = node, .incremental = false };
}

/// True when the current position marks the end of a definition body:
/// either eof, or an `identifier` immediately followed by `<-`.
fn isDefinitionBoundary(self: *Parser) bool {
    const tag = self.peek().tag;
    if (tag == .eof) return true;
    if (tag != .identifier) return false;
    return self.peekNextMeaningful() == .left_arrow;
}

/// Expression <- Sequence (SLASH Sequence)*
fn parseExpression(self: *Parser) ParseError!Ast.Node {
    var buf: [256]Ast.Node = undefined;
    var count: usize = 0;

    buf[0] = try self.parseSequence();
    count = 1;

    while (true) {
        self.skipTrivia();
        if (self.peek().tag != .slash) break;
        _ = self.advance();
        self.skipTrivia();
        buf[count] = try self.parseSequence();
        count += 1;
    }

    if (count == 1) return buf[0];
    return .{ .alternation = self.nodes.addSlice(buf[0..count]) };
}

/// Sequence <- Prefix*
fn parseSequence(self: *Parser) ParseError!Ast.Node {
    var buf: [256]Ast.Node = undefined;
    var count: usize = 0;

    while (self.isAtPrefix()) {
        buf[count] = try self.parsePrefix();
        count += 1;
        self.skipTrivia();
    }

    if (count == 0) {
        // Empty sequence — matches empty string. Represent as empty concat.
        return .{ .concatenation = self.nodes.addSlice(buf[0..0]) };
    }
    if (count == 1) return buf[0];
    return .{ .concatenation = self.nodes.addSlice(buf[0..count]) };
}

/// Prefix <- (AND / NOT)? Suffix
fn parsePrefix(self: *Parser) ParseError!Ast.Node {
    const tag = self.peek().tag;
    if (tag == .@"and") {
        _ = self.advance();
        self.skipTrivia();
        const inner = try self.parseSuffix();
        return .{ .and_predicate = self.nodes.addOne(inner) };
    }
    if (tag == .not) {
        _ = self.advance();
        self.skipTrivia();
        const inner = try self.parseSuffix();
        return .{ .not_predicate = self.nodes.addOne(inner) };
    }
    return self.parseSuffix();
}

/// Suffix <- Primary (QUESTION / STAR / PLUS)?
fn parseSuffix(self: *Parser) ParseError!Ast.Node {
    const primary = try self.parsePrimary();

    return switch (self.peek().tag) {
        .question => {
            _ = self.advance();
            return .{ .repetition = .{ .min = 0, .max = 1, .element = self.nodes.addOne(primary) } };
        },
        .star => {
            _ = self.advance();
            return .{ .repetition = .{ .min = 0, .max = null, .element = self.nodes.addOne(primary) } };
        },
        .plus => {
            _ = self.advance();
            return .{ .repetition = .{ .min = 1, .max = null, .element = self.nodes.addOne(primary) } };
        },
        else => primary,
    };
}

/// Primary <- Identifier !LEFTARROW
///          / OPEN Expression CLOSE
///          / Literal / Class / DOT
fn parsePrimary(self: *Parser) ParseError!Ast.Node {
    return switch (self.peek().tag) {
        .identifier => {
            // Only treat as reference if not followed by <-
            const next = self.peekNextMeaningful();
            if (next == .left_arrow) {
                self.fail(.expression, self.peek());
                return error.SyntaxError;
            }
            return .{ .rulename = self.advance().lexeme(self.source) };
        },
        .left_paren => {
            _ = self.advance(); // consume (
            self.skipTrivia();
            const expr = try self.parseExpression();
            self.skipTrivia();
            if (self.peek().tag != .right_paren) {
                self.fail(.right_paren, self.peek());
                return error.SyntaxError;
            }
            _ = self.advance(); // consume )
            return expr;
        },
        .literal => self.parseLiteral(),
        .char_class => self.parseCharClass(),
        .dot => {
            _ = self.advance();
            return .any;
        },
        else => {
            self.fail(.expression, self.peek());
            return error.SyntaxError;
        },
    };
}

fn parseLiteral(self: *Parser) Ast.Node {
    const lex = self.advance().lexeme(self.source);
    // Strip surrounding quotes, decode escapes.
    const inner = lex[1 .. lex.len - 1];
    const decoded = self.decodeEscapes(inner);
    return .{ .char_val = .{ .value = decoded, .case_sensitive = true } };
}

fn decodeEscapes(self: *Parser, raw: []const u8) []const u8 {
    // Fast path: no backslashes.
    if (std.mem.indexOfScalar(u8, raw, '\\') == null) return raw;

    const start = self.bytes.count;
    var i: usize = 0;
    while (i < raw.len) {
        if (raw[i] == '\\' and i + 1 < raw.len) {
            i += 1;
            const c = raw[i];
            const decoded: u8 = switch (c) {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\'', '"', '[', ']', '\\' => c,
                '0'...'2' => blk: {
                    // Possible octal: up to 3 digits.
                    const result = self.decodeOctal(raw, &i);
                    if (result) |val| break :blk val;
                    // Not a valid octal sequence, treat as literal.
                    _ = self.bytes.addOne('\\');
                    break :blk c;
                },
                else => blk: {
                    // Unknown escape — keep backslash.
                    _ = self.bytes.addOne('\\');
                    break :blk c;
                },
            };
            _ = self.bytes.addOne(decoded);
            i += 1;
        } else {
            _ = self.bytes.addOne(raw[i]);
            i += 1;
        }
    }
    return self.bytes.items[start..self.bytes.count];
}

/// Try to decode an octal escape starting at raw[*i].
/// *i points to the first octal digit.
/// Returns the decoded byte if valid, null otherwise.
/// On success, *i is advanced to the last digit (caller will do +1).
fn decodeOctal(self: *Parser, raw: []const u8, i: *usize) ?u8 {
    _ = self;
    const start = i.*;
    // Need at least 2 digits for a valid octal escape.
    if (start + 1 >= raw.len) return null;
    if (!isOctalDigit(raw[start + 1])) return null;

    // 3-digit octal (0-2, 0-7, 0-7)
    if (start + 2 < raw.len and isOctalDigit(raw[start + 2])) {
        const val = (@as(u16, raw[start] - '0') * 64) +
            (@as(u16, raw[start + 1] - '0') * 8) +
            @as(u16, raw[start + 2] - '0');
        if (val <= 255) {
            i.* = start + 2; // advance past all 3 digits (caller does +1)
            return @intCast(val);
        }
        return null;
    }

    // 2-digit octal
    const val = (raw[start] - '0') * 8 + (raw[start + 1] - '0');
    i.* = start + 1;
    return val;
}

fn isOctalDigit(c: u8) bool {
    return c >= '0' and c <= '7';
}

fn parseCharClass(self: *Parser) Ast.Node {
    const lex = self.advance().lexeme(self.source);
    // Strip surrounding brackets: [content]
    const inner = lex[1 .. lex.len - 1];

    const start = self.ranges.count;
    var i: usize = 0;
    while (i < inner.len) {
        const lo = self.decodeClassChar(inner, &i) orelse break;
        // Check for range: char '-' char
        if (i + 1 < inner.len and inner[i] == '-') {
            i += 1; // skip '-'
            const hi = self.decodeClassChar(inner, &i) orelse {
                // Malformed range — treat '-' as literal.
                _ = self.ranges.addOne(.{ .lo = lo, .hi = lo });
                _ = self.ranges.addOne(.{ .lo = '-', .hi = '-' });
                break;
            };
            _ = self.ranges.addOne(.{ .lo = lo, .hi = hi });
        } else {
            _ = self.ranges.addOne(.{ .lo = lo, .hi = lo });
        }
    }

    return .{ .char_class = self.ranges.items[start..self.ranges.count] };
}

/// Decode one character from a character class body, advancing *i past it.
fn decodeClassChar(self: *Parser, raw: []const u8, i: *usize) ?u8 {
    if (i.* >= raw.len) return null;
    if (raw[i.*] == '\\' and i.* + 1 < raw.len) {
        i.* += 1;
        const c = raw[i.*];
        i.* += 1;
        return switch (c) {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\'', '"', '[', ']', '\\' => c,
            '0'...'2' => {
                // Try octal.
                var oi = i.* - 1;
                if (self.decodeOctal(raw, &oi)) |val| {
                    i.* = oi + 1;
                    return val;
                }
                return c;
            },
            else => c,
        };
    }
    const c = raw[i.*];
    i.* += 1;
    return c;
}

/// Can the current position start a Prefix?
fn isAtPrefix(self: *Parser) bool {
    return switch (self.peek().tag) {
        .@"and", .not => true,
        else => self.isAtPrimary(),
    };
}

/// Can the current position start a Primary?
fn isAtPrimary(self: *Parser) bool {
    return switch (self.peek().tag) {
        .left_paren, .literal, .char_class, .dot => true,
        .identifier => {
            // An identifier followed by <- starts a new definition, not a reference.
            const next = self.peekNextMeaningful();
            return next != .left_arrow;
        },
        else => false,
    };
}

const Scanner = @import("Scanner.zig").Scanner;

fn parseSource(source: []const u8) ParseError!struct { parser: Parser, rules: []const Ast.Rule } {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    if (parser.diagnostics.count != 0) return error.SyntaxError;
    return .{ .parser = parser, .rules = rules };
}

test "simple definition" {
    const result = try parseSource("A <- B");
    try std.testing.expectEqual(1, result.rules.len);
    try std.testing.expectEqualStrings("A", result.rules[0].name);
    try std.testing.expectEqualStrings("B", result.rules[0].node.rulename);
}

test "ordered choice" {
    const result = try parseSource("A <- B / C / D");
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(3, alts.len);
    try std.testing.expectEqualStrings("B", alts[0].rulename);
    try std.testing.expectEqualStrings("C", alts[1].rulename);
    try std.testing.expectEqualStrings("D", alts[2].rulename);
}

test "sequence" {
    const result = try parseSource("A <- B C D");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(3, cat.len);
    try std.testing.expectEqualStrings("B", cat[0].rulename);
    try std.testing.expectEqualStrings("C", cat[1].rulename);
    try std.testing.expectEqualStrings("D", cat[2].rulename);
}

test "star suffix" {
    const result = try parseSource("A <- B*");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(null, rep.max);
    try std.testing.expectEqualStrings("B", rep.element.rulename);
}

test "plus suffix" {
    const result = try parseSource("A <- B+");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(1, rep.min);
    try std.testing.expectEqual(null, rep.max);
    try std.testing.expectEqualStrings("B", rep.element.rulename);
}

test "question suffix" {
    const result = try parseSource("A <- B?");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(1, rep.max.?);
    try std.testing.expectEqualStrings("B", rep.element.rulename);
}

test "and predicate" {
    const result = try parseSource("A <- &B");
    try std.testing.expectEqualStrings("B", result.rules[0].node.and_predicate.rulename);
}

test "not predicate" {
    const result = try parseSource("A <- !B");
    try std.testing.expectEqualStrings("B", result.rules[0].node.not_predicate.rulename);
}

test "dot wildcard" {
    const result = try parseSource("A <- .");
    try std.testing.expectEqual(.any, result.rules[0].node);
}

test "literal (single quotes)" {
    const result = try parseSource("A <- 'hello'");
    try std.testing.expectEqualStrings("hello", result.rules[0].node.char_val.value);
    try std.testing.expectEqual(true, result.rules[0].node.char_val.case_sensitive);
}

test "literal (double quotes)" {
    const result = try parseSource("A <- \"hello\"");
    try std.testing.expectEqualStrings("hello", result.rules[0].node.char_val.value);
}

test "character class simple" {
    const result = try parseSource("A <- [abc]");
    const ranges = result.rules[0].node.char_class;
    try std.testing.expectEqual(3, ranges.len);
    try std.testing.expectEqual('a', ranges[0].lo);
    try std.testing.expectEqual('a', ranges[0].hi);
    try std.testing.expectEqual('b', ranges[1].lo);
    try std.testing.expectEqual('c', ranges[2].lo);
}

test "character class range" {
    const result = try parseSource("A <- [a-z]");
    const ranges = result.rules[0].node.char_class;
    try std.testing.expectEqual(1, ranges.len);
    try std.testing.expectEqual('a', ranges[0].lo);
    try std.testing.expectEqual('z', ranges[0].hi);
}

test "character class mixed" {
    const result = try parseSource("A <- [a-zA-Z_]");
    const ranges = result.rules[0].node.char_class;
    try std.testing.expectEqual(3, ranges.len);
    try std.testing.expectEqual('a', ranges[0].lo);
    try std.testing.expectEqual('z', ranges[0].hi);
    try std.testing.expectEqual('A', ranges[1].lo);
    try std.testing.expectEqual('Z', ranges[1].hi);
    try std.testing.expectEqual('_', ranges[2].lo);
    try std.testing.expectEqual('_', ranges[2].hi);
}

test "grouped expression" {
    const result = try parseSource("A <- (B / C) D");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(2, cat.len);
    try std.testing.expectEqual(2, cat[0].alternation.len);
    try std.testing.expectEqualStrings("D", cat[1].rulename);
}

test "multiple definitions" {
    const result = try parseSource("A <- B\nC <- D");
    try std.testing.expectEqual(2, result.rules.len);
    try std.testing.expectEqualStrings("A", result.rules[0].name);
    try std.testing.expectEqualStrings("C", result.rules[1].name);
}

test "not predicate with dot (end of file)" {
    const result = try parseSource("EOF <- !.");
    const inner = result.rules[0].node.not_predicate.*;
    try std.testing.expectEqual(.any, inner);
}

test "complex: identifier with lookahead" {
    // Primary <- Identifier !LEFTARROW
    const result = try parseSource("P <- A !B C");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(3, cat.len);
    try std.testing.expectEqualStrings("A", cat[0].rulename);
    try std.testing.expectEqualStrings("B", cat[1].not_predicate.rulename);
    try std.testing.expectEqualStrings("C", cat[2].rulename);
}

test "incremental is always false" {
    const result = try parseSource("A <- B");
    try std.testing.expectEqual(false, result.rules[0].incremental);
}

test "recovery: error in first definition, second parsed" {
    var scanner = Scanner.init("A <- )\nB <- C");
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, "A <- )\nB <- C");
    const rules = try parser.parse();
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("B", rules[0].name);
    try std.testing.expect(parser.getDiagnostics().len > 0);
}

test "recovery: missing left arrow" {
    var scanner = Scanner.init("A B\nC <- D");
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, "A B\nC <- D");
    const rules = try parser.parse();
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("C", rules[0].name);
}

test "multi-line alternation continuation" {
    const result = try parseSource(
        \\Primary <- Identifier
        \\         / Literal
        \\         / DOT
    );
    try std.testing.expectEqual(1, result.rules.len);
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(3, alts.len);
}

test "Class rule from PEG grammar" {
    const result = try parseSource("Class <- '[' (!']' Range)* ']' Spacing");
    try std.testing.expectEqual(1, result.rules.len);
    try std.testing.expectEqualStrings("Class", result.rules[0].name);
}

test "multi-rule PEG grammar subset" {
    const result = try parseSource(
        \\Identifier <- IdentStart IdentCont* Spacing
        \\IdentStart <- [a-zA-Z_]
        \\IdentCont  <- IdentStart / [0-9]
    );
    try std.testing.expectEqual(3, result.rules.len);
}

test "Literal rule with quotes and escapes" {
    const result = try parseSource(
        \\Literal <- ['] (!['] Char)* ['] Spacing
    );
    try std.testing.expectEqual(1, result.rules.len);
}

test "Char rule multi-line" {
    const result = try parseSource(
        \\Char <- '\\' [nrt'"\[\]\\]
        \\     / '\\' [0-2] [0-7] [0-7]
        \\     / '\\' [0-7] [0-7]?
        \\     / !'\\' .
    );
    try std.testing.expectEqual(1, result.rules.len);
}
