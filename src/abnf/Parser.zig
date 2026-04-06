const std = @import("std");
const Token = @import("Token.zig").Token;
const Ast = @import("../Ast.zig");
const Diagnostic = @import("Diagnostic.zig").Diagnostic;
const parser_base = @import("../parser.zig");
const Pool = @import("../pool.zig").Pool;

pub const Config = struct {
    max_rules: usize = 256,
    max_nodes: usize = 4096,
    max_bytes: usize = 1024,
    max_diagnostics: usize = 64,
};

pub const Parser = ParserWith(.{});

pub fn ParserWith(comptime config: Config) type {
    return struct {
        const Self = @This();

        const primitives = parser_base.ParserBase(Self, Token, Diagnostic, &.{ .comment, .newline }, .{
            .name_tag = .rulename,
            .def_tags = &.{ .equals, .equals_slash },
        });
        pub const peek = primitives.peek;
        const peekAt = primitives.peekAt;
        const advance = primitives.advance;
        pub const skipTrivia = primitives.skipTrivia;
        pub const peekNextMeaningful = primitives.peekNextMeaningful;
        pub const synchronize = primitives.synchronize;
        const fail = primitives.fail;

        pub const ParseError = error{ SyntaxError, Overflow, InvalidCharacter };

        tokens: []const Token,
        source: []const u8,
        pos: usize = 0,

        nodes: Pool(Ast.Node, config.max_nodes) = .{},
        bytes: Pool(u8, config.max_bytes) = .{},
        rules: Pool(Ast.Rule, config.max_rules) = .{},
        diagnostics: Pool(Diagnostic, config.max_diagnostics) = .{},

        pub fn init(tokens: []const Token, source: []const u8) Self {
            return .{
                .tokens = tokens,
                .source = source,
            };
        }

        pub fn parse(self: *Self) ParseError![]const Ast.Rule {
            self.skipTrivia();
            while (self.peek().tag != .eof) {
                const rule = self.parseRule() catch |err| switch (err) {
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

        pub fn getDiagnostics(self: *const Self) []const Diagnostic {
            return self.diagnostics.slice();
        }

        fn parseRule(self: *Self) ParseError!Ast.Rule {
            if (self.peek().tag != .rulename) {
                self.fail(.rulename, self.peek());
                return error.SyntaxError;
            }
            const name = self.advance().lexeme(self.source);
            self.skipTrivia();
            var incremental = false;
            if (self.peek().tag == .equals or self.peek().tag == .equals_slash) {
                incremental = self.advance().tag == .equals_slash;
            }
            self.skipTrivia();
            return .{ .name = name, .node = try self.parseAlternation(), .incremental = incremental };
        }

        fn parseAlternation(self: *Self) ParseError!Ast.Node {
            var buf: [256]Ast.Node = undefined;
            var count: usize = 0;

            buf[0] = try self.parseConcatenation();
            count = 1;

            while (true) {
                self.skipTrivia();
                if (self.peek().tag != .slash) break;
                _ = self.advance();
                self.skipTrivia();
                buf[count] = try self.parseConcatenation();
                count += 1;
            }

            if (count == 1) return buf[0];
            return .{ .alternation = self.nodes.addSlice(buf[0..count]) };
        }

        fn parseConcatenation(self: *Self) ParseError!Ast.Node {
            var buf: [256]Ast.Node = undefined;
            var count: usize = 0;

            if (!self.isAtRepetition()) {
                self.fail(.element, self.peek());
                return error.SyntaxError;
            }

            while (self.isAtRepetition()) {
                buf[count] = try self.parseRepetition();
                count += 1;
                self.skipTrivia();
            }

            if (count == 1) return buf[0];
            return .{ .concatenation = self.nodes.addSlice(buf[0..count]) };
        }

        fn parseRepetition(self: *Self) ParseError!Ast.Node {
            var min: usize = 0;
            var max: ?usize = null;
            var has_repeat = false;

            switch (self.peek().tag) {
                .number => {
                    if (self.peekAt(1).tag == .star) {
                        min = try self.parseNumber();
                        _ = self.advance();
                        if (self.peek().tag == .number) max = try self.parseNumber();
                    } else {
                        const n = try self.parseNumber();
                        min = n;
                        max = n;
                    }
                    has_repeat = true;
                },
                .star => {
                    _ = self.advance();
                    if (self.peek().tag == .number) max = try self.parseNumber();
                    has_repeat = true;
                },
                else => {},
            }

            const element = try self.parseElement();
            if (!has_repeat) return element;

            const ptr = self.nodes.addOne(element);
            return .{ .repetition = .{ .min = min, .max = max, .element = ptr } };
        }

        fn parseElement(self: *Self) ParseError!Ast.Node {
            return switch (self.peek().tag) {
                .rulename => .{ .rulename = self.advance().lexeme(self.source) },
                .char_val => {
                    const lex = self.advance().lexeme(self.source);
                    return .{ .char_val = .{ .value = lex[1 .. lex.len - 1], .case_sensitive = false } };
                },
                .char_val_ci => {
                    const lex = self.advance().lexeme(self.source);
                    return .{ .char_val = .{ .value = lex[3 .. lex.len - 1], .case_sensitive = false } };
                },
                .char_val_cs => {
                    const lex = self.advance().lexeme(self.source);
                    return .{ .char_val = .{ .value = lex[3 .. lex.len - 1], .case_sensitive = true } };
                },
                .bin_val, .dec_val, .hex_val => .{ .num_val = try self.parseNumVal() },
                .prose_val => {
                    const lex = self.advance().lexeme(self.source);
                    return .{ .prose_val = lex[1 .. lex.len - 1] };
                },
                .left_paren => try self.parseGroup(),
                .left_bracket => try self.parseOption(),
                else => {
                    self.fail(.element, self.peek());
                    return error.SyntaxError;
                },
            };
        }

        fn parseGroup(self: *Self) ParseError!Ast.Node {
            const open = self.advance();
            if (open.tag != .left_paren) {
                self.fail(.left_paren, open);
                return error.SyntaxError;
            }
            self.skipTrivia();
            const node = try self.parseAlternation();
            self.skipTrivia();
            const close = self.advance();
            if (close.tag != .right_paren) {
                self.fail(.right_paren, close);
                return error.SyntaxError;
            }
            return node;
        }

        fn parseOption(self: *Self) ParseError!Ast.Node {
            const open = self.advance();
            if (open.tag != .left_bracket) {
                self.fail(.left_bracket, open);
                return error.SyntaxError;
            }
            self.skipTrivia();
            const inner = try self.parseAlternation();
            self.skipTrivia();
            const close = self.advance();
            if (close.tag != .right_bracket) {
                self.fail(.right_bracket, close);
                return error.SyntaxError;
            }

            const ptr = self.nodes.addOne(inner);
            return .{ .repetition = .{ .min = 0, .max = 1, .element = ptr } };
        }

        fn parseNumVal(self: *Self) !Ast.NumVal {
            const lex = self.advance().lexeme(self.source);
            const base: u8 = switch (lex[1]) {
                'b' => 2,
                'd' => 10,
                'x' => 16,
                else => unreachable,
            };
            const digits = lex[2..];

            if (std.mem.indexOfScalar(u8, digits, '-')) |dash| {
                return .{ .range = .{
                    .lo = try std.fmt.parseInt(u8, digits[0..dash], base),
                    .hi = try std.fmt.parseInt(u8, digits[dash + 1 ..], base),
                } };
            }

            if (std.mem.indexOfScalar(u8, digits, '.')) |_| {
                var buf: [256]u8 = undefined;
                var count: usize = 0;
                var iter = std.mem.splitScalar(u8, digits, '.');
                while (iter.next()) |part| {
                    buf[count] = try std.fmt.parseInt(u8, part, base);
                    count += 1;
                }
                return .{ .concat = self.bytes.addSlice(buf[0..count]) };
            }

            return .{ .single = try std.fmt.parseInt(u8, digits, base) };
        }

        fn parseNumber(self: *Self) !usize {
            return std.fmt.parseInt(usize, self.advance().lexeme(self.source), 10);
        }

        fn isAtRepetition(self: *Self) bool {
            return switch (self.peek().tag) {
                .star,
                .number,
                .left_paren,
                .left_bracket,
                .char_val,
                .char_val_ci,
                .char_val_cs,
                .bin_val,
                .dec_val,
                .hex_val,
                .prose_val,
                => true,
                .rulename => {
                    const next = self.peekNextMeaningful();
                    return next != .equals and next != .equals_slash;
                },
                else => false,
            };
        }
    };
}

const Scanner = @import("Scanner.zig").Scanner;

fn parseSource(source: []const u8) Parser.ParseError!struct { parser: Parser, rules: []const Ast.Rule } {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    if (parser.diagnostics.count != 0) return error.SyntaxError;
    return .{ .parser = parser, .rules = rules };
}

test "single rule with rulename reference" {
    const result = try parseSource("foo = bar");
    try std.testing.expectEqual(1, result.rules.len);
    try std.testing.expectEqualStrings("foo", result.rules[0].name);
    try std.testing.expectEqualStrings("bar", result.rules[0].node.rulename);
}

test "alternation" {
    const result = try parseSource("foo = a / b / c");
    try std.testing.expectEqual(1, result.rules.len);
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(3, alts.len);
    try std.testing.expectEqualStrings("a", alts[0].rulename);
    try std.testing.expectEqualStrings("b", alts[1].rulename);
    try std.testing.expectEqualStrings("c", alts[2].rulename);
}

test "concatenation" {
    const result = try parseSource("foo = a b c");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(3, cat.len);
    try std.testing.expectEqualStrings("a", cat[0].rulename);
    try std.testing.expectEqualStrings("b", cat[1].rulename);
    try std.testing.expectEqualStrings("c", cat[2].rulename);
}

test "repetition star" {
    const result = try parseSource("foo = *bar");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(null, rep.max);
    try std.testing.expectEqualStrings("bar", rep.element.rulename);
}

test "repetition bounded" {
    const result = try parseSource("foo = 3*5bar");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(3, rep.min);
    try std.testing.expectEqual(5, rep.max.?);
    try std.testing.expectEqualStrings("bar", rep.element.rulename);
}

test "repetition exact" {
    const result = try parseSource("foo = 3bar");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(3, rep.min);
    try std.testing.expectEqual(3, rep.max.?);
}

test "char val strips quotes" {
    const result = try parseSource("foo = \"hello\"");
    try std.testing.expectEqualStrings("hello", result.rules[0].node.char_val.value);
    try std.testing.expectEqual(false, result.rules[0].node.char_val.case_sensitive);
}

test "case-sensitive char val (RFC 7405)" {
    const result = try parseSource("foo = %s\"Hello\"");
    try std.testing.expectEqualStrings("Hello", result.rules[0].node.char_val.value);
    try std.testing.expectEqual(true, result.rules[0].node.char_val.case_sensitive);
}

test "explicit case-insensitive char val (RFC 7405)" {
    const result = try parseSource("foo = %i\"Hello\"");
    try std.testing.expectEqualStrings("Hello", result.rules[0].node.char_val.value);
    try std.testing.expectEqual(false, result.rules[0].node.char_val.case_sensitive);
}

test "hex val range" {
    const result = try parseSource("foo = %x41-5A");
    const range = result.rules[0].node.num_val.range;
    try std.testing.expectEqual(0x41, range.lo);
    try std.testing.expectEqual(0x5A, range.hi);
}

test "hex val concat" {
    const result = try parseSource("foo = %x48.65.6C");
    const concat = result.rules[0].node.num_val.concat;
    try std.testing.expectEqualStrings("Hel", concat);
}

test "option desugars to repetition 0..1" {
    const result = try parseSource("foo = [bar]");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(1, rep.max.?);
    try std.testing.expectEqualStrings("bar", rep.element.rulename);
}

test "multiple rules" {
    const result = try parseSource("foo = a\nbar = b");
    try std.testing.expectEqual(2, result.rules.len);
    try std.testing.expectEqualStrings("foo", result.rules[0].name);
    try std.testing.expectEqualStrings("bar", result.rules[1].name);
}

test "incremental alternation preserved unmerged" {
    const result = try parseSource("foo = a\nfoo =/ b");
    try std.testing.expectEqual(2, result.rules.len);
    try std.testing.expectEqualStrings("foo", result.rules[0].name);
    try std.testing.expectEqual(false, result.rules[0].incremental);
    try std.testing.expectEqualStrings("a", result.rules[0].node.rulename);
    try std.testing.expectEqualStrings("foo", result.rules[1].name);
    try std.testing.expectEqual(true, result.rules[1].incremental);
    try std.testing.expectEqualStrings("b", result.rules[1].node.rulename);
}

fn expectSyntaxError(source: []const u8, expected: Diagnostic.Expected, found_tag: Token.Tag) !void {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    _ = try parser.parse();
    const diags = parser.getDiagnostics();
    try std.testing.expect(diags.len > 0);
    try std.testing.expectEqual(expected, diags[0].expected);
    try std.testing.expectEqual(found_tag, diags[0].found_tag);
}

test "diagnostic: unexpected token in element position" {
    try expectSyntaxError("foo = (a / )", .element, .right_paren);
}

test "diagnostic: missing closing paren" {
    try expectSyntaxError("foo = (a", .right_paren, .eof);
}

test "diagnostic: missing closing bracket" {
    try expectSyntaxError("foo = [bar", .right_bracket, .eof);
}

test "recovery: error in first rule, second rule parsed" {
    var scanner = Scanner.init("foo = )\nbar = baz");
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, "foo = )\nbar = baz");
    const rules = try parser.parse();
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("bar", rules[0].name);
    try std.testing.expectEqual(1, parser.getDiagnostics().len);
}

test "recovery: multiple errors accumulate" {
    const source = "a = )\nb = ]\nc = d";
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("c", rules[0].name);
    try std.testing.expectEqual(2, parser.getDiagnostics().len);
}

test "recovery: all rules have errors" {
    const source = "a = )\nb = ]";
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    try std.testing.expectEqual(0, rules.len);
}

test "recovery: unclosed group, next rule still parsed" {
    const source = "a = (b /\nc = d";
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    try std.testing.expect(parser.getDiagnostics().len > 0);
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("c", rules[0].name);
}
