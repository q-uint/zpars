/// Comptime ABNF-to-tokenizer compiler.
///
/// Parses an ABNF grammar string at comptime and produces a zero-overhead
/// scanner type with a dispatch-table architecture.
///
///     const BnfScanner = Tokenizer.CompileScanner(
///         \\rulename   = "<" 1*(%x20-3D / %x3F-7E) ">"
///         \\definition = "::="
///         \\pipe       = "|"
///         \\newline    = %x0D %x0A / %x0A
///     , .{ .skip = &.{ ' ', '\t' }, .catch_all = "terminal", .line_tag = "newline" });
///
///     var scanner = BnfScanner.init(source);
///     const tokens = scanner.scanTokens();
const std = @import("std");
const Ast = @import("../Ast.zig");
const AbnfScanner = @import("Scanner.zig").Scanner;
const AbnfParser = @import("Parser.zig").Parser;
const tok = @import("../token.zig");

pub const Config = struct {
    skip: []const u8 = &.{},
    catch_all: ?[:0]const u8 = null,
    line_tag: ?[:0]const u8 = null,
};

pub fn CompileScanner(comptime grammar: []const u8, comptime config: Config) type {
    comptime {
        @setEvalBranchQuota(10_000);
        var scanner = AbnfScanner.init(grammar);
        const tokens = scanner.scanTokens();
        var parser = AbnfParser.init(tokens, grammar);
        const rules = parser.parse() catch @compileError("ABNF grammar has syntax errors");
        if (parser.diagnostics.count > 0) @compileError("ABNF grammar has syntax errors");
        return buildScannerType(rules, config);
    }
}

fn buildScannerType(comptime rules: []const Ast.Rule, comptime config: Config) type {
    const rule_count = rules.len;
    const has_catch_all = config.catch_all != null;
    const tag_count = rule_count + @as(usize, if (has_catch_all) 1 else 0) + 2; // +eof +invalid

    const GeneratedTag = @Type(.{ .@"enum" = .{
        .tag_type = std.math.IntFittingRange(0, tag_count - 1),
        .fields = blk: {
            var fields: [tag_count]std.builtin.Type.EnumField = undefined;
            var i: usize = 0;
            for (rules) |rule| {
                const name_z: [:0]const u8 = (rule.name ++ .{0})[0..rule.name.len :0];
                fields[i] = .{ .name = name_z, .value = i };
                i += 1;
            }
            if (has_catch_all) {
                fields[i] = .{ .name = config.catch_all.?, .value = i };
                i += 1;
            }
            fields[i] = .{ .name = "eof", .value = i };
            i += 1;
            fields[i] = .{ .name = "invalid", .value = i };
            break :blk &fields;
        },
        .decls = &.{},
        .is_exhaustive = true,
    } });

    const GeneratedToken = tok.Token(GeneratedTag);

    const skip_set = comptime sk: {
        var s: [256]bool = @splat(false);
        for (config.skip) |b| s[b] = true;
        break :sk s;
    };

    const first_bytes: [rule_count][256]bool = comptime fb: {
        var result: [rule_count][256]bool = undefined;
        for (0..rule_count) |i| {
            result[i] = computeFirstBytes(rules[i].node, rules);
        }
        break :fb result;
    };

    // any_rule_byte[b] is true if byte b is a first-byte of any rule.
    const any_rule_byte: [256]bool = comptime arb: {
        var result: [256]bool = @splat(false);
        for (0..rule_count) |i| {
            for (0..256) |b| {
                if (first_bytes[i][b]) result[b] = true;
            }
        }
        break :arb result;
    };

    const MaskInt = std.meta.Int(.unsigned, @max(rule_count, 1));

    const Action = union(enum) {
        single: GeneratedTag,
        skip,
        handler: MaskInt,
        catch_all,
        invalid,
    };

    const dispatch_table: [256]Action = comptime dt: {
        var t: [256]Action = @splat(if (has_catch_all)
            Action.catch_all
        else
            Action.invalid);

        for (config.skip) |b| t[b] = .skip;

        for (0..256) |b| {
            if (skip_set[b]) continue;

            var mask: MaskInt = 0;
            var count: usize = 0;
            for (0..rule_count) |i| {
                if (first_bytes[i][b]) {
                    mask |= @as(MaskInt, 1) << @intCast(i);
                    count += 1;
                }
            }

            if (count == 0) continue;

            if (count == 1) {
                for (0..rule_count) |i| {
                    if (first_bytes[i][b]) {
                        if (isSingleByte(rules[i].node))
                            t[b] = .{ .single = @enumFromInt(i) }
                        else
                            t[b] = .{ .handler = mask };
                        break;
                    }
                }
            } else {
                t[b] = .{ .handler = mask };
            }
        }

        break :dt t;
    };

    // Pre-generate matcher types for each rule. Each type has a
    // match(source, current) method that consumes input bytes. Types don't
    // carry pointers into the parser's pools, avoiding the "captured value
    // contains reference to comptime var" error.
    const rule_matchers = comptime rm: {
        var m: [rule_count]type = undefined;
        for (0..rule_count) |i| {
            m[i] = NodeMatcher(rules[i].node, rules);
        }
        break :rm m;
    };

    return struct {
        const Self = @This();
        pub const Tag = GeneratedTag;
        pub const Token = GeneratedToken;
        pub const max_tokens = 4096;

        source: []const u8,
        tokens: [max_tokens]Token = undefined,
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

        fn scanToken(self: *Self) void {
            const c = self.advanceByte();
            switch (dispatch_table[c]) {
                .single => |tag| self.addToken(tag),
                .skip => {},
                .handler => |mask| {
                    self.current = self.start;
                    var best_end: usize = self.start;
                    var best_tag: Tag = .invalid;
                    inline for (0..rule_count) |i| {
                        if (mask & (@as(MaskInt, 1) << i) != 0) {
                            var pos = self.start;
                            if (rule_matchers[i].match(self.source, &pos)) {
                                if (pos > best_end) {
                                    best_end = pos;
                                    best_tag = @enumFromInt(i);
                                }
                            }
                        }
                    }
                    if (best_end > self.start) {
                        self.current = best_end;
                        self.addToken(best_tag);
                    } else {
                        self.current = self.start + 1;
                        self.addToken(.invalid);
                    }
                },
                .catch_all => {
                    // Only reachable when `has_catch_all` is true; the
                    // comptime guard keeps `config.catch_all.?` from
                    // being analyzed in configs without a catch-all.
                    if (comptime has_catch_all) {
                        while (!self.isAtEnd()) {
                            const p = self.peekByte();
                            if (skip_set[p] or any_rule_byte[p]) break;
                            _ = self.advanceByte();
                        }
                        self.addToken(@field(Tag, config.catch_all.?));
                    } else unreachable;
                },
                .invalid => self.addToken(.invalid),
            }
        }

        fn advanceByte(self: *Self) u8 {
            const b = self.source[self.current];
            self.current += 1;
            return b;
        }

        fn peekByte(self: *Self) u8 {
            if (self.isAtEnd()) return 0;
            return self.source[self.current];
        }

        fn isAtEnd(self: *Self) bool {
            return self.current >= self.source.len;
        }

        fn addToken(self: *Self, tag: Tag) void {
            // Record the token first (so a newline token keeps the
            // line it *terminates*), then advance the line counter for
            // subsequent tokens.
            self.tokens[self.token_count] = .{
                .tag = tag,
                .start = self.start,
                .len = self.current - self.start,
                .line = self.line,
            };
            self.token_count += 1;
            if (comptime config.line_tag) |lt| {
                if (tag == @field(Tag, lt)) self.line += 1;
            }
        }
    };
}

/// Generate a matcher type for an AST node. The returned type has a single
/// method: `match(source: []const u8, current: *usize) bool` that tries
/// to match starting at `current.*`, advancing it on success.
///
/// This follows the same pattern as Compiler.zig: resolve AST references
/// into types at comptime so the result carries no pointers into the
/// parser's mutable pools.
fn NodeMatcher(comptime node: Ast.Node, comptime rules: []const Ast.Rule) type {
    return switch (node) {
        .char_val => |cv| struct {
            pub fn match(source: []const u8, current: *usize) bool {
                if (current.* + cv.value.len > source.len) return false;
                const slice = source[current.*..][0..cv.value.len];
                if (cv.case_sensitive) {
                    if (!std.mem.eql(u8, slice, cv.value)) return false;
                } else {
                    for (slice, cv.value) |a, b| {
                        const al: u8 = if (a >= 'A' and a <= 'Z') a + 32 else a;
                        const bl: u8 = if (b >= 'A' and b <= 'Z') b + 32 else b;
                        if (al != bl) return false;
                    }
                }
                current.* += cv.value.len;
                return true;
            }
        },
        .num_val => |nv| switch (nv) {
            .single => |byte| struct {
                pub fn match(source: []const u8, current: *usize) bool {
                    if (current.* >= source.len) return false;
                    if (source[current.*] != byte) return false;
                    current.* += 1;
                    return true;
                }
            },
            .range => |r| struct {
                pub fn match(source: []const u8, current: *usize) bool {
                    if (current.* >= source.len) return false;
                    const b = source[current.*];
                    if (b < r.lo or b > r.hi) return false;
                    current.* += 1;
                    return true;
                }
            },
            .concat => |bytes| struct {
                pub fn match(source: []const u8, current: *usize) bool {
                    if (current.* + bytes.len > source.len) return false;
                    const slice = source[current.*..][0..bytes.len];
                    if (!std.mem.eql(u8, slice, bytes)) return false;
                    current.* += bytes.len;
                    return true;
                }
            },
        },
        .concatenation => |elems| blk: {
            var m: [elems.len]type = undefined;
            for (elems, 0..) |e, i| m[i] = NodeMatcher(e, rules);
            const matchers = m;
            break :blk struct {
                pub fn match(source: []const u8, current: *usize) bool {
                    const saved = current.*;
                    inline for (matchers) |M| {
                        if (!M.match(source, current)) {
                            current.* = saved;
                            return false;
                        }
                    }
                    return true;
                }
            };
        },
        .alternation => |alts| blk: {
            var m: [alts.len]type = undefined;
            for (alts, 0..) |a, i| m[i] = NodeMatcher(a, rules);
            const matchers = m;
            break :blk struct {
                pub fn match(source: []const u8, current: *usize) bool {
                    inline for (matchers) |M| {
                        const saved = current.*;
                        if (M.match(source, current)) return true;
                        current.* = saved;
                    }
                    return false;
                }
            };
        },
        .repetition => |rep| blk: {
            const elem_matcher = NodeMatcher(rep.element.*, rules);
            const min = rep.min;
            const limit = rep.max orelse 4096;
            break :blk struct {
                pub fn match(source: []const u8, current: *usize) bool {
                    var count: usize = 0;
                    while (count < limit) {
                        const saved = current.*;
                        if (!elem_matcher.match(source, current)) {
                            current.* = saved;
                            break;
                        }
                        count += 1;
                    }
                    return count >= min;
                }
            };
        },
        .rulename => |name| blk: {
            var resolved: ?type = null;
            for (rules) |rule| {
                if (eqlIgnoreCase(rule.name, name)) {
                    resolved = NodeMatcher(rule.node, rules);
                    break;
                }
            }
            const rule_matcher = resolved orelse @compileError("undefined rule: " ++ name);
            break :blk struct {
                pub fn match(source: []const u8, current: *usize) bool {
                    return rule_matcher.match(source, current);
                }
            };
        },
        else => struct {
            pub fn match(_: []const u8, _: *usize) bool {
                return false;
            }
        },
    };
}

fn computeFirstBytes(comptime node: Ast.Node, comptime rules: []const Ast.Rule) [256]bool {
    return switch (node) {
        .char_val => |cv| blk: {
            var result: [256]bool = @splat(false);
            if (cv.value.len > 0) {
                if (cv.case_sensitive) {
                    result[cv.value[0]] = true;
                } else {
                    const c = cv.value[0];
                    if (c >= 'A' and c <= 'Z') {
                        result[c] = true;
                        result[c + 32] = true;
                    } else if (c >= 'a' and c <= 'z') {
                        result[c] = true;
                        result[c - 32] = true;
                    } else {
                        result[c] = true;
                    }
                }
            }
            break :blk result;
        },
        .num_val => |nv| switch (nv) {
            .single => |byte| blk: {
                var result: [256]bool = @splat(false);
                result[byte] = true;
                break :blk result;
            },
            .range => |r| blk: {
                var result: [256]bool = @splat(false);
                for (r.lo..@as(u16, r.hi) + 1) |b| result[b] = true;
                break :blk result;
            },
            .concat => |bytes| blk: {
                var result: [256]bool = @splat(false);
                if (bytes.len > 0) result[bytes[0]] = true;
                break :blk result;
            },
        },
        .concatenation => |elems| if (elems.len > 0) computeFirstBytes(elems[0], rules) else @as([256]bool, @splat(false)),
        .alternation => |alts| blk: {
            var result: [256]bool = @splat(false);
            for (alts) |alt| {
                const fb = computeFirstBytes(alt, rules);
                for (0..256) |b| {
                    if (fb[b]) result[b] = true;
                }
            }
            break :blk result;
        },
        .repetition => |rep| computeFirstBytes(rep.element.*, rules),
        .rulename => |name| blk: {
            for (rules) |rule| {
                if (eqlIgnoreCase(rule.name, name)) {
                    return computeFirstBytes(rule.node, rules);
                }
            }
            break :blk @as([256]bool, @splat(false));
        },
        else => @as([256]bool, @splat(false)),
    };
}

fn isSingleByte(comptime node: Ast.Node) bool {
    return switch (node) {
        .num_val => |nv| switch (nv) {
            .single => true,
            else => false,
        },
        .char_val => |cv| cv.value.len == 1,
        else => false,
    };
}

fn eqlIgnoreCase(comptime a: []const u8, comptime b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |ac, bc| {
        const al: u8 = if (ac >= 'A' and ac <= 'Z') ac + 32 else ac;
        const bl: u8 = if (bc >= 'A' and bc <= 'Z') bc + 32 else bc;
        if (al != bl) return false;
    }
    return true;
}

const testing = std.testing;

fn expectTags(comptime S: type, source: []const u8, expected: []const S.Tag) !void {
    var scanner = S.init(source);
    const tokens = scanner.scanTokens();
    const actual = try testing.allocator.alloc(S.Tag, tokens.len);
    defer testing.allocator.free(actual);
    for (tokens, 0..) |t, i| actual[i] = t.tag;
    try testing.expectEqualSlices(S.Tag, expected, actual);
}

test "first-byte conflict: longest match wins" {
    // ABNF rulenames use `-` (not `_`) per RFC 5234. Reference the
    // generated enum fields via `@"..."` since hyphens aren't legal
    // Zig identifier characters.
    const S = CompileScanner(
        \\eq       = "="
        \\eq-slash = "=/"
    , .{});
    try expectTags(S, "=/", &.{ .@"eq-slash", .eof });
    try expectTags(S, "=", &.{ .eq, .eof });
}

test "first-byte conflict: declaration order breaks ties" {
    const S = CompileScanner(
        \\keyword = "if"
        \\ident   = 1*(%x61-7A)
    , .{});
    // Both match "if" (2 bytes). "keyword" is declared first, so it wins.
    try expectTags(S, "if", &.{ .keyword, .eof });
    // "foo" only matches ident.
    try expectTags(S, "foo", &.{ .ident, .eof });
}

test "first-byte conflict: multiple prefixed rules" {
    const S = CompileScanner(
        \\hex = "%x" 1*(%x30-39 / %x41-46 / %x61-66)
        \\dec = "%d" 1*(%x30-39)
        \\bin = "%b" 1*(%x30-31)
    , .{});
    try expectTags(S, "%xFF", &.{ .hex, .eof });
    try expectTags(S, "%d42", &.{ .dec, .eof });
    try expectTags(S, "%b01", &.{ .bin, .eof });
}
