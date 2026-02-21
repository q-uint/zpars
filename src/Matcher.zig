/// Runtime ABNF matcher — tree-walking interpreter over `Ast.Node`.
///
/// Unlike the comptime `Abnf.Compile`, this matcher works with grammars
/// loaded at runtime. Feed it the merged rules from `Validator.validate()`
/// and match arbitrary input strings against any rule.
///
///     var scanner = Scanner.init(grammar);
///     const tokens = scanner.scanTokens();
///     var parser = Parser.init(tokens, grammar);
///     const rules = try parser.parse();
///     var validator = Validator.init(allocator, rules);
///     const merged = try validator.validate();
///     const matcher = Matcher.init(merged);
///     const r = matcher.match("start", "hello world").?;
///     // r.value == "hello", r.rest == " world"
const std = @import("std");
const Ast = @import("Ast.zig");

const Matcher = @This();

pub const Result = struct {
    /// The matched input span.
    value: []const u8,
    /// Unconsumed input after the match.
    rest: []const u8,
};

/// Maximum recursion depth to guard against stack overflow.
const max_depth = 256;

rules: []const Ast.Rule,

pub fn init(rules: []const Ast.Rule) Matcher {
    return .{ .rules = rules };
}

/// Match `input` against the rule named `rule_name`.
/// Returns null if the rule is not found or the input does not match.
pub fn match(self: *const Matcher, rule_name: []const u8, input: []const u8) ?Result {
    return self.matchRulename(rule_name, input, 0);
}

// --- Core matching engine ----------------------------------------------------

fn matchNode(self: *const Matcher, node: Ast.Node, input: []const u8, depth: usize) ?Result {
    if (depth > max_depth) return null;

    return switch (node) {
        .char_val => |cv| matchCharVal(cv, input),
        .num_val => |nv| matchNumVal(nv, input),
        .prose_val => null,
        .rulename => |name| self.matchRulename(name, input, depth),
        .alternation => |alts| self.matchAlternation(alts, input, depth),
        .concatenation => |elems| self.matchConcatenation(elems, input, depth),
        .repetition => |rep| self.matchRepetition(rep, input, depth),
    };
}

fn matchCharVal(cv: Ast.CharVal, input: []const u8) ?Result {
    if (input.len < cv.value.len) return null;
    const span = input[0..cv.value.len];

    if (cv.case_sensitive) {
        if (!std.mem.eql(u8, span, cv.value)) return null;
    } else {
        if (!std.ascii.eqlIgnoreCase(span, cv.value)) return null;
    }

    return .{ .value = span, .rest = input[cv.value.len..] };
}

fn matchNumVal(nv: Ast.NumVal, input: []const u8) ?Result {
    switch (nv) {
        .single => |byte| {
            if (input.len == 0 or input[0] != byte) return null;
            return .{ .value = input[0..1], .rest = input[1..] };
        },
        .range => |r| {
            if (input.len == 0 or input[0] < r.lo or input[0] > r.hi) return null;
            return .{ .value = input[0..1], .rest = input[1..] };
        },
        .concat => |bytes| {
            if (input.len < bytes.len) return null;
            if (!std.mem.eql(u8, input[0..bytes.len], bytes)) return null;
            return .{ .value = input[0..bytes.len], .rest = input[bytes.len..] };
        },
    }
}

fn matchAlternation(self: *const Matcher, alts: []const Ast.Node, input: []const u8, depth: usize) ?Result {
    for (alts) |alt| {
        if (self.matchNode(alt, input, depth)) |r| return r;
    }
    return null;
}

fn matchConcatenation(self: *const Matcher, elems: []const Ast.Node, input: []const u8, depth: usize) ?Result {
    var rest = input;
    for (elems) |elem| {
        const r = self.matchNode(elem, rest, depth) orelse return null;
        rest = r.rest;
    }
    return .{ .value = input[0 .. input.len - rest.len], .rest = rest };
}

fn matchRepetition(self: *const Matcher, rep: Ast.Repetition, input: []const u8, depth: usize) ?Result {
    var rest = input;
    var count: usize = 0;

    while (rep.max == null or count < rep.max.?) {
        const r = self.matchNode(rep.element.*, rest, depth) orelse break;
        // Guard against zero-length matches causing infinite loops.
        if (r.rest.len == rest.len) break;
        rest = r.rest;
        count += 1;
    }

    if (count < rep.min) return null;
    return .{ .value = input[0 .. input.len - rest.len], .rest = rest };
}

fn matchRulename(self: *const Matcher, name: []const u8, input: []const u8, depth: usize) ?Result {
    // Core rules (RFC 5234 Appendix B).
    if (matchCoreRule(name, input)) |r| return r;

    // User-defined rules: find first matching definition.
    // (Rules should already be merged by Validator, but handle multiple
    // definitions as alternation for robustness.)
    var first_result: ?Result = null;
    var found = false;
    for (self.rules) |rule| {
        if (std.ascii.eqlIgnoreCase(rule.name, name)) {
            found = true;
            if (first_result == null) {
                first_result = self.matchNode(rule.node, input, depth + 1);
                if (first_result != null) return first_result;
            }
        }
    }
    if (!found) return null; // Undefined rule.
    return first_result;
}

fn matchCoreRule(name: []const u8, input: []const u8) ?Result {
    const Tag = enum {
        ALPHA,
        BIT,
        CHAR,
        CR,
        LF,
        CRLF,
        CTL,
        DIGIT,
        DQUOTE,
        HEXDIG,
        HTAB,
        LWSP,
        OCTET,
        SP,
        VCHAR,
        WSP,
    };

    const core_rules = comptime .{
        .{ "ALPHA", Tag.ALPHA },
        .{ "BIT", Tag.BIT },
        .{ "CHAR", Tag.CHAR },
        .{ "CR", Tag.CR },
        .{ "LF", Tag.LF },
        .{ "CRLF", Tag.CRLF },
        .{ "CTL", Tag.CTL },
        .{ "DIGIT", Tag.DIGIT },
        .{ "DQUOTE", Tag.DQUOTE },
        .{ "HEXDIG", Tag.HEXDIG },
        .{ "HTAB", Tag.HTAB },
        .{ "LWSP", Tag.LWSP },
        .{ "OCTET", Tag.OCTET },
        .{ "SP", Tag.SP },
        .{ "VCHAR", Tag.VCHAR },
        .{ "WSP", Tag.WSP },
    };

    const tag: Tag = inline for (core_rules) |entry| {
        if (std.ascii.eqlIgnoreCase(entry[0], name)) break entry[1];
    } else return null;

    return switch (tag) {
        .ALPHA => {
            if (input.len == 0) return null;
            if (std.ascii.isAlphabetic(input[0]))
                return .{ .value = input[0..1], .rest = input[1..] };
            return null;
        },
        .BIT => {
            if (input.len == 0) return null;
            if (input[0] == '0' or input[0] == '1')
                return .{ .value = input[0..1], .rest = input[1..] };
            return null;
        },
        .CHAR => {
            if (input.len == 0) return null;
            if (input[0] >= 0x01 and input[0] <= 0x7F)
                return .{ .value = input[0..1], .rest = input[1..] };
            return null;
        },
        .CR => matchByte(input, 0x0D),
        .LF => matchByte(input, 0x0A),
        .CRLF => {
            if (input.len >= 2 and input[0] == 0x0D and input[1] == 0x0A)
                return .{ .value = input[0..2], .rest = input[2..] };
            return null;
        },
        .CTL => {
            if (input.len == 0) return null;
            if (input[0] <= 0x1F or input[0] == 0x7F)
                return .{ .value = input[0..1], .rest = input[1..] };
            return null;
        },
        .DIGIT => {
            if (input.len == 0) return null;
            if (std.ascii.isDigit(input[0]))
                return .{ .value = input[0..1], .rest = input[1..] };
            return null;
        },
        .DQUOTE => matchByte(input, 0x22),
        .HEXDIG => {
            if (input.len == 0) return null;
            if (std.ascii.isHex(input[0]))
                return .{ .value = input[0..1], .rest = input[1..] };
            return null;
        },
        .HTAB => matchByte(input, 0x09),
        .LWSP => {
            // *(WSP / CRLF WSP) — zero or more.
            var rest = input;
            while (rest.len > 0) {
                if (rest[0] == 0x20 or rest[0] == 0x09) {
                    rest = rest[1..];
                } else if (rest.len >= 3 and rest[0] == 0x0D and rest[1] == 0x0A and
                    (rest[2] == 0x20 or rest[2] == 0x09))
                {
                    rest = rest[3..];
                } else break;
            }
            return .{ .value = input[0 .. input.len - rest.len], .rest = rest };
        },
        .OCTET => {
            if (input.len == 0) return null;
            return .{ .value = input[0..1], .rest = input[1..] };
        },
        .SP => matchByte(input, 0x20),
        .VCHAR => {
            if (input.len == 0) return null;
            if (input[0] >= 0x21 and input[0] <= 0x7E)
                return .{ .value = input[0..1], .rest = input[1..] };
            return null;
        },
        .WSP => {
            if (input.len == 0) return null;
            if (input[0] == 0x20 or input[0] == 0x09)
                return .{ .value = input[0..1], .rest = input[1..] };
            return null;
        },
    };
}

fn matchByte(input: []const u8, expected: u8) ?Result {
    if (input.len == 0 or input[0] != expected) return null;
    return .{ .value = input[0..1], .rest = input[1..] };
}

// --- Tests -------------------------------------------------------------------

const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");
const Validator = @import("Validator.zig");

/// Parse an ABNF grammar, validate it, and return a Matcher ready to use.
fn compileMatcher(allocator: std.mem.Allocator, grammar: []const u8) !struct { matcher: Matcher, arena: std.heap.ArenaAllocator } {
    var scanner = Scanner.init(grammar);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, grammar);
    const rules = try parser.parse();
    std.debug.assert(parser.getDiagnostics().len == 0);
    var arena = std.heap.ArenaAllocator.init(allocator);
    var validator = Validator.init(arena.allocator(), rules);
    const merged = try validator.validate();
    return .{ .matcher = Matcher.init(merged), .arena = arena };
}

test "single char_val rule (case-insensitive)" {
    var ctx = try compileMatcher(std.testing.allocator, "greeting = \"hello\"");
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("greeting", "Hello world").?;
    try std.testing.expectEqualStrings("Hello", r.value);
    try std.testing.expectEqualStrings(" world", r.rest);
}

test "case-sensitive string" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\foo = %s"Hello"
    );
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "Hello") != null);
    try std.testing.expect(ctx.matcher.match("foo", "hello") == null);
}

test "numeric range" {
    var ctx = try compileMatcher(std.testing.allocator, "upper = %x41-5A");
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("upper", "Abc").?;
    try std.testing.expectEqualStrings("A", r.value);
    try std.testing.expectEqualStrings("bc", r.rest);
    try std.testing.expect(ctx.matcher.match("upper", "abc") == null);
}

test "numeric single" {
    var ctx = try compileMatcher(std.testing.allocator, "at = %x40");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("at", "@") != null);
    try std.testing.expect(ctx.matcher.match("at", "A") == null);
}

test "numeric concat" {
    var ctx = try compileMatcher(std.testing.allocator, "ab = %x41.42");
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("ab", "ABcd").?;
    try std.testing.expectEqualStrings("AB", r.value);
    try std.testing.expectEqualStrings("cd", r.rest);
}

test "alternation" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\bit = "0" / "1"
    );
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("bit", "0") != null);
    try std.testing.expect(ctx.matcher.match("bit", "1") != null);
    try std.testing.expect(ctx.matcher.match("bit", "2") == null);
}

test "concatenation" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\pair = %x41 %x42
    );
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("pair", "ABcd").?;
    try std.testing.expectEqualStrings("AB", r.value);
}

test "repetition" {
    var ctx = try compileMatcher(std.testing.allocator, "digits = 1*DIGIT");
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("digits", "123abc").?;
    try std.testing.expectEqualStrings("123", r.value);
    try std.testing.expectEqualStrings("abc", r.rest);
}

test "repetition bounded" {
    var ctx = try compileMatcher(std.testing.allocator, "two = 2*3DIGIT");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("two", "1") == null);
    const r = ctx.matcher.match("two", "123456").?;
    try std.testing.expectEqualStrings("123", r.value);
}

test "option (optional)" {
    var ctx = try compileMatcher(std.testing.allocator, "maybe = [DIGIT]");
    defer ctx.arena.deinit();
    const r1 = ctx.matcher.match("maybe", "5abc").?;
    try std.testing.expectEqualStrings("5", r1.value);
    const r2 = ctx.matcher.match("maybe", "abc").?;
    try std.testing.expectEqualStrings("", r2.value);
}

test "multi-rule grammar" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\number = 1*DIGIT
        \\pair = number "," number
    );
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("pair", "42,7!").?;
    try std.testing.expectEqualStrings("42,7", r.value);
    try std.testing.expectEqualStrings("!", r.rest);
}

test "core rule: ALPHA" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = ALPHA");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "A") != null);
    try std.testing.expect(ctx.matcher.match("foo", "z") != null);
    try std.testing.expect(ctx.matcher.match("foo", "5") == null);
}

test "core rule: DIGIT" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = DIGIT");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "0") != null);
    try std.testing.expect(ctx.matcher.match("foo", "9") != null);
    try std.testing.expect(ctx.matcher.match("foo", "a") == null);
}

test "core rule: WSP" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = WSP");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", " ") != null);
    try std.testing.expect(ctx.matcher.match("foo", "\t") != null);
    try std.testing.expect(ctx.matcher.match("foo", "a") == null);
}

test "incremental alternation (=/)" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\foo = "a"
        \\foo =/ "b"
    );
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "a") != null);
    try std.testing.expect(ctx.matcher.match("foo", "b") != null);
    try std.testing.expect(ctx.matcher.match("foo", "c") == null);
}

test "case-insensitive rule name lookup" {
    var ctx = try compileMatcher(std.testing.allocator, "Foo = digit");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "5") != null);
}

test "group with alternation" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\foo = ("a" / "b") "c"
    );
    defer ctx.arena.deinit();
    const r1 = ctx.matcher.match("foo", "ac").?;
    try std.testing.expectEqualStrings("ac", r1.value);
    const r2 = ctx.matcher.match("foo", "bc").?;
    try std.testing.expectEqualStrings("bc", r2.value);
    try std.testing.expect(ctx.matcher.match("foo", "cc") == null);
}

test "HTTP version" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\version = "HTTP/" 1*DIGIT "." 1*DIGIT
    );
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("version", "HTTP/1.1 OK").?;
    try std.testing.expectEqualStrings("HTTP/1.1", r.value);
    try std.testing.expectEqualStrings(" OK", r.rest);
}

test "pair" {
    var ctx = try compileMatcher(std.testing.allocator,
        \\number = 1*DIGIT
        \\pair   = number "," number
    );
    defer ctx.arena.deinit();
    const r = ctx.matcher.match("pair", "42,7!").?;
    try std.testing.expectEqualStrings("42,7", r.value);
    try std.testing.expectEqualStrings("!", r.rest);
}

test "undefined rule returns null" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = \"a\"");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("nonexistent", "a") == null);
}

test "empty input" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = \"a\"");
    defer ctx.arena.deinit();
    try std.testing.expect(ctx.matcher.match("foo", "") == null);
}

test "repetition star (zero or more)" {
    var ctx = try compileMatcher(std.testing.allocator, "foo = *DIGIT");
    defer ctx.arena.deinit();
    const r1 = ctx.matcher.match("foo", "abc").?;
    try std.testing.expectEqualStrings("", r1.value);
    const r2 = ctx.matcher.match("foo", "123abc").?;
    try std.testing.expectEqualStrings("123", r2.value);
}
