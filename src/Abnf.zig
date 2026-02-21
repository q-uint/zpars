/// Comptime ABNF-to-combinator compiler.
///
/// Parses an ABNF grammar string at comptime and produces a zero-overhead
/// parser combinator type.
///
///     const NumParser = Abnf.Compile("NUM = 1*DIGIT", "NUM");
///     const r = NumParser.parse("42abc").?;
///     // r.value == "42", r.rest == "abc"
const std = @import("std");
const Ast = @import("Ast.zig");
const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");
const c = @import("combinators.zig");

/// Compile an ABNF grammar string into a comptime parser combinator type.
///
/// The returned type has the standard combinator interface:
///   pub const Value = []const u8;
///   pub fn parse(input: []const u8) ?c.Result(Value);
pub fn Compile(comptime grammar: []const u8, comptime start_rule: []const u8) type {
    comptime {
        var scanner = Scanner.init(grammar);
        const tokens = scanner.scanTokens();
        var parser = Parser.init(tokens, grammar);
        const rules = parser.parse() catch @compileError("ABNF grammar has syntax errors");
        if (parser.diagnostics_count > 0) @compileError("ABNF grammar has syntax errors");
        return compileRulename(start_rule, rules, .{});
    }
}

// --- Node compilation --------------------------------------------------------

fn compileNode(comptime node: Ast.Node, comptime rules: []const Ast.Rule, comptime visited: anytype) type {
    return switch (node) {
        .char_val => |cv| if (cv.case_sensitive)
            c.Capture(c.Literal(cv.value))
        else
            c.Capture(c.CaseInsensitiveLiteral(cv.value)),

        .num_val => |nv| switch (nv) {
            .single => |byte| c.Capture(c.ByteLiteral(byte)),
            .range => |r| c.Capture(c.CharRange(r.lo, r.hi)),
            .concat => |bytes| compileByteSeq(bytes),
        },

        .rulename => |name| compileRulename(name, rules, visited),

        .alternation => |alts| compileAlts(alts, rules, visited),

        .concatenation => |elems| compileConcats(elems, rules, visited),

        .repetition => |rep| c.Capture(c.Many(
            compileNode(rep.element.*, rules, visited),
            .{ .min = rep.min, .max = rep.max },
        )),

        .prose_val => @compileError("prose-val cannot be compiled to a parser"),
    };
}

fn compileAlts(comptime alts: []const Ast.Node, comptime rules: []const Ast.Rule, comptime visited: anytype) type {
    if (alts.len == 1) return compileNode(alts[0], rules, visited);
    return c.Choice(
        compileNode(alts[0], rules, visited),
        compileAlts(alts[1..], rules, visited),
    );
}

fn compileConcats(comptime elems: []const Ast.Node, comptime rules: []const Ast.Rule, comptime visited: anytype) type {
    if (elems.len == 1) return compileNode(elems[0], rules, visited);
    return c.Capture(c.Sequence(
        compileNode(elems[0], rules, visited),
        compileConcatsInner(elems[1..], rules, visited),
    ));
}

fn compileConcatsInner(comptime elems: []const Ast.Node, comptime rules: []const Ast.Rule, comptime visited: anytype) type {
    if (elems.len == 1) return compileNode(elems[0], rules, visited);
    return c.Sequence(
        compileNode(elems[0], rules, visited),
        compileConcatsInner(elems[1..], rules, visited),
    );
}

fn compileByteSeq(comptime bytes: []const u8) type {
    if (bytes.len == 1) return c.Capture(c.ByteLiteral(bytes[0]));
    return c.Capture(compileByteSeqInner(bytes));
}

fn compileByteSeqInner(comptime bytes: []const u8) type {
    if (bytes.len == 1) return c.ByteLiteral(bytes[0]);
    return c.Sequence(c.ByteLiteral(bytes[0]), compileByteSeqInner(bytes[1..]));
}

// --- Rule name resolution ----------------------------------------------------

fn compileRulename(comptime name: []const u8, comptime rules: []const Ast.Rule, comptime visited: anytype) type {
    // Cycle detection.
    inline for (visited) |v| {
        if (eqlIgnoreCase(v, name)) @compileError("recursive rule: " ++ name);
    }

    // Core rules (RFC 5234 Appendix B).
    if (eqlIgnoreCase(name, "ALPHA")) return c.Capture(c.Choice(c.CharRange(0x41, 0x5A), c.CharRange(0x61, 0x7A)));
    if (eqlIgnoreCase(name, "BIT")) return c.Capture(c.Choice(c.ByteLiteral('0'), c.ByteLiteral('1')));
    if (eqlIgnoreCase(name, "CHAR")) return c.Capture(c.CharRange(0x01, 0x7F));
    if (eqlIgnoreCase(name, "CR")) return c.Capture(c.ByteLiteral(0x0D));
    if (eqlIgnoreCase(name, "LF")) return c.Capture(c.ByteLiteral(0x0A));
    if (eqlIgnoreCase(name, "CRLF")) return c.Capture(c.Sequence(c.ByteLiteral(0x0D), c.ByteLiteral(0x0A)));
    if (eqlIgnoreCase(name, "CTL")) return c.Capture(c.Choice(c.CharRange(0x00, 0x1F), c.ByteLiteral(0x7F)));
    if (eqlIgnoreCase(name, "DIGIT")) return c.Capture(c.CharRange(0x30, 0x39));
    if (eqlIgnoreCase(name, "DQUOTE")) return c.Capture(c.ByteLiteral(0x22));
    if (eqlIgnoreCase(name, "HEXDIG")) return c.Capture(c.Choice(c.CharRange(0x30, 0x39), c.Choice(c.CharRange(0x41, 0x46), c.CharRange(0x61, 0x66))));
    if (eqlIgnoreCase(name, "HTAB")) return c.Capture(c.ByteLiteral(0x09));
    if (eqlIgnoreCase(name, "OCTET")) return c.Capture(c.CharRange(0x00, 0xFF));
    if (eqlIgnoreCase(name, "SP")) return c.Capture(c.ByteLiteral(0x20));
    if (eqlIgnoreCase(name, "VCHAR")) return c.Capture(c.CharRange(0x21, 0x7E));
    if (eqlIgnoreCase(name, "WSP")) return c.Capture(c.Choice(c.ByteLiteral(0x20), c.ByteLiteral(0x09)));
    if (eqlIgnoreCase(name, "LWSP")) return c.Capture(c.Many(c.Choice(
        c.Capture(c.Choice(c.ByteLiteral(0x20), c.ByteLiteral(0x09))),
        c.Capture(c.Sequence(c.Sequence(c.ByteLiteral(0x0D), c.ByteLiteral(0x0A)), c.Choice(c.ByteLiteral(0x20), c.ByteLiteral(0x09)))),
    ), .{}));

    // User-defined rules: collect all definitions (handles =/).
    const new_visited = visited ++ .{name};
    comptime var matching: [Parser.max_rules]Ast.Node = undefined;
    comptime var count: usize = 0;
    inline for (rules) |rule| {
        if (eqlIgnoreCase(rule.name, name)) {
            matching[count] = rule.node;
            count += 1;
        }
    }

    if (count == 0) @compileError("undefined rule: " ++ name);
    if (count == 1) return compileNode(matching[0], rules, new_visited);

    // Multiple definitions (=/) → alternation.
    return compileAlts(matching[0..count], rules, new_visited);
}

// --- Utilities ---------------------------------------------------------------

fn eqlIgnoreCase(comptime a: []const u8, comptime b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |ac, bc| {
        const al: u8 = if (ac >= 'A' and ac <= 'Z') ac + 32 else ac;
        const bl: u8 = if (bc >= 'A' and bc <= 'Z') bc + 32 else bc;
        if (al != bl) return false;
    }
    return true;
}

// --- Tests -------------------------------------------------------------------

test "single char_val rule (case-insensitive)" {
    const P = Compile("greeting = \"hello\"", "greeting");
    const r = P.parse("Hello world").?;
    try std.testing.expectEqualStrings("Hello", r.value);
    try std.testing.expectEqualStrings(" world", r.rest);
}

test "case-sensitive string" {
    const P = Compile(
        \\foo = %s"Hello"
    , "foo");
    try std.testing.expect(P.parse("Hello") != null);
    try std.testing.expect(P.parse("hello") == null);
}

test "numeric range" {
    const P = Compile("upper = %x41-5A", "upper");
    const r = P.parse("Abc").?;
    try std.testing.expectEqualStrings("A", r.value);
    try std.testing.expectEqualStrings("bc", r.rest);
    try std.testing.expect(P.parse("abc") == null);
}

test "numeric single" {
    const P = Compile("at = %x40", "at");
    try std.testing.expect(P.parse("@") != null);
    try std.testing.expect(P.parse("A") == null);
}

test "numeric concat" {
    const P = Compile("ab = %x41.42", "ab");
    const r = P.parse("ABcd").?;
    try std.testing.expectEqualStrings("AB", r.value);
    try std.testing.expectEqualStrings("cd", r.rest);
}

test "alternation" {
    const P = Compile(
        \\bit = "0" / "1"
    , "bit");
    try std.testing.expect(P.parse("0") != null);
    try std.testing.expect(P.parse("1") != null);
    try std.testing.expect(P.parse("2") == null);
}

test "concatenation" {
    const P = Compile(
        \\pair = %x41 %x42
    , "pair");
    const r = P.parse("ABcd").?;
    try std.testing.expectEqualStrings("AB", r.value);
}

test "repetition" {
    const P = Compile("digits = 1*DIGIT", "digits");
    const r = P.parse("123abc").?;
    try std.testing.expectEqualStrings("123", r.value);
    try std.testing.expectEqualStrings("abc", r.rest);
}

test "repetition bounded" {
    const P = Compile("two = 2*3DIGIT", "two");
    try std.testing.expect(P.parse("1") == null);
    const r = P.parse("123456").?;
    try std.testing.expectEqualStrings("123", r.value);
}

test "option (optional)" {
    const P = Compile("maybe = [DIGIT]", "maybe");
    const r1 = P.parse("5abc").?;
    try std.testing.expectEqualStrings("5", r1.value);
    const r2 = P.parse("abc").?;
    try std.testing.expectEqualStrings("", r2.value);
}

test "multi-rule grammar" {
    const P = Compile(
        \\number = 1*DIGIT
        \\pair = number "," number
    , "pair");
    const r = P.parse("42,7!").?;
    try std.testing.expectEqualStrings("42,7", r.value);
    try std.testing.expectEqualStrings("!", r.rest);
}

test "core rule: ALPHA" {
    const P = Compile("foo = ALPHA", "foo");
    try std.testing.expect(P.parse("A") != null);
    try std.testing.expect(P.parse("z") != null);
    try std.testing.expect(P.parse("5") == null);
}

test "core rule: DIGIT" {
    const P = Compile("foo = DIGIT", "foo");
    try std.testing.expect(P.parse("0") != null);
    try std.testing.expect(P.parse("9") != null);
    try std.testing.expect(P.parse("a") == null);
}

test "core rule: WSP" {
    const P = Compile("foo = WSP", "foo");
    try std.testing.expect(P.parse(" ") != null);
    try std.testing.expect(P.parse("\t") != null);
    try std.testing.expect(P.parse("a") == null);
}

test "incremental alternation (=/)" {
    const P = Compile(
        \\foo = "a"
        \\foo =/ "b"
    , "foo");
    try std.testing.expect(P.parse("a") != null);
    try std.testing.expect(P.parse("b") != null);
    try std.testing.expect(P.parse("c") == null);
}

test "case-insensitive rule name lookup" {
    const P = Compile("Foo = digit", "foo");
    try std.testing.expect(P.parse("5") != null);
}

test "group" {
    const P = Compile(
        \\foo = ("a" / "b") "c"
    , "foo");
    const r1 = P.parse("ac").?;
    try std.testing.expectEqualStrings("ac", r1.value);
    const r2 = P.parse("bc").?;
    try std.testing.expectEqualStrings("bc", r2.value);
    try std.testing.expect(P.parse("cc") == null);
}

test "README: HTTP version" {
    const HttpVersion = Compile(
        \\version = "HTTP/" 1*DIGIT "." 1*DIGIT
    , "version");
    const r = HttpVersion.parse("HTTP/1.1 OK").?;
    try std.testing.expectEqualStrings("HTTP/1.1", r.value);
    try std.testing.expectEqualStrings(" OK", r.rest);
}

test "README: pair" {
    const Pair = Compile(
        \\number = 1*DIGIT
        \\pair   = number "," number
    , "pair");
    const r = Pair.parse("42,7!").?;
    try std.testing.expectEqualStrings("42,7", r.value);
    try std.testing.expectEqualStrings("!", r.rest);
}

test "README: combinators" {
    const Digit = c.CharRange('0', '9');
    const Number = c.Capture(c.Many(Digit, .{ .min = 1 }));
    const P = c.Sequence(Number, c.Sequence(c.Literal(","), Number));
    const r = c.Capture(P).parse("42,7!").?;
    try std.testing.expectEqualStrings("42,7", r.value);
}
