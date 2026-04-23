/// Comptime parser combinators.
///
/// Each "parser" is a type with a static `parse` method and a `Value` decl:
///
///     pub const Value = T;
///     pub fn parse(input: []const u8) ?Result(Value);
///
/// Combinators are `fn(...) type` functions that compose parsers into new ones.
/// Everything is resolved at comptime — zero runtime overhead.
const std = @import("std");

// Core result type

pub fn Result(comptime T: type) type {
    return struct {
        value: T,
        rest: []const u8,
    };
}

// Primitive parsers

/// Match an exact string literal.
pub fn Literal(comptime str: []const u8) type {
    return struct {
        pub const Value = void;

        pub fn parse(input: []const u8) ?Result(Value) {
            if (input.len < str.len) return null;
            if (!std.mem.eql(u8, input[0..str.len], str)) return null;
            return .{ .value = {}, .rest = input[str.len..] };
        }
    };
}

/// Match a single byte satisfying a comptime predicate.
pub fn Char(comptime predicate: fn (u8) bool) type {
    return struct {
        pub const Value = u8;

        pub fn parse(input: []const u8) ?Result(Value) {
            if (input.len == 0) return null;
            if (!predicate(input[0])) return null;
            return .{ .value = input[0], .rest = input[1..] };
        }
    };
}

/// Match a single byte in the inclusive range `lo..hi`.
pub fn CharRange(comptime lo: u8, comptime hi: u8) type {
    return Char(struct {
        fn f(c: u8) bool {
            return c >= lo and c <= hi;
        }
    }.f);
}

/// Match a single byte that appears in `chars`.
pub fn AnyOf(comptime chars: []const u8) type {
    return Char(struct {
        fn f(c: u8) bool {
            inline for (chars) |x| if (c == x) return true;
            return false;
        }
    }.f);
}

/// Match a single byte that does not appear in `chars`.
pub fn NoneOf(comptime chars: []const u8) type {
    return Char(struct {
        fn f(c: u8) bool {
            inline for (chars) |x| if (c == x) return false;
            return true;
        }
    }.f);
}

/// Match any single byte.
pub const Any = struct {
    pub const Value = u8;

    pub fn parse(input: []const u8) ?Result(Value) {
        if (input.len == 0) return null;
        return .{ .value = input[0], .rest = input[1..] };
    }
};

/// Match end of input.
pub const Eof = struct {
    pub const Value = void;

    pub fn parse(input: []const u8) ?Result(Value) {
        if (input.len != 0) return null;
        return .{ .value = {}, .rest = input };
    }
};

// Combinators

/// Run each parser in `parsers` in order; produce a tuple of their values.
/// `parsers` must be a tuple of parser types, e.g. `Seq(.{A, B, C})`.
pub fn Seq(comptime parsers: anytype) type {
    const N = parsers.len;
    if (N == 0) @compileError("Seq requires at least one parser");

    comptime var types: [N]type = undefined;
    inline for (parsers, 0..) |P, i| types[i] = P.Value;
    const ValueT = std.meta.Tuple(&types);

    return struct {
        pub const Value = ValueT;

        pub fn parse(input: []const u8) ?Result(Value) {
            var value: Value = undefined;
            var rest = input;
            inline for (parsers, 0..) |P, i| {
                const r = P.parse(rest) orelse return null;
                value[i] = r.value;
                rest = r.rest;
            }
            return .{ .value = value, .rest = rest };
        }
    };
}

/// Try each parser in `parsers` in order; return the first success.
/// All parsers must produce the same `Value` type.
pub fn Alt(comptime parsers: anytype) type {
    const N = parsers.len;
    if (N == 0) @compileError("Alt requires at least one parser");

    const ValueT = parsers[0].Value;
    inline for (parsers) |P| {
        if (P.Value != ValueT) @compileError("Alt requires all parsers to have the same Value type");
    }

    return struct {
        pub const Value = ValueT;

        pub fn parse(input: []const u8) ?Result(Value) {
            inline for (parsers) |P| {
                if (P.parse(input)) |r| return r;
            }
            return null;
        }
    };
}

/// Zero-width negative lookahead: succeed iff `P` fails, consuming no input.
pub fn Not(comptime P: type) type {
    return struct {
        pub const Value = void;

        pub fn parse(input: []const u8) ?Result(Value) {
            if (P.parse(input) != null) return null;
            return .{ .value = {}, .rest = input };
        }
    };
}

/// Repetition bounds for `Many`.
pub const Bounds = struct {
    min: usize = 0,
    max: ?usize = null,
};

/// Match `P` repeatedly within the given bounds. Produces a slice of values.
///
/// Results are written into a per-instantiation threadlocal buffer of size
/// `bounds.max orelse 4096`. The returned `.value` slice points into that
/// buffer, so callers must consume it before the next call to
/// `Many(P, bounds).parse` on the same thread — a second call clobbers the
/// first's results. Wrap with `Capture` if you need a slice of `input` instead.
pub fn Many(comptime P: type, comptime bounds: Bounds) type {
    const limit = bounds.max orelse 4096;

    return struct {
        pub const Value = []const P.Value;

        threadlocal var buf: [limit]P.Value = undefined;

        pub fn parse(input: []const u8) ?Result(Value) {
            var count: usize = 0;
            var rest = input;

            while (count < limit) {
                const r = P.parse(rest) orelse break;
                buf[count] = r.value;
                count += 1;
                rest = r.rest;
            }

            if (count < bounds.min) return null;
            return .{ .value = buf[0..count], .rest = rest };
        }
    };
}

/// Match `P` zero or one time; produce `?P.Value`.
pub fn Optional(comptime P: type) type {
    return struct {
        pub const Value = ?P.Value;

        pub fn parse(input: []const u8) ?Result(Value) {
            if (P.parse(input)) |r| {
                return .{ .value = r.value, .rest = r.rest };
            }
            return .{ .value = null, .rest = input };
        }
    };
}

/// Transform the result of `P` through a comptime function.
pub fn Map(comptime P: type, comptime mapFn: anytype) type {
    const R = @typeInfo(@TypeOf(mapFn)).@"fn".return_type.?;

    return struct {
        pub const Value = R;

        pub fn parse(input: []const u8) ?Result(Value) {
            const r = P.parse(input) orelse return null;
            return .{ .value = mapFn(r.value), .rest = r.rest };
        }
    };
}

// ABNF support combinators

/// Match a single exact byte value.
pub fn ByteLiteral(comptime byte: u8) type {
    return Char(struct {
        fn f(c: u8) bool {
            return c == byte;
        }
    }.f);
}

/// Match a string literal case-insensitively (ASCII only).
/// ABNF `"text"` is case-insensitive by default (RFC 5234 §2.3).
pub fn CaseInsensitiveLiteral(comptime str: []const u8) type {
    return struct {
        pub const Value = void;

        pub fn parse(input: []const u8) ?Result(Value) {
            if (input.len < str.len) return null;
            inline for (str, 0..) |expected, i| {
                if (toLower(input[i]) != toLower(expected)) return null;
            }
            return .{ .value = {}, .rest = input[str.len..] };
        }

        fn toLower(c: u8) u8 {
            return if (c >= 'A' and c <= 'Z') c + 32 else c;
        }
    };
}

/// Run inner parser `P`; on success, produce the matched input span
/// as `[]const u8` rather than `P`'s native Value type.
pub fn Capture(comptime P: type) type {
    return struct {
        pub const Value = []const u8;

        pub fn parse(input: []const u8) ?Result(Value) {
            const r = P.parse(input) orelse return null;
            return .{
                .value = input[0 .. input.len - r.rest.len],
                .rest = r.rest,
            };
        }
    };
}

// Tests

test "Literal matches exact string" {
    const P = Literal("hello");
    const r = P.parse("hello world").?;
    try std.testing.expectEqualStrings(" world", r.rest);
}

test "Literal rejects mismatch" {
    const P = Literal("hello");
    try std.testing.expect(P.parse("helo") == null);
}

test "Literal rejects short input" {
    const P = Literal("hello");
    try std.testing.expect(P.parse("hel") == null);
}

test "Char matches predicate" {
    const P = Char(std.ascii.isAlphabetic);
    const r = P.parse("abc").?;
    try std.testing.expectEqual('a', r.value);
    try std.testing.expectEqualStrings("bc", r.rest);
}

test "Char rejects non-match" {
    const P = Char(std.ascii.isAlphabetic);
    try std.testing.expect(P.parse("123") == null);
}

test "CharRange matches inclusive range" {
    const P = CharRange('a', 'z');
    const r = P.parse("m!").?;
    try std.testing.expectEqual('m', r.value);
    try std.testing.expectEqualStrings("!", r.rest);
}

test "CharRange rejects out-of-range" {
    const P = CharRange('a', 'z');
    try std.testing.expect(P.parse("A") == null);
}

test "AnyOf matches member byte" {
    const P = AnyOf("+-*/");
    const r = P.parse("*x").?;
    try std.testing.expectEqual('*', r.value);
    try std.testing.expectEqualStrings("x", r.rest);
}

test "AnyOf rejects non-member" {
    const P = AnyOf("+-*/");
    try std.testing.expect(P.parse("a") == null);
}

test "AnyOf rejects empty input" {
    const P = AnyOf("+-*/");
    try std.testing.expect(P.parse("") == null);
}

test "NoneOf matches non-member byte" {
    const P = NoneOf("\"\\");
    const r = P.parse("ab").?;
    try std.testing.expectEqual('a', r.value);
    try std.testing.expectEqualStrings("b", r.rest);
}

test "NoneOf rejects member" {
    const P = NoneOf("\"\\");
    try std.testing.expect(P.parse("\"x") == null);
}

test "NoneOf rejects empty input" {
    const P = NoneOf("\"\\");
    try std.testing.expect(P.parse("") == null);
}

test "Any matches single byte" {
    const r = Any.parse("xy").?;
    try std.testing.expectEqual('x', r.value);
    try std.testing.expectEqualStrings("y", r.rest);
}

test "Any rejects empty input" {
    try std.testing.expect(Any.parse("") == null);
}

test "Eof matches empty input" {
    const r = Eof.parse("").?;
    try std.testing.expectEqual({}, r.value);
}

test "Eof rejects non-empty input" {
    try std.testing.expect(Eof.parse("x") == null);
}

test "Seq chains two parsers" {
    const P = Seq(.{ Literal("ab"), Literal("cd") });
    const r = P.parse("abcdef").?;
    try std.testing.expectEqualStrings("ef", r.rest);
}

test "Seq fails if first fails" {
    const P = Seq(.{ Literal("ab"), Literal("cd") });
    try std.testing.expect(P.parse("xxcd") == null);
}

test "Seq fails if second fails" {
    const P = Seq(.{ Literal("ab"), Literal("cd") });
    try std.testing.expect(P.parse("abxx") == null);
}

test "Seq chains three parsers" {
    const P = Seq(.{ Literal("ab"), Literal("cd"), Literal("ef") });
    const r = P.parse("abcdefgh").?;
    try std.testing.expectEqualStrings("gh", r.rest);
}

test "Seq exposes tuple of values" {
    const P = Seq(.{ CharRange('0', '9'), CharRange('a', 'z'), CharRange('0', '9') });
    const r = P.parse("1a2rest").?;
    try std.testing.expectEqual('1', r.value[0]);
    try std.testing.expectEqual('a', r.value[1]);
    try std.testing.expectEqual('2', r.value[2]);
    try std.testing.expectEqualStrings("rest", r.rest);
}

test "Alt picks first on success" {
    const P = Alt(.{ Literal("ab"), Literal("cd") });
    const r = P.parse("abXX").?;
    try std.testing.expectEqualStrings("XX", r.rest);
}

test "Alt falls back to second" {
    const P = Alt(.{ Literal("ab"), Literal("cd") });
    const r = P.parse("cdXX").?;
    try std.testing.expectEqualStrings("XX", r.rest);
}

test "Alt fails if all fail" {
    const P = Alt(.{ Literal("ab"), Literal("cd") });
    try std.testing.expect(P.parse("efgh") == null);
}

test "Alt with three branches" {
    const P = Alt(.{ Literal("ab"), Literal("cd"), Literal("ef") });
    try std.testing.expect(P.parse("abX") != null);
    try std.testing.expect(P.parse("cdX") != null);
    try std.testing.expect(P.parse("efX") != null);
    try std.testing.expect(P.parse("ghX") == null);
}

test "Many zero-or-more collects matches" {
    const P = Many(CharRange('0', '9'), .{});
    const r = P.parse("123abc").?;
    try std.testing.expectEqualStrings("123", r.value);
    try std.testing.expectEqualStrings("abc", r.rest);
}

test "Many zero-or-more succeeds on zero matches" {
    const P = Many(CharRange('0', '9'), .{});
    const r = P.parse("abc").?;
    try std.testing.expectEqual(0, r.value.len);
    try std.testing.expectEqualStrings("abc", r.rest);
}

test "Many one-or-more rejects zero matches" {
    const P = Many(CharRange('0', '9'), .{ .min = 1 });
    try std.testing.expect(P.parse("abc") == null);
}

test "Many bounded respects max" {
    const P = Many(CharRange('0', '9'), .{ .max = 2 });
    const r = P.parse("12345").?;
    try std.testing.expectEqualStrings("12", r.value);
    try std.testing.expectEqualStrings("345", r.rest);
}

test "Optional captures value" {
    const P = Optional(Literal("ab"));
    const r = P.parse("abcd").?;
    try std.testing.expectEqual({}, r.value.?);
    try std.testing.expectEqualStrings("cd", r.rest);
}

test "Optional returns null on no match" {
    const P = Optional(Literal("ab"));
    const r = P.parse("xxxx").?;
    try std.testing.expectEqual(null, r.value);
    try std.testing.expectEqualStrings("xxxx", r.rest);
}

test "Not succeeds when inner fails, consuming nothing" {
    const P = Not(Literal("let"));
    const r = P.parse("foo").?;
    try std.testing.expectEqualStrings("foo", r.rest);
}

test "Not fails when inner succeeds" {
    const P = Not(Literal("let"));
    try std.testing.expect(P.parse("let x") == null);
}

test "Not succeeds on empty input when inner needs input" {
    const P = Not(Literal("x"));
    const r = P.parse("").?;
    try std.testing.expectEqualStrings("", r.rest);
}

test "Not enables keyword vs identifier disambiguation" {
    // `Word("let")` — match "let" only if not followed by an identifier char.
    const Alpha = Char(std.ascii.isAlphabetic);
    const Word = Seq(.{ Literal("let"), Not(Alpha) });
    try std.testing.expect(Word.parse("let x") != null);
    try std.testing.expect(Word.parse("letter") == null);
}

test "Map transforms value" {
    const P = Map(CharRange('0', '9'), struct {
        fn f(c: u8) u8 {
            return c - '0';
        }
    }.f);
    const r = P.parse("7x").?;
    try std.testing.expectEqual(7, r.value);
    try std.testing.expectEqualStrings("x", r.rest);
}

test "composed: simple integer parser" {
    // Parse one or more digits, e.g. "42"
    const Digit = CharRange('0', '9');
    const Digits = Many(Digit, .{ .min = 1 });
    const r = Digits.parse("42abc").?;
    try std.testing.expectEqualStrings("42", r.value);
    try std.testing.expectEqualStrings("abc", r.rest);
}

test "composed: keyword then identifier" {
    const Let = Literal("let");
    const Space = Literal(" ");
    const Alpha = Char(std.ascii.isAlphabetic);
    const AlphaNum = Char(std.ascii.isAlphanumeric);
    const Ident = Seq(.{ Alpha, Many(AlphaNum, .{}) });
    const P = Seq(.{ Let, Space, Ident });

    const r = P.parse("let foo123 = 1").?;
    try std.testing.expectEqualStrings(" = 1", r.rest);
}

test "ByteLiteral matches exact byte" {
    const P = ByteLiteral(0x41);
    const r = P.parse("Abc").?;
    try std.testing.expectEqual('A', r.value);
    try std.testing.expectEqualStrings("bc", r.rest);
}

test "ByteLiteral rejects wrong byte" {
    const P = ByteLiteral(0x41);
    try std.testing.expect(P.parse("abc") == null);
}

test "CaseInsensitiveLiteral matches any case" {
    const P = CaseInsensitiveLiteral("hello");
    try std.testing.expect(P.parse("hello") != null);
    try std.testing.expect(P.parse("HELLO") != null);
    try std.testing.expect(P.parse("HeLLo") != null);
}

test "CaseInsensitiveLiteral rejects mismatch" {
    const P = CaseInsensitiveLiteral("hello");
    try std.testing.expect(P.parse("hxllo") == null);
}

test "Capture returns matched span" {
    const P = Capture(Seq(.{ Literal("ab"), Literal("cd") }));
    const r = P.parse("abcdef").?;
    try std.testing.expectEqualStrings("abcd", r.value);
    try std.testing.expectEqualStrings("ef", r.rest);
}

test "Capture with Many returns full span" {
    const P = Capture(Many(CharRange('0', '9'), .{ .min = 1 }));
    const r = P.parse("123abc").?;
    try std.testing.expectEqualStrings("123", r.value);
    try std.testing.expectEqualStrings("abc", r.rest);
}

test "Capture enables heterogeneous Alt" {
    // Without Capture, Alt(.{Literal, CharRange}) would fail because
    // void != u8. Capture makes both produce []const u8.
    const P = Alt(.{
        Capture(Literal("ab")),
        Capture(CharRange('0', '9')),
    });
    const r1 = P.parse("abXX").?;
    try std.testing.expectEqualStrings("ab", r1.value);
    const r2 = P.parse("5XX").?;
    try std.testing.expectEqualStrings("5", r2.value);
}
