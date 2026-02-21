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

// ---------------------------------------------------------------------------
// Core result type
// ---------------------------------------------------------------------------

pub fn Result(comptime T: type) type {
    return struct {
        value: T,
        rest: []const u8,
    };
}

// ---------------------------------------------------------------------------
// Primitive parsers
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Combinators
// ---------------------------------------------------------------------------

/// Run `A` then `B`; produce a tuple of both values.
pub fn Sequence(comptime A: type, comptime B: type) type {
    return struct {
        pub const Value = struct { A.Value, B.Value };

        pub fn parse(input: []const u8) ?Result(Value) {
            const ra = A.parse(input) orelse return null;
            const rb = B.parse(ra.rest) orelse return null;
            return .{ .value = .{ ra.value, rb.value }, .rest = rb.rest };
        }
    };
}

/// Try `A`; if it fails, try `B`. Both must produce the same Value type.
pub fn Choice(comptime A: type, comptime B: type) type {
    if (A.Value != B.Value) @compileError("Choice requires both parsers to have the same Value type");

    return struct {
        pub const Value = A.Value;

        pub fn parse(input: []const u8) ?Result(Value) {
            return A.parse(input) orelse B.parse(input);
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
/// Allocates into a comptime-sized buffer and returns a slice into the
/// input-adjacent stack frame, so this is safe for bounded repetitions.
/// For unbounded repetitions, caps at 4096 to avoid blowing the stack.
pub fn Many(comptime P: type, comptime bounds: Bounds) type {
    const limit = bounds.max orelse 4096;

    return struct {
        pub const Value = []const P.Value;

        pub fn parse(input: []const u8) ?Result(Value) {
            var buf: [limit]P.Value = undefined;
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

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

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

test "Sequence chains two parsers" {
    const P = Sequence(Literal("ab"), Literal("cd"));
    const r = P.parse("abcdef").?;
    try std.testing.expectEqualStrings("ef", r.rest);
}

test "Sequence fails if first fails" {
    const P = Sequence(Literal("ab"), Literal("cd"));
    try std.testing.expect(P.parse("xxcd") == null);
}

test "Sequence fails if second fails" {
    const P = Sequence(Literal("ab"), Literal("cd"));
    try std.testing.expect(P.parse("abxx") == null);
}

test "Choice picks first on success" {
    const P = Choice(Literal("ab"), Literal("cd"));
    const r = P.parse("abXX").?;
    try std.testing.expectEqualStrings("XX", r.rest);
}

test "Choice falls back to second" {
    const P = Choice(Literal("ab"), Literal("cd"));
    const r = P.parse("cdXX").?;
    try std.testing.expectEqualStrings("XX", r.rest);
}

test "Choice fails if both fail" {
    const P = Choice(Literal("ab"), Literal("cd"));
    try std.testing.expect(P.parse("efgh") == null);
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
    const Ident = Sequence(Alpha, Many(AlphaNum, .{}));
    const P = Sequence(Sequence(Let, Space), Ident);

    const r = P.parse("let foo123 = 1").?;
    try std.testing.expectEqualStrings(" = 1", r.rest);
}
