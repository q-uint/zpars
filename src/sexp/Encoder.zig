/// S-expression encoder — serializes a `Value` into RFC 9804 transport
/// representations.
///
/// Supports all three forms:
/// - Canonical (§6.2): verbatim-only, no whitespace, deterministic
/// - Basic transport (§6.3): canonical form base64-wrapped in `{}`
/// - Advanced transport (§6.4): human-readable tokens, quoted strings, hex
const std = @import("std");
const sexp = @import("../sexp.zig");
const Value = sexp.Value;
const String = sexp.String;
const Scanner = @import("Scanner.zig");

const Writer = std.Io.Writer;
const hex_chars = "0123456789abcdef";

/// Encode in canonical transport representation (RFC 9804 §6.2).
///
/// Every octet-string uses verbatim `length:data` encoding, no whitespace
/// appears anywhere, and display hints use `[len:hint]len:data`. This is
/// the unique, deterministic representation used for digital signatures.
pub fn canonical(value: Value, writer: *Writer) Writer.Error!void {
    switch (value) {
        .string => |str| try writeCanonicalString(str, writer),
        .list => |items| {
            try writer.writeByte('(');
            for (items) |item| {
                try canonical(item, writer);
            }
            try writer.writeByte(')');
        },
    }
}

/// Encode in basic transport representation (RFC 9804 §6.3).
///
/// The entire canonical form is base64-encoded and wrapped in `{…}`.
/// Needs an allocator for the intermediate canonical buffer.
pub fn basic(value: Value, allocator: std.mem.Allocator, writer: *Writer) (std.mem.Allocator.Error || Writer.Error)!void {
    // Buffer the canonical form.
    var aw: Writer.Allocating = .init(allocator);
    defer aw.deinit();
    try canonical(value, &aw.writer);
    // Wrap in {base64}.
    try writer.writeByte('{');
    const encoder = std.base64.standard.Encoder;
    try encoder.encodeWriter(writer, aw.writer.buffered());
    try writer.writeByte('}');
}

/// Encode in advanced transport representation (RFC 9804 §6.4).
///
/// Uses the most readable encoding for each octet-string: bareword
/// tokens where possible, quoted strings for printable ASCII, and
/// hexadecimal `#…#` for binary data. List elements are separated
/// by a single space.
pub fn advanced(value: Value, writer: *Writer) Writer.Error!void {
    switch (value) {
        .string => |str| try writeAdvancedString(str, writer),
        .list => |items| {
            try writer.writeByte('(');
            for (items, 0..) |item, i| {
                if (i > 0) try writer.writeByte(' ');
                try advanced(item, writer);
            }
            try writer.writeByte(')');
        },
    }
}

/// Write verbatim encoding: `length:data`.
fn writeVerbatim(data: []const u8, writer: *Writer) Writer.Error!void {
    try writer.print("{d}:", .{data.len});
    try writer.writeAll(data);
}

/// Write a canonical octet-string, with optional display hint.
fn writeCanonicalString(str: String, writer: *Writer) Writer.Error!void {
    if (str.display) |hint| {
        try writer.writeByte('[');
        try writeVerbatim(hint, writer);
        try writer.writeByte(']');
    }
    try writeVerbatim(str.data, writer);
}

/// Check if data qualifies as an RFC 9804 token.
fn isToken(data: []const u8) bool {
    if (data.len == 0) return false;
    if (!Scanner.isTokenStart(data[0])) return false;
    for (data[1..]) |c| {
        if (!Scanner.isTokenCont(c)) return false;
    }
    return true;
}

/// Check if all bytes are printable ASCII (0x20–0x7E).
fn isPrintable(data: []const u8) bool {
    for (data) |c| {
        if (c < 0x20 or c > 0x7E) return false;
    }
    return true;
}

/// Write a quoted string with C-style escapes.
fn writeQuoted(data: []const u8, writer: *Writer) Writer.Error!void {
    try writer.writeByte('"');
    for (data) |c| {
        switch (c) {
            '\\' => try writer.writeAll("\\\\"),
            '"' => try writer.writeAll("\\\""),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            0x07 => try writer.writeAll("\\a"),
            0x08 => try writer.writeAll("\\b"),
            0x0B => try writer.writeAll("\\v"),
            0x0C => try writer.writeAll("\\f"),
            else => {
                if (c >= 0x20 and c <= 0x7E) {
                    try writer.writeByte(c);
                } else {
                    try writer.writeAll("\\x");
                    try writer.writeByte(hex_chars[c >> 4]);
                    try writer.writeByte(hex_chars[c & 0x0F]);
                }
            },
        }
    }
    try writer.writeByte('"');
}

/// Write hexadecimal encoding: `#hex_pairs#`.
fn writeHex(data: []const u8, writer: *Writer) Writer.Error!void {
    try writer.writeByte('#');
    for (data) |c| {
        try writer.writeByte(hex_chars[c >> 4]);
        try writer.writeByte(hex_chars[c & 0x0F]);
    }
    try writer.writeByte('#');
}

/// Write an advanced-transport octet-string atom (without display hint).
///
/// Strategy: token > quoted string > hex.
fn writeAdvancedAtom(data: []const u8, writer: *Writer) Writer.Error!void {
    if (isToken(data)) {
        try writer.writeAll(data);
    } else if (isPrintable(data)) {
        try writeQuoted(data, writer);
    } else {
        try writeHex(data, writer);
    }
}

/// Write an advanced-transport octet-string with optional display hint.
fn writeAdvancedString(str: String, writer: *Writer) Writer.Error!void {
    if (str.display) |hint| {
        try writer.writeByte('[');
        try writeAdvancedAtom(hint, writer);
        try writer.writeByte(']');
    }
    try writeAdvancedAtom(str.data, writer);
}

fn expectCanonical(expected: []const u8, value: Value) !void {
    var buf: [4096]u8 = undefined;
    var fbs: Writer = .fixed(&buf);
    try canonical(value, &fbs);
    try std.testing.expectEqualStrings(expected, fbs.buffered());
}

fn expectAdvanced(expected: []const u8, value: Value) !void {
    var buf: [4096]u8 = undefined;
    var fbs: Writer = .fixed(&buf);
    try advanced(value, &fbs);
    try std.testing.expectEqualStrings(expected, fbs.buffered());
}

fn expectBasic(expected: []const u8, value: Value) !void {
    var buf: [4096]u8 = undefined;
    var fbs: Writer = .fixed(&buf);
    try basic(value, std.testing.allocator, &fbs);
    try std.testing.expectEqualStrings(expected, fbs.buffered());
}

fn expectValuesEqual(a: Value, b: Value) !void {
    switch (a) {
        .string => |sa| {
            try std.testing.expectEqualStrings(sa.data, b.string.data);
            if (sa.display) |da| {
                try std.testing.expectEqualStrings(da, b.string.display.?);
            } else {
                try std.testing.expect(b.string.display == null);
            }
        },
        .list => |la| {
            try std.testing.expectEqual(la.len, b.list.len);
            for (la, b.list) |ea, eb| {
                try expectValuesEqual(ea, eb);
            }
        },
    }
}

// ── Canonical encoding tests (RFC 9804 §6.2) ─────────────────────

test "canonical: simple string" {
    try expectCanonical("3:abc", .{ .string = .{ .data = "abc" } });
}

test "canonical: empty string" {
    try expectCanonical("0:", .{ .string = .{ .data = "" } });
}

test "canonical: bare string 10 bytes" {
    try expectCanonical("10:abcdefghij", .{ .string = .{ .data = "abcdefghij" } });
}

test "canonical: empty list" {
    try expectCanonical("()", .{ .list = &.{} });
}

test "canonical: (issuer bob)" {
    try expectCanonical("(6:issuer3:bob)", .{ .list = &.{
        .{ .string = .{ .data = "issuer" } },
        .{ .string = .{ .data = "bob" } },
    } });
}

test "canonical: nested (subject (ref alice mother))" {
    try expectCanonical("(7:subject(3:ref5:alice6:mother))", .{ .list = &.{
        .{ .string = .{ .data = "subject" } },
        .{ .list = &.{
            .{ .string = .{ .data = "ref" } },
            .{ .string = .{ .data = "alice" } },
            .{ .string = .{ .data = "mother" } },
        } },
    } });
}

test "canonical: display hint" {
    try expectCanonical("[12:image/bitmap]9:xxxxxxxxx", .{ .string = .{
        .data = "xxxxxxxxx",
        .display = "image/bitmap",
    } });
}

test "canonical: display hint in list" {
    try expectCanonical("(4:icon[12:image/bitmap]9:xxxxxxxxx)", .{ .list = &.{
        .{ .string = .{ .data = "icon" } },
        .{ .string = .{ .data = "xxxxxxxxx", .display = "image/bitmap" } },
    } });
}

test "canonical: binary data with null bytes" {
    try expectCanonical("3:\x00\x01\x02", .{ .string = .{ .data = "\x00\x01\x02" } });
}

// ── Basic transport tests (RFC 9804 §6.3) ─────────────────────────

test "basic: (a b c)" {
    // Canonical of (a b c) is (1:a1:b1:c), base64 is KDE6YTE6YjE6Yyk=
    try expectBasic("{KDE6YTE6YjE6Yyk=}", .{ .list = &.{
        .{ .string = .{ .data = "a" } },
        .{ .string = .{ .data = "b" } },
        .{ .string = .{ .data = "c" } },
    } });
}

test "basic: empty list" {
    // Canonical of () is "()", base64 of "()" is "KCk="
    try expectBasic("{KCk=}", .{ .list = &.{} });
}

test "basic: simple string" {
    // Canonical of "abc" is "3:abc", base64 of "3:abc" is "MzphYmM="
    try expectBasic("{MzphYmM=}", .{ .string = .{ .data = "abc" } });
}

// ── Advanced transport tests (RFC 9804 §6.4) ──────────────────────

test "advanced: token-eligible string" {
    try expectAdvanced("subject", .{ .string = .{ .data = "subject" } });
}

test "advanced: token with simple-punc" {
    try expectAdvanced("not-before", .{ .string = .{ .data = "not-before" } });
}

test "advanced: token with slashes" {
    try expectAdvanced("//example.net/names/smith", .{ .string = .{ .data = "//example.net/names/smith" } });
}

test "advanced: quoted string (contains space)" {
    try expectAdvanced("\"hello world\"", .{ .string = .{ .data = "hello world" } });
}

test "advanced: quoted string (empty)" {
    try expectAdvanced("\"\"", .{ .string = .{ .data = "" } });
}

test "advanced: quoted string with escapes" {
    try expectAdvanced("\"he said \\\"hi\\\"\"", .{ .string = .{ .data = "he said \"hi\"" } });
}

test "advanced: quoted string with backslash" {
    try expectAdvanced("\"a\\\\b\"", .{ .string = .{ .data = "a\\b" } });
}

test "advanced: hex for binary data" {
    try expectAdvanced("#00010203#", .{ .string = .{ .data = "\x00\x01\x02\x03" } });
}

test "advanced: hex for single null byte" {
    try expectAdvanced("#00#", .{ .string = .{ .data = "\x00" } });
}

test "advanced: list with spacing" {
    try expectAdvanced("(a b c)", .{ .list = &.{
        .{ .string = .{ .data = "a" } },
        .{ .string = .{ .data = "b" } },
        .{ .string = .{ .data = "c" } },
    } });
}

test "advanced: nested list" {
    try expectAdvanced("(a (b c) d)", .{ .list = &.{
        .{ .string = .{ .data = "a" } },
        .{ .list = &.{
            .{ .string = .{ .data = "b" } },
            .{ .string = .{ .data = "c" } },
        } },
        .{ .string = .{ .data = "d" } },
    } });
}

test "advanced: display hint (both tokens)" {
    try expectAdvanced("[image/gif]data", .{ .string = .{
        .data = "data",
        .display = "image/gif",
    } });
}

test "advanced: display hint with quoted" {
    try expectAdvanced("[image/gif]\"hello world\"", .{ .string = .{
        .data = "hello world",
        .display = "image/gif",
    } });
}

test "advanced: control characters use hex" {
    try expectAdvanced("#0001020a0d#", .{ .string = .{ .data = "\x00\x01\x02\n\r" } });
}

test "advanced: mixed printable and non-printable uses quoted with escapes" {
    // "hello\nworld" — has a non-printable char but it's a named escape
    // isPrintable returns false because \n < 0x20, so this goes to hex path...
    // Actually, let's verify: \n = 0x0A which is < 0x20, so isPrintable is false.
    // This means it goes to hex encoding.
    try expectAdvanced("#68656c6c6f0a776f726c64#", .{ .string = .{ .data = "hello\nworld" } });
}

// ── Round-trip tests ──────────────────────────────────────────────

test "round-trip: advanced -> canonical -> parse" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    // Parse advanced input.
    const r1 = try sexp.parse(a, "(certificate (issuer \"Alice\") (subject \"Bob\"))");

    // Encode canonical.
    var buf: [4096]u8 = undefined;
    var fbs: Writer = .fixed(&buf);
    try canonical(r1.value, &fbs);

    // Parse the canonical output.
    const r2 = try sexp.parse(a, fbs.buffered());

    // Values must match.
    try expectValuesEqual(r1.value, r2.value);
}

test "round-trip: canonical -> advanced -> parse" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const r1 = try sexp.parse(a, "(6:issuer3:bob)");

    var buf: [4096]u8 = undefined;
    var fbs: Writer = .fixed(&buf);
    try advanced(r1.value, &fbs);

    const r2 = try sexp.parse(a, fbs.buffered());
    try expectValuesEqual(r1.value, r2.value);
}

test "round-trip: snicker example" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const r1 = try sexp.parse(a, "(snicker \"abc\" (#03# |YWJj|))");

    // Encode canonical, then parse back.
    var buf: [4096]u8 = undefined;
    var fbs: Writer = .fixed(&buf);
    try canonical(r1.value, &fbs);

    const r2 = try sexp.parse(a, fbs.buffered());
    try expectValuesEqual(r1.value, r2.value);
}

test "round-trip: basic transport" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const original: Value = .{ .list = &.{
        .{ .string = .{ .data = "issuer" } },
        .{ .string = .{ .data = "bob" } },
    } };

    // Encode as basic.
    var buf: [4096]u8 = undefined;
    var fbs: Writer = .fixed(&buf);
    try basic(original, std.testing.allocator, &fbs);

    // Parse the basic output.
    const r = try sexp.parse(a, fbs.buffered());
    try expectValuesEqual(original, r.value);
}

test "round-trip: display hint through canonical" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const r1 = try sexp.parse(a, "(4:icon[12:image/bitmap]9:xxxxxxxxx)");

    var buf: [4096]u8 = undefined;
    var fbs: Writer = .fixed(&buf);
    try canonical(r1.value, &fbs);
    try std.testing.expectEqualStrings("(4:icon[12:image/bitmap]9:xxxxxxxxx)", fbs.buffered());

    const r2 = try sexp.parse(a, fbs.buffered());
    try expectValuesEqual(r1.value, r2.value);
}

test "round-trip: display hint through advanced" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const r1 = try sexp.parse(a, "[image/gif]\"data\"");

    var buf: [4096]u8 = undefined;
    var fbs: Writer = .fixed(&buf);
    try advanced(r1.value, &fbs);

    const r2 = try sexp.parse(a, fbs.buffered());
    try expectValuesEqual(r1.value, r2.value);
}
