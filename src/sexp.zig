/// RFC 9804 S-expression parser.
///
/// Parses all three transport representations:
/// - Canonical (verbatim-only, no whitespace)
/// - Basic transport (canonical + `{base64}` wrapping)
/// - Advanced transport (human-readable: tokens, quoted strings, hex, base64, display hints)
const std = @import("std");
pub const Encoder = @import("sexp/Encoder.zig");
pub const Scanner = @import("sexp/Scanner.zig");
pub const Token = @import("sexp/Token.zig");

/// A parsed S-expression value (RFC 9804 §3).
///
/// An S-expression is either an octet-string or a list of S-expressions.
pub const Value = union(enum) {
    /// An octet-string, possibly with a display hint.
    string: String,
    /// A parenthesized list of values.
    list: []const Value,
};

/// An octet-string with optional display hint (RFC 9804 §4).
pub const String = struct {
    /// The decoded octet data.
    data: []const u8,
    /// Optional MIME-type display hint, e.g. "image/gif".
    display: ?[]const u8 = null,
};

pub const Error = error{ InvalidSexp, OutOfMemory };

/// Parse an S-expression in advanced transport representation.
/// Returns the parsed value and the number of source bytes consumed.
pub fn parse(allocator: std.mem.Allocator, source: []const u8) Error!struct { value: Value, rest: []const u8 } {
    var parser = Parser.init(allocator, source);
    const value = try parser.parseValue();
    // Skip trailing whitespace.
    parser.scanner.skipWhitespace();
    return .{ .value = value, .rest = source[parser.scanner.current..] };
}

const Parser = struct {
    allocator: std.mem.Allocator,
    scanner: Scanner,
    source: []const u8,

    fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
        return .{
            .allocator = allocator,
            .scanner = Scanner.init(source),
            .source = source,
        };
    }

    fn parseValue(self: *Parser) Error!Value {
        const tok = self.scanner.next();
        return switch (tok.tag) {
            .lparen => self.parseList(),
            .lbracket => self.parseDisplayHint(),
            .lbrace => self.parseWrappedSexp(tok),
            .verbatim => .{ .string = .{ .data = decodeVerbatim(tok.lexeme(self.source)) } },
            .quoted_string => .{ .string = .{ .data = try self.decodeQuoted(tok.lexeme(self.source)) } },
            .sexp_token => .{ .string = .{ .data = tok.lexeme(self.source) } },
            .hexadecimal => .{ .string = .{ .data = try self.decodeHex(tok.lexeme(self.source)) } },
            .base64 => .{ .string = .{ .data = try self.decodeBase64(tok.lexeme(self.source)) } },
            .decimal => {
                // Bare decimal with no following encoding — treat as error.
                return error.InvalidSexp;
            },
            else => error.InvalidSexp,
        };
    }

    fn parseList(self: *Parser) Error!Value {
        var items: std.ArrayList(Value) = .empty;
        errdefer items.deinit(self.allocator);
        while (true) {
            // Peek to check for rparen or eof.
            const saved = self.scanner.current;
            const saved_line = self.scanner.line;
            const tok = self.scanner.next();
            if (tok.tag == .rparen) break;
            if (tok.tag == .eof) return error.InvalidSexp;
            // Rewind and parse value.
            self.scanner.current = saved;
            self.scanner.line = saved_line;
            try items.append(self.allocator, try self.parseValue());
        }
        return .{ .list = try items.toOwnedSlice(self.allocator) };
    }

    /// Parse `[hint] string` — display hint.
    fn parseDisplayHint(self: *Parser) Error!Value {
        // Parse the hint string.
        const hint_tok = self.scanner.next();
        const hint_data = switch (hint_tok.tag) {
            .verbatim => decodeVerbatim(hint_tok.lexeme(self.source)),
            .quoted_string => try self.decodeQuoted(hint_tok.lexeme(self.source)),
            .sexp_token => hint_tok.lexeme(self.source),
            .hexadecimal => try self.decodeHex(hint_tok.lexeme(self.source)),
            .base64 => try self.decodeBase64(hint_tok.lexeme(self.source)),
            else => return error.InvalidSexp,
        };
        // Expect closing bracket.
        const close = self.scanner.next();
        if (close.tag != .rbracket) return error.InvalidSexp;
        // Parse the actual string.
        const str_tok = self.scanner.next();
        const str_data = switch (str_tok.tag) {
            .verbatim => decodeVerbatim(str_tok.lexeme(self.source)),
            .quoted_string => try self.decodeQuoted(str_tok.lexeme(self.source)),
            .sexp_token => str_tok.lexeme(self.source),
            .hexadecimal => try self.decodeHex(str_tok.lexeme(self.source)),
            .base64 => try self.decodeBase64(str_tok.lexeme(self.source)),
            else => return error.InvalidSexp,
        };
        return .{ .string = .{ .data = str_data, .display = hint_data } };
    }

    /// Parse a `{base64}` wrapped canonical S-expression.
    fn parseWrappedSexp(self: *Parser, tok: Token.Token) Error!Value {
        const lex = tok.lexeme(self.source);
        // Strip { and }, decode base64 content.
        const inner = stripOuter(lex);
        const decoded = try decodeBase64Content(self.allocator, inner);
        // Recursively parse the canonical S-expression.
        const result = parse(self.allocator, decoded) catch return error.InvalidSexp;
        return result.value;
    }

    // ── Decoders ───────────────────────────────────────────────────

    /// Decode verbatim: `decimal:octets` → octets.
    fn decodeVerbatim(lex: []const u8) []const u8 {
        const colon = std.mem.indexOfScalar(u8, lex, ':') orelse return lex;
        return lex[colon + 1 ..];
    }

    /// Decode quoted string: optional length prefix + `"..escaped.."` → raw bytes.
    fn decodeQuoted(self: *Parser, lex: []const u8) Error![]const u8 {
        // Find the opening double-quote.
        const dq = std.mem.indexOfScalar(u8, lex, '"') orelse return error.InvalidSexp;
        const inner = lex[dq + 1 .. lex.len - 1]; // strip quotes
        return self.decodeEscapes(inner);
    }

    /// Decode hex: optional length prefix + `#hex-with-ws#` → raw bytes.
    fn decodeHex(self: *Parser, lex: []const u8) Error![]const u8 {
        // Find first '#'.
        const open = std.mem.indexOfScalar(u8, lex, '#') orelse return error.InvalidSexp;
        const inner = lex[open + 1 .. lex.len - 1]; // strip '#' delimiters
        // Collect hex digits, ignoring whitespace.
        var buf: std.ArrayList(u8) = .empty;
        errdefer buf.deinit(self.allocator);
        var i: usize = 0;
        while (i < inner.len) : (i += 1) {
            const c = inner[i];
            if (Scanner.isWhitespaceChar(c)) continue;
            if (!Scanner.isHexDigit(c)) return error.InvalidSexp;
            if (i + 1 >= inner.len) return error.InvalidSexp;
            // Find next hex digit (skip whitespace).
            var j = i + 1;
            while (j < inner.len and Scanner.isWhitespaceChar(inner[j])) j += 1;
            if (j >= inner.len or !Scanner.isHexDigit(inner[j])) return error.InvalidSexp;
            const hi = hexVal(c);
            const lo = hexVal(inner[j]);
            try buf.append(self.allocator, (hi << 4) | lo);
            i = j; // loop will increment
        }
        return try buf.toOwnedSlice(self.allocator);
    }

    /// Decode base64: optional length prefix + `|base64-with-ws|` → raw bytes.
    fn decodeBase64(self: *Parser, lex: []const u8) Error![]const u8 {
        const open = std.mem.indexOfScalar(u8, lex, '|') orelse return error.InvalidSexp;
        const inner = lex[open + 1 .. lex.len - 1]; // strip '|' delimiters
        return decodeBase64Content(self.allocator, inner);
    }

    fn decodeBase64Content(allocator: std.mem.Allocator, inner: []const u8) Error![]const u8 {
        // Strip whitespace from base64 content.
        var clean: std.ArrayList(u8) = .empty;
        defer clean.deinit(allocator);
        for (inner) |c| {
            if (!Scanner.isWhitespaceChar(c)) try clean.append(allocator, c);
        }
        // Decode.
        const decoder = std.base64.standard.decoderWithIgnore("");
        const decoded_len = decoder.calcSizeUpperBound(clean.items.len);
        const out = try allocator.alloc(u8, decoded_len);
        const actual_len = decoder.decode(out, clean.items) catch return error.InvalidSexp;
        if (actual_len < out.len) {
            // Shrink. Since we can't realloc easily, just return a slice.
            return out[0..actual_len];
        }
        return out;
    }

    /// Decode C-style escape sequences in a quoted string body.
    fn decodeEscapes(self: *Parser, raw: []const u8) Error![]const u8 {
        // Fast path: no backslashes.
        if (std.mem.indexOfScalar(u8, raw, '\\') == null) return raw;

        var buf: std.ArrayList(u8) = .empty;
        errdefer buf.deinit(self.allocator);
        const a = self.allocator;
        var i: usize = 0;
        while (i < raw.len) {
            if (raw[i] == '\\' and i + 1 < raw.len) {
                i += 1;
                const c = raw[i];
                switch (c) {
                    'a' => {
                        try buf.append(a, 0x07);
                        i += 1;
                    },
                    'b' => {
                        try buf.append(a, 0x08);
                        i += 1;
                    },
                    't' => {
                        try buf.append(a, '\t');
                        i += 1;
                    },
                    'v' => {
                        try buf.append(a, 0x0B);
                        i += 1;
                    },
                    'n' => {
                        try buf.append(a, '\n');
                        i += 1;
                    },
                    'f' => {
                        try buf.append(a, 0x0C);
                        i += 1;
                    },
                    'r' => {
                        try buf.append(a, '\r');
                        i += 1;
                    },
                    '"' => {
                        try buf.append(a, '"');
                        i += 1;
                    },
                    '\'' => {
                        try buf.append(a, '\'');
                        i += 1;
                    },
                    '?' => {
                        try buf.append(a, '?');
                        i += 1;
                    },
                    '\\' => {
                        try buf.append(a, '\\');
                        i += 1;
                    },
                    'x' => {
                        // \xHH
                        if (i + 2 < raw.len and Scanner.isHexDigit(raw[i + 1]) and Scanner.isHexDigit(raw[i + 2])) {
                            const val = (hexVal(raw[i + 1]) << 4) | hexVal(raw[i + 2]);
                            try buf.append(a, val);
                            i += 3;
                        } else {
                            try buf.append(a, '\\');
                            try buf.append(a, 'x');
                            i += 1;
                        }
                    },
                    '0'...'7' => {
                        // \OOO (exactly 3 octal digits)
                        if (i + 2 < raw.len and isOctal(raw[i + 1]) and isOctal(raw[i + 2])) {
                            const val: u8 = @intCast((@as(u16, c - '0') * 64) + (@as(u16, raw[i + 1] - '0') * 8) + (raw[i + 2] - '0'));
                            try buf.append(a, val);
                            i += 3;
                        } else {
                            try buf.append(a, '\\');
                            try buf.append(a, c);
                            i += 1;
                        }
                    },
                    '\r' => {
                        // Line continuation.
                        i += 1;
                        if (i < raw.len and raw[i] == '\n') i += 1;
                    },
                    '\n' => {
                        // Line continuation.
                        i += 1;
                        if (i < raw.len and raw[i] == '\r') i += 1;
                    },
                    else => {
                        try buf.append(a, '\\');
                        try buf.append(a, c);
                        i += 1;
                    },
                }
            } else {
                try buf.append(a, raw[i]);
                i += 1;
            }
        }
        return try buf.toOwnedSlice(a);
    }
};

fn hexVal(c: u8) u8 {
    return switch (c) {
        '0'...'9' => c - '0',
        'a'...'f' => c - 'a' + 10,
        'A'...'F' => c - 'A' + 10,
        else => 0,
    };
}

fn isOctal(c: u8) bool {
    return c >= '0' and c <= '7';
}

fn stripOuter(lex: []const u8) []const u8 {
    if (lex.len < 2) return lex;
    return lex[1 .. lex.len - 1];
}

// ── Tests ──────────────────────────────────────────────────────────────

test "parse token" {
    const r = try parse(std.testing.allocator, "subject");
    try std.testing.expectEqualStrings("subject", r.value.string.data);
    try std.testing.expectEqualStrings("", r.rest);
}

test "parse quoted string" {
    const r = try parse(std.testing.allocator, "\"hello\"");
    try std.testing.expectEqualStrings("hello", r.value.string.data);
}

test "parse quoted string with escapes" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"hello\\nworld\"");
    try std.testing.expectEqualStrings("hello\nworld", r.value.string.data);
}

test "parse verbatim" {
    const r = try parse(std.testing.allocator, "3:abc");
    try std.testing.expectEqualStrings("abc", r.value.string.data);
}

test "parse empty verbatim" {
    const r = try parse(std.testing.allocator, "0:");
    try std.testing.expectEqualStrings("", r.value.string.data);
}

test "parse hexadecimal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "#616263#");
    try std.testing.expectEqualStrings("abc", r.value.string.data);
}

test "parse empty hex" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "##");
    try std.testing.expectEqualStrings("", r.value.string.data);
}

test "parse base64" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "|YWJj|");
    try std.testing.expectEqualStrings("abc", r.value.string.data);
}

test "parse empty list" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "()");
    try std.testing.expectEqual(0, r.value.list.len);
}

test "parse list of tokens" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "(a b c)");
    try std.testing.expectEqual(3, r.value.list.len);
    try std.testing.expectEqualStrings("a", r.value.list[0].string.data);
    try std.testing.expectEqualStrings("b", r.value.list[1].string.data);
    try std.testing.expectEqualStrings("c", r.value.list[2].string.data);
}

test "parse nested list" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "(a (b c) d)");
    try std.testing.expectEqual(3, r.value.list.len);
    try std.testing.expectEqualStrings("a", r.value.list[0].string.data);
    try std.testing.expectEqual(2, r.value.list[1].list.len);
    try std.testing.expectEqualStrings("d", r.value.list[2].string.data);
}

test "parse canonical form" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "(6:issuer3:bob)");
    try std.testing.expectEqual(2, r.value.list.len);
    try std.testing.expectEqualStrings("issuer", r.value.list[0].string.data);
    try std.testing.expectEqualStrings("bob", r.value.list[1].string.data);
}

test "parse display hint" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "[3:gif]9:xxxxxxxxx");
    try std.testing.expectEqualStrings("xxxxxxxxx", r.value.string.data);
    try std.testing.expectEqualStrings("gif", r.value.string.display.?);
}

test "parse mixed expression" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "(abc (de #6667#) \"ghi jkl\")");
    try std.testing.expectEqual(3, r.value.list.len);
    try std.testing.expectEqualStrings("abc", r.value.list[0].string.data);
    const inner = r.value.list[1].list;
    try std.testing.expectEqual(2, inner.len);
    try std.testing.expectEqualStrings("de", inner[0].string.data);
    try std.testing.expectEqualStrings("fg", inner[1].string.data);
    try std.testing.expectEqualStrings("ghi jkl", r.value.list[2].string.data);
}

test "parse rest" {
    const r = try parse(std.testing.allocator, "abc rest");
    try std.testing.expectEqualStrings("abc", r.value.string.data);
    try std.testing.expectEqualStrings("rest", r.rest);
}

test "parse quoted with hex escape" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\x41\\x42\"");
    try std.testing.expectEqualStrings("AB", r.value.string.data);
}

test "parse quoted with octal escape" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\101\\102\"");
    try std.testing.expectEqualStrings("AB", r.value.string.data);
}

test "parse base64 wrapped sexp" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    // {KDE6YTE6YjE6Yyk=} decodes to (1:a1:b1:c)
    const r = try parse(arena.allocator(), "{KDE6YTE6YjE6Yyk=}");
    try std.testing.expectEqual(3, r.value.list.len);
    try std.testing.expectEqualStrings("a", r.value.list[0].string.data);
    try std.testing.expectEqualStrings("b", r.value.list[1].string.data);
    try std.testing.expectEqualStrings("c", r.value.list[2].string.data);
}

test "parse length-prefixed quoted string" {
    const r = try parse(std.testing.allocator, "7\"subject\"");
    try std.testing.expectEqualStrings("subject", r.value.string.data);
}

test "RFC 9804 example: SPKI cert body" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(),
        \\(certificate
        \\ (issuer "Alice")
        \\ (subject "Bob"))
    );
    try std.testing.expectEqual(3, r.value.list.len);
    try std.testing.expectEqualStrings("certificate", r.value.list[0].string.data);
    const issuer = r.value.list[1].list;
    try std.testing.expectEqual(2, issuer.len);
    try std.testing.expectEqualStrings("issuer", issuer[0].string.data);
    try std.testing.expectEqualStrings("Alice", issuer[1].string.data);
}

// ── RFC 9804 edge case tests ───────────────────────────────────────

// §4.1 Verbatim — binary data with special characters
test "verbatim with colons and quotes" {
    // 4:::": — four octets: : : " :
    const r = try parse(std.testing.allocator, "4:::\":");
    try std.testing.expectEqualStrings("::\":", r.value.string.data);
}

test "verbatim 12-byte string" {
    const r = try parse(std.testing.allocator, "12:hello world!");
    try std.testing.expectEqualStrings("hello world!", r.value.string.data);
}

test "verbatim with embedded newline" {
    const r = try parse(std.testing.allocator, "5:ab\ncd");
    try std.testing.expectEqualStrings("ab\ncd", r.value.string.data);
}

test "verbatim with null byte" {
    const r = try parse(std.testing.allocator, "3:a\x00b");
    try std.testing.expectEqual(3, r.value.string.data.len);
    try std.testing.expectEqual('a', r.value.string.data[0]);
    try std.testing.expectEqual(0, r.value.string.data[1]);
    try std.testing.expectEqual('b', r.value.string.data[2]);
}

// §4.2 Token — pseudo-alphabetic start characters
test "token colon-equals-dots" {
    const r = try parse(std.testing.allocator, ":=..");
    try std.testing.expectEqualStrings(":=..", r.value.string.data);
}

test "token with slashes (URL-like)" {
    const r = try parse(std.testing.allocator, "//example.net/names/smith");
    try std.testing.expectEqualStrings("//example.net/names/smith", r.value.string.data);
}

test "token single star" {
    const r = try parse(std.testing.allocator, "*");
    try std.testing.expectEqualStrings("*", r.value.string.data);
}

test "token class-of-1997" {
    const r = try parse(std.testing.allocator, "class-of-1997");
    try std.testing.expectEqualStrings("class-of-1997", r.value.string.data);
}

// §4.3 Quoted string — all escape sequences per RFC 9804
test "escape: alert" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\a\"");
    try std.testing.expectEqual(0x07, r.value.string.data[0]);
}

test "escape: backspace" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\b\"");
    try std.testing.expectEqual(0x08, r.value.string.data[0]);
}

test "escape: tab" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\t\"");
    try std.testing.expectEqual('\t', r.value.string.data[0]);
}

test "escape: vertical tab" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\v\"");
    try std.testing.expectEqual(0x0B, r.value.string.data[0]);
}

test "escape: form feed" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\f\"");
    try std.testing.expectEqual(0x0C, r.value.string.data[0]);
}

test "escape: carriage return" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\r\"");
    try std.testing.expectEqual('\r', r.value.string.data[0]);
}

test "escape: single quote" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\'\"");
    try std.testing.expectEqual('\'', r.value.string.data[0]);
}

test "escape: double quote" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\\"\"");
    try std.testing.expectEqual('"', r.value.string.data[0]);
}

test "escape: question mark" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\?\"");
    try std.testing.expectEqual('?', r.value.string.data[0]);
}

test "escape: backslash" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\\\\"");
    try std.testing.expectEqual('\\', r.value.string.data[0]);
}

test "escape: octal 376 = 0xFE" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\376\"");
    try std.testing.expectEqual(0xFE, r.value.string.data[0]);
}

test "escape: octal 000 = null" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\000\"");
    try std.testing.expectEqual(0x00, r.value.string.data[0]);
}

test "escape: hex xfe = 0xFE" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\xfe\"");
    try std.testing.expectEqual(0xFE, r.value.string.data[0]);
}

test "escape: hex xFE (uppercase)" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\xFE\"");
    try std.testing.expectEqual(0xFE, r.value.string.data[0]);
}

test "escape: three newlines via octal" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"\\012\\012\\012\"");
    try std.testing.expectEqual(3, r.value.string.data.len);
    try std.testing.expectEqual('\n', r.value.string.data[0]);
    try std.testing.expectEqual('\n', r.value.string.data[1]);
    try std.testing.expectEqual('\n', r.value.string.data[2]);
}

// §4.3 Line continuation — backslash followed by newline is ignored
test "escape: LF line continuation" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"hello\\\nworld\"");
    try std.testing.expectEqualStrings("helloworld", r.value.string.data);
}

test "escape: CRLF line continuation" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"hello\\\r\nworld\"");
    try std.testing.expectEqualStrings("helloworld", r.value.string.data);
}

test "escape: CR line continuation" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"hello\\\rworld\"");
    try std.testing.expectEqualStrings("helloworld", r.value.string.data);
}

test "escape: LFCR line continuation" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "\"hello\\\n\rworld\"");
    try std.testing.expectEqualStrings("helloworld", r.value.string.data);
}

// §4.3 Quoted string — empty
test "quoted string empty" {
    const r = try parse(std.testing.allocator, "\"\"");
    try std.testing.expectEqualStrings("", r.value.string.data);
}

// §4.3 Quoted string — no escapes (fast path)
test "quoted string no escapes" {
    const r = try parse(std.testing.allocator, "\"hi there\"");
    try std.testing.expectEqualStrings("hi there", r.value.string.data);
}

// §4.4 Hexadecimal — whitespace inside hex digits
test "hex with spaces between pairs" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "# 61 62 63 #");
    try std.testing.expectEqualStrings("abc", r.value.string.data);
}

test "hex with newlines" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "# 616\n263 #");
    try std.testing.expectEqualStrings("abc", r.value.string.data);
}

test "hex single null byte" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "#00#");
    try std.testing.expectEqual(1, r.value.string.data.len);
    try std.testing.expectEqual(0x00, r.value.string.data[0]);
}

// §4.5 Base-64 — with whitespace
test "base64 with spaces" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "| Y W J j |");
    try std.testing.expectEqualStrings("abc", r.value.string.data);
}

// §4.5 Base-64 — empty
test "base64 empty" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "||");
    try std.testing.expectEqualStrings("", r.value.string.data);
}

// §4.5 Base-64 — with padding
test "base64 with double padding" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "|YWJjZA==|");
    try std.testing.expectEqualStrings("abcd", r.value.string.data);
}

test "base64 with single padding" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "|YWJjZGU=|");
    try std.testing.expectEqualStrings("abcde", r.value.string.data);
}

// §3 Display hints — various encoding types for hint
test "display hint with quoted hint" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "[\"image/gif\"]\"data\"");
    try std.testing.expectEqualStrings("data", r.value.string.data);
    try std.testing.expectEqualStrings("image/gif", r.value.string.display.?);
}

test "display hint with token hint" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "[image/gif]\"data\"");
    try std.testing.expectEqualStrings("data", r.value.string.data);
    try std.testing.expectEqualStrings("image/gif", r.value.string.display.?);
}

test "display hint with whitespace around" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "[ image/gif ]\"data\"");
    try std.testing.expectEqualStrings("data", r.value.string.data);
    try std.testing.expectEqualStrings("image/gif", r.value.string.display.?);
}

// §5 Canonical form — display hint in canonical
test "canonical display hint" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "(4:icon[12:image/bitmap]9:xxxxxxxxx)");
    try std.testing.expectEqual(2, r.value.list.len);
    try std.testing.expectEqualStrings("icon", r.value.list[0].string.data);
    try std.testing.expectEqualStrings("xxxxxxxxx", r.value.list[1].string.data);
    try std.testing.expectEqualStrings("image/bitmap", r.value.list[1].string.display.?);
}

// §5 Canonical form — nested
test "canonical nested" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "(7:subject(3:ref5:alice6:mother))");
    try std.testing.expectEqual(2, r.value.list.len);
    try std.testing.expectEqualStrings("subject", r.value.list[0].string.data);
    const inner = r.value.list[1].list;
    try std.testing.expectEqual(3, inner.len);
    try std.testing.expectEqualStrings("ref", inner[0].string.data);
    try std.testing.expectEqualStrings("alice", inner[1].string.data);
    try std.testing.expectEqualStrings("mother", inner[2].string.data);
}

// §5 Canonical — bare verbatim (not in list)
test "canonical bare verbatim" {
    const r = try parse(std.testing.allocator, "10:abcdefghij");
    try std.testing.expectEqualStrings("abcdefghij", r.value.string.data);
}

// Deep nesting
test "deeply nested lists" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "(a (b (c (d))))");
    try std.testing.expectEqual(2, r.value.list.len);
    const l1 = r.value.list[1].list;
    try std.testing.expectEqual(2, l1.len);
    const l2 = l1[1].list;
    try std.testing.expectEqual(2, l2.len);
    const l3 = l2[1].list;
    try std.testing.expectEqual(1, l3.len);
    try std.testing.expectEqualStrings("d", l3[0].string.data);
}

// Multiple encoding equivalence: all represent "abc"
test "encoding equivalence: all forms of abc" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const r1 = try parse(a, "abc");
    const r2 = try parse(a, "3:abc");
    const r3 = try parse(a, "\"abc\"");
    const r4 = try parse(a, "#616263#");
    const r5 = try parse(a, "|YWJj|");

    try std.testing.expectEqualStrings("abc", r1.value.string.data);
    try std.testing.expectEqualStrings("abc", r2.value.string.data);
    try std.testing.expectEqualStrings("abc", r3.value.string.data);
    try std.testing.expectEqualStrings("abc", r4.value.string.data);
    try std.testing.expectEqualStrings("abc", r5.value.string.data);
}

// RFC example: (a bob c) with spacing variants
test "list with extra whitespace" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "( a ( bob c ) ( ( d e ) ( e f ) )  )");
    try std.testing.expectEqual(3, r.value.list.len);
    try std.testing.expectEqualStrings("a", r.value.list[0].string.data);
}

// RFC example: snicker list
test "RFC example: snicker" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const r = try parse(arena.allocator(), "(snicker \"abc\" (#03# |YWJj|))");
    try std.testing.expectEqual(3, r.value.list.len);
    try std.testing.expectEqualStrings("snicker", r.value.list[0].string.data);
    try std.testing.expectEqualStrings("abc", r.value.list[1].string.data);
    const inner = r.value.list[2].list;
    try std.testing.expectEqual(2, inner.len);
    try std.testing.expectEqual(0x03, inner[0].string.data[0]);
    try std.testing.expectEqualStrings("abc", inner[1].string.data);
}

// Error cases
test "error: unterminated list" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    try std.testing.expectError(error.InvalidSexp, parse(arena.allocator(), "(a b"));
}

test "error: unexpected rparen" {
    try std.testing.expectError(error.InvalidSexp, parse(std.testing.allocator, ")"));
}

test "error: empty input" {
    try std.testing.expectError(error.InvalidSexp, parse(std.testing.allocator, ""));
}

test "error: bare rbracket" {
    try std.testing.expectError(error.InvalidSexp, parse(std.testing.allocator, "]"));
}
