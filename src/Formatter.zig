/// ABNF formatter — pretty-prints an AST back into canonical ABNF text.
///
/// Rule names are padded to align the `=` / `=/` operators across all
/// rules in the grammar.
const std = @import("std");
const Ast = @import("Ast.zig");

/// Format a complete grammar (list of rules) to the writer.
pub fn formatGrammar(rules: []const Ast.Rule, writer: anytype) !void {
    var max_name: usize = 0;
    for (rules) |rule| {
        if (rule.name.len > max_name) max_name = rule.name.len;
    }

    for (rules) |rule| {
        try writer.writeAll(rule.name);
        for (0..max_name - rule.name.len + 1) |_| try writer.writeByte(' ');
        if (rule.incremental) {
            try writer.writeAll("=/ ");
        } else {
            try writer.writeAll("= ");
        }
        try formatNode(rule.node, writer);
        try writer.writeByte('\n');
    }
}

/// Format a single AST node.
pub fn formatNode(node: Ast.Node, writer: anytype) anyerror!void {
    switch (node) {
        .alternation => |alts| {
            for (alts, 0..) |alt, i| {
                if (i > 0) try writer.writeAll(" / ");
                try formatNode(alt, writer);
            }
        },
        .concatenation => |elems| {
            for (elems, 0..) |elem, i| {
                if (i > 0) try writer.writeByte(' ');
                switch (elem) {
                    .alternation => {
                        try writer.writeByte('(');
                        try formatNode(elem, writer);
                        try writer.writeByte(')');
                    },
                    else => try formatNode(elem, writer),
                }
            }
        },
        .repetition => |rep| try formatRepetition(rep, writer),
        .char_val => |cv| try formatCharVal(cv, writer),
        .num_val => |nv| try formatNumVal(nv, writer),
        .prose_val => |pv| {
            try writer.writeByte('<');
            try writer.writeAll(pv);
            try writer.writeByte('>');
        },
        .rulename => |name| try writer.writeAll(name),
    }
}

fn formatRepetition(rep: Ast.Repetition, writer: anytype) anyerror!void {
    // Option shorthand: [element]
    if (rep.min == 0 and rep.max != null and rep.max.? == 1) {
        try writer.writeByte('[');
        try formatNode(rep.element.*, writer);
        try writer.writeByte(']');
        return;
    }

    // Repeat prefix.
    if (rep.min == 0 and rep.max == null) {
        try writer.writeByte('*');
    } else if (rep.min == 0) {
        try writer.print("*{d}", .{rep.max.?});
    } else if (rep.max == null) {
        try writer.print("{d}*", .{rep.min});
    } else if (rep.min == rep.max.?) {
        try writer.print("{d}", .{rep.min});
    } else {
        try writer.print("{d}*{d}", .{ rep.min, rep.max.? });
    }

    // Element — needs grouping if compound.
    switch (rep.element.*) {
        .alternation, .concatenation => {
            try writer.writeByte('(');
            try formatNode(rep.element.*, writer);
            try writer.writeByte(')');
        },
        else => try formatNode(rep.element.*, writer),
    }
}

fn formatCharVal(cv: Ast.CharVal, writer: anytype) !void {
    if (cv.case_sensitive) try writer.writeAll("%s");
    try writer.writeByte('"');
    try writer.writeAll(cv.value);
    try writer.writeByte('"');
}

fn formatNumVal(nv: Ast.NumVal, writer: anytype) !void {
    switch (nv) {
        .single => |b| {
            try writer.writeAll("%x");
            try writeHex(writer, b);
        },
        .range => |r| {
            try writer.writeAll("%x");
            try writeHex(writer, r.lo);
            try writer.writeByte('-');
            try writeHex(writer, r.hi);
        },
        .concat => |bytes| {
            for (bytes, 0..) |b, i| {
                if (i == 0) try writer.writeAll("%x") else try writer.writeByte('.');
                try writeHex(writer, b);
            }
        },
    }
}

const hex_digits = "0123456789ABCDEF";

fn writeHex(writer: anytype, byte: u8) !void {
    try writer.writeByte(hex_digits[byte >> 4]);
    try writer.writeByte(hex_digits[byte & 0x0F]);
}

// --- Tests -------------------------------------------------------------------

const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");

fn expectFmt(expected: []const u8, input: []const u8) !void {
    var scanner = Scanner.init(input);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, input);
    const rules = try parser.parse();
    std.debug.assert(parser.getDiagnostics().len == 0);
    var buf: [4096]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try formatGrammar(rules, fbs.writer());
    try std.testing.expectEqualStrings(expected, fbs.getWritten());
}

test "simple rule" {
    try expectFmt("foo = bar\n", "foo = bar");
}

test "alternation" {
    try expectFmt("foo = a / b / c\n", "foo = a / b / c");
}

test "concatenation" {
    try expectFmt("foo = a b c\n", "foo = a b c");
}

test "repetition star" {
    try expectFmt("foo = *bar\n", "foo = *bar");
}

test "repetition bounded" {
    try expectFmt("foo = 3*5bar\n", "foo = 3*5bar");
}

test "repetition exact" {
    try expectFmt("foo = 3bar\n", "foo = 3bar");
}

test "repetition min only" {
    try expectFmt("foo = 3*bar\n", "foo = 3*bar");
}

test "repetition max only" {
    try expectFmt("foo = *5bar\n", "foo = *5bar");
}

test "option shorthand" {
    try expectFmt("foo = [bar]\n", "foo = [bar]");
}

test "char val" {
    try expectFmt("foo = \"hello\"\n",
        \\foo = "hello"
    );
}

test "case-sensitive char val" {
    try expectFmt("foo = %s\"Hello\"\n",
        \\foo = %s"Hello"
    );
}

test "hex val single" {
    try expectFmt("foo = %x41\n", "foo = %x41");
}

test "hex val range" {
    try expectFmt("foo = %x41-5A\n", "foo = %x41-5A");
}

test "hex val concat" {
    try expectFmt("foo = %x48.65.6C\n", "foo = %x48.65.6C");
}

test "prose val" {
    try expectFmt("foo = <some prose>\n", "foo = <some prose>");
}

test "incremental alternation" {
    try expectFmt("foo = a\nfoo =/ b\n", "foo = a\nfoo =/ b");
}

test "multi-rule alignment" {
    try expectFmt(
        \\number = 1*DIGIT
        \\pair   = number "," number
        \\
    ,
        \\number = 1*DIGIT
        \\pair = number "," number
    );
}

test "grouped alternation in concatenation" {
    try expectFmt("foo = (a / b) c\n", "foo = (a / b) c");
}

test "grouped repetition element" {
    try expectFmt("foo = *(a b)\n", "foo = *(a b)");
}
