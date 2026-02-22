/// BNF formatter — pretty-prints an AST back into canonical BNF text
/// (ALGOL 60 variant).
///
/// Rule names are padded to align the `::=` operators across all rules.
const std = @import("std");
const Ast = @import("../Ast.zig");

/// Format a complete grammar (list of rules) to the writer.
pub fn formatGrammar(rules: []const Ast.Rule, writer: anytype) !void {
    var max_name: usize = 0;
    for (rules) |rule| {
        // Account for angle brackets: <name>
        const display_len = rule.name.len + 2;
        if (display_len > max_name) max_name = display_len;
    }

    for (rules) |rule| {
        try writer.writeByte('<');
        try writer.writeAll(rule.name);
        try writer.writeByte('>');
        const display_len = rule.name.len + 2;
        for (0..max_name - display_len + 1) |_| try writer.writeByte(' ');
        try writer.writeAll("::= ");
        try formatNode(rule.node, writer);
        try writer.writeByte('\n');
    }
}

/// Format a single AST node as BNF.
pub fn formatNode(node: Ast.Node, writer: anytype) anyerror!void {
    switch (node) {
        .alternation => |alts| {
            for (alts, 0..) |alt, i| {
                if (i > 0) try writer.writeAll(" | ");
                try formatNode(alt, writer);
            }
        },
        .concatenation => |elems| {
            for (elems, 0..) |elem, i| {
                if (i > 0) try writer.writeByte(' ');
                try formatNode(elem, writer);
            }
        },
        .char_val => |cv| try writer.writeAll(cv.value),
        .rulename => |name| {
            try writer.writeByte('<');
            try writer.writeAll(name);
            try writer.writeByte('>');
        },
        // These node types don't appear in BNF-parsed ASTs, but handle
        // them gracefully in case an ABNF AST is formatted as BNF.
        .repetition, .num_val, .prose_val => {},
    }
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
    try expectFmt("<digit> ::= 0\n", "<digit> ::= 0");
}

test "alternation" {
    try expectFmt("<bit> ::= 0 | 1\n", "<bit> ::= 0 | 1");
}

test "concatenation" {
    try expectFmt("<pair> ::= <a> <b>\n", "<pair> ::= <a> <b>");
}

test "mixed terminals and non-terminals" {
    try expectFmt("<expr> ::= <term> + <term>\n", "<expr> ::= <term> + <term>");
}

test "empty rule" {
    try expectFmt("<empty> ::= \n", "<empty> ::=");
}

test "multi-rule alignment" {
    try expectFmt(
        \\<digit>  ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
        \\<number> ::= <digit> | <number> <digit>
        \\
    ,
        \\<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
        \\<number> ::= <digit> | <number> <digit>
    );
}

test "ALGOL 60 example round-trip" {
    try expectFmt(
        "<ab> ::= ( | [ | <ab> ( | <ab> <d>\n",
        "<ab> ::= ( | [ | <ab> ( | <ab> <d>",
    );
}
