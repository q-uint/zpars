/// ABNF formatter — pretty-prints an AST back into canonical ABNF text.
///
/// Rule names are padded to align the `=` / `=/` operators across all
/// rules in the grammar. Comments are preserved from the token stream.
const std = @import("std");
const Ast = @import("../Ast.zig");
const Token = @import("Token.zig").Token;

/// Format a complete grammar to the writer, preserving comments from the
/// token stream while reformatting rule bodies from the parsed AST.
pub fn formatGrammar(rules: []const Ast.Rule, tokens: []const Token, source: []const u8, writer: anytype) !void {
    var max_name: usize = 0;
    for (rules) |rule| {
        if (rule.name.len > max_name) max_name = rule.name.len;
    }

    var rule_idx: usize = 0;
    var tok_idx: usize = 0;

    while (tok_idx < tokens.len) {
        const tok = tokens[tok_idx];
        switch (tok.tag) {
            .eof => break,
            .comment => {
                try writer.writeAll(tok.lexeme(source));
                try writer.writeByte('\n');
                tok_idx += 1;
                // Skip the newline after the comment (if any).
                if (tok_idx < tokens.len and tokens[tok_idx].tag == .newline)
                    tok_idx += 1;
            },
            .newline => {
                tok_idx += 1;
            },
            .rulename => {
                if (rule_idx < rules.len) {
                    const rule = rules[rule_idx];
                    rule_idx += 1;

                    try writer.writeAll(rule.name);
                    for (0..max_name - rule.name.len + 1) |_| try writer.writeByte(' ');
                    if (rule.incremental) {
                        try writer.writeAll("=/ ");
                    } else {
                        try writer.writeAll("= ");
                    }
                    try formatNode(rule.node, writer);

                    // Skip past the original rule tokens to find an
                    // optional trailing comment on the same logical line.
                    tok_idx += 1; // skip rulename
                    tok_idx = Token.skipBodyTokens(tokens, tok_idx, isRuleStart);

                    // Emit any trailing comment on this rule's line.
                    if (tok_idx < tokens.len and tokens[tok_idx].tag == .comment) {
                        try writer.writeByte(' ');
                        try writer.writeAll(tokens[tok_idx].lexeme(source));
                        tok_idx += 1;
                    }
                    try writer.writeByte('\n');
                    // Skip trailing newline(s).
                    while (tok_idx < tokens.len and tokens[tok_idx].tag == .newline)
                        tok_idx += 1;
                } else {
                    // No more parsed rules; skip the token.
                    tok_idx += 1;
                }
            },
            else => {
                // Unexpected token outside a rule — skip.
                tok_idx += 1;
            },
        }
    }
}

/// True when the token at `idx` begins a new rule (rulename followed by
/// `=` or `=/`).
fn isRuleStart(tokens: []const Token, idx: usize) bool {
    if (tokens[idx].tag != .rulename) return false;
    const after = Token.nextMeaningful(tokens, idx + 1);
    return after < tokens.len and
        (tokens[after].tag == .equals or tokens[after].tag == .equals_slash);
}

/// Format a complete grammar from only the AST (no comment preservation).
pub fn formatGrammarRules(rules: []const Ast.Rule, writer: anytype) !void {
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
        // PEG-only nodes — not produced by the ABNF parser, but handle
        // gracefully to keep the switch exhaustive.
        .and_predicate, .not_predicate, .char_class, .any => {},
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
    try formatGrammar(rules, tokens, input, fbs.writer());
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

test "inline comment preserved" {
    try expectFmt("foo = bar ; a comment\n", "foo = bar ; a comment");
}

test "standalone comment before rule" {
    try expectFmt("; header\nfoo = bar\n", "; header\nfoo = bar");
}

test "multiple leading comments" {
    try expectFmt("; line 1\n; line 2\nfoo = bar\n", "; line 1\n; line 2\nfoo = bar");
}

test "inline and standalone comments" {
    try expectFmt(
        \\; section
        \\foo = a ; comment on foo
        \\bar = b
        \\
    ,
        \\; section
        \\foo = a ; comment on foo
        \\bar = b
    );
}

test "comment between rules" {
    try expectFmt(
        \\foo = a
        \\; separator
        \\bar = b
        \\
    ,
        \\foo = a
        \\; separator
        \\bar = b
    );
}

test "multi-line comment block" {
    try expectFmt(
        \\; line 1
        \\; line 2
        \\; line 3
        \\foo = a
        \\
    ,
        \\; line 1
        \\; line 2
        \\; line 3
        \\foo = a
    );
}
