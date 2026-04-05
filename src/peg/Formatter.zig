/// PEG formatter — pretty-prints an AST back into PEG notation.
///
/// Rule names are padded to align the `<-` operators across all rules.
/// Comments are preserved from the token stream.
const std = @import("std");
const Ast = @import("../Ast.zig");
const token_mod = @import("../token.zig");
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
                if (tok_idx < tokens.len and tokens[tok_idx].tag == .newline)
                    tok_idx += 1;
            },
            .newline => {
                tok_idx += 1;
            },
            .identifier => {
                // Check if this identifier starts a definition (followed by <-).
                if (isDefinitionStart(tokens, tok_idx)) {
                    if (rule_idx < rules.len) {
                        const rule = rules[rule_idx];
                        rule_idx += 1;

                        try writer.writeAll(rule.name);
                        for (0..max_name - rule.name.len + 1) |_| try writer.writeByte(' ');
                        try writer.writeAll("<- ");
                        try formatNode(rule.node, false, writer);

                        // Skip past the original definition tokens.
                        tok_idx += 1; // skip identifier
                        tok_idx = token_mod.skipBodyTokens(Token, tokens, tok_idx, isDefinitionStart);

                        // Emit any trailing comment.
                        if (tok_idx < tokens.len and tokens[tok_idx].tag == .comment) {
                            try writer.writeByte(' ');
                            try writer.writeAll(tokens[tok_idx].lexeme(source));
                            tok_idx += 1;
                        }
                        try writer.writeByte('\n');
                        while (tok_idx < tokens.len and tokens[tok_idx].tag == .newline)
                            tok_idx += 1;
                    } else {
                        tok_idx += 1;
                    }
                } else {
                    tok_idx += 1;
                }
            },
            else => {
                tok_idx += 1;
            },
        }
    }
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
        try writer.writeAll("<- ");
        try formatNode(rule.node, false, writer);
        try writer.writeByte('\n');
    }
}

/// Format a single AST node as PEG.
/// `in_predicate` is true when the node is the direct child of a predicate,
/// so compound expressions know to wrap themselves in parens.
pub fn formatNode(node: Ast.Node, in_predicate: bool, writer: anytype) anyerror!void {
    switch (node) {
        .alternation => |alts| {
            if (in_predicate) try writer.writeByte('(');
            for (alts, 0..) |alt, i| {
                if (i > 0) try writer.writeAll(" / ");
                try formatNode(alt, false, writer);
            }
            if (in_predicate) try writer.writeByte(')');
        },
        .concatenation => |elems| {
            if (in_predicate and elems.len > 1) try writer.writeByte('(');
            for (elems, 0..) |elem, i| {
                if (i > 0) try writer.writeByte(' ');
                switch (elem) {
                    .alternation => {
                        try writer.writeByte('(');
                        try formatNode(elem, false, writer);
                        try writer.writeByte(')');
                    },
                    else => try formatNode(elem, false, writer),
                }
            }
            if (in_predicate and elems.len > 1) try writer.writeByte(')');
        },
        .repetition => |rep| try formatRepetition(rep, writer),
        .char_val => |cv| {
            try writer.writeByte('\'');
            try writeLiteralEscaped(cv.value, writer);
            try writer.writeByte('\'');
        },
        .rulename => |name| try writer.writeAll(name),
        .and_predicate => |inner| {
            try writer.writeByte('&');
            try formatNode(inner.*, true, writer);
        },
        .not_predicate => |inner| {
            try writer.writeByte('!');
            try formatNode(inner.*, true, writer);
        },
        .char_class => |ranges| try formatCharClass(ranges, writer),
        .any => try writer.writeByte('.'),
        // Nodes not produced by the PEG parser.
        .num_val, .prose_val, .neg_char_class, .anchor_start, .anchor_end, .capture => unreachable,
    }
}

fn formatRepetition(rep: Ast.Repetition, writer: anytype) anyerror!void {
    // Determine suffix character.
    const needs_group = switch (rep.element.*) {
        .alternation, .concatenation => true,
        else => false,
    };

    if (needs_group) try writer.writeByte('(');
    try formatNode(rep.element.*, false, writer);
    if (needs_group) try writer.writeByte(')');

    if (rep.min == 0 and rep.max != null and rep.max.? == 1) {
        try writer.writeByte('?');
    } else if (rep.min == 0 and rep.max == null) {
        try writer.writeByte('*');
    } else if (rep.min == 1 and rep.max == null) {
        try writer.writeByte('+');
    }
    // Other repetition forms don't have PEG equivalents; skip suffix.
}

fn writeLiteralEscaped(value: []const u8, writer: anytype) !void {
    for (value) |c| {
        switch (c) {
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            '\\' => try writer.writeAll("\\\\"),
            '\'' => try writer.writeAll("\\'"),
            else => {
                if (c >= 0x20 and c <= 0x7E) {
                    try writer.writeByte(c);
                } else {
                    try writer.writeByte('\\');
                    try writer.writeByte('0' + (c >> 6));
                    try writer.writeByte('0' + ((c >> 3) & 7));
                    try writer.writeByte('0' + (c & 7));
                }
            },
        }
    }
}

fn formatCharClass(ranges: []const Ast.ClassRange, writer: anytype) !void {
    try writer.writeByte('[');
    for (ranges) |r| {
        try writeClassChar(r.lo, writer);
        if (r.hi != r.lo) {
            try writer.writeByte('-');
            try writeClassChar(r.hi, writer);
        }
    }
    try writer.writeByte(']');
}

fn writeClassChar(c: u8, writer: anytype) !void {
    switch (c) {
        '\n' => try writer.writeAll("\\n"),
        '\r' => try writer.writeAll("\\r"),
        '\t' => try writer.writeAll("\\t"),
        '\\' => try writer.writeAll("\\\\"),
        ']' => try writer.writeAll("\\]"),
        '[' => try writer.writeAll("\\["),
        else => {
            if (c >= 0x20 and c <= 0x7E) {
                try writer.writeByte(c);
            } else {
                // Octal escape for non-printable.
                try writer.writeByte('\\');
                try writer.writeByte('0' + (c >> 6));
                try writer.writeByte('0' + ((c >> 3) & 7));
                try writer.writeByte('0' + (c & 7));
            }
        },
    }
}

fn isDefinitionStart(tokens: []const Token, idx: usize) bool {
    if (tokens[idx].tag != .identifier) return false;
    const after = token_mod.nextMeaningful(Token, tokens, idx + 1);
    return after < tokens.len and tokens[after].tag == .left_arrow;
}

const Scanner = @import("Scanner.zig");
const PegParser = @import("Parser.zig");

fn expectFmt(expected: []const u8, input: []const u8) !void {
    var scanner = Scanner.init(input);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, input);
    const rules = try parser.parse();
    std.debug.assert(parser.getDiagnostics().len == 0);
    var buf: [4096]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try formatGrammar(rules, tokens, input, fbs.writer());
    try std.testing.expectEqualStrings(expected, fbs.getWritten());
}

test "simple definition" {
    try expectFmt("A <- B\n", "A <- B");
}

test "ordered choice" {
    try expectFmt("A <- B / C / D\n", "A <- B / C / D");
}

test "sequence" {
    try expectFmt("A <- B C D\n", "A <- B C D");
}

test "star" {
    try expectFmt("A <- B*\n", "A <- B*");
}

test "plus" {
    try expectFmt("A <- B+\n", "A <- B+");
}

test "question" {
    try expectFmt("A <- B?\n", "A <- B?");
}

test "and predicate" {
    try expectFmt("A <- &B\n", "A <- &B");
}

test "not predicate" {
    try expectFmt("A <- !B\n", "A <- !B");
}

test "dot" {
    try expectFmt("A <- .\n", "A <- .");
}

test "literal" {
    try expectFmt("A <- 'hello'\n", "A <- 'hello'");
}

test "character class" {
    try expectFmt("A <- [a-zA-Z_]\n", "A <- [a-zA-Z_]");
}

test "grouped alternation in sequence" {
    try expectFmt("A <- (B / C) D\n", "A <- (B / C) D");
}

test "multi-rule alignment" {
    try expectFmt(
        \\Expr <- Term
        \\Term <- Factor
        \\
    ,
        \\Expr <- Term
        \\Term <- Factor
    );
}

test "comment preserved" {
    try expectFmt("# header\nA <- B\n", "# header\nA <- B");
}

test "end of file pattern" {
    try expectFmt("EOF <- !.\n", "EOF <- !.");
}
