/// BNF scanner -- comptime-generated from ABNF grammar.
///
/// Tokenizes BNF grammars (ALGOL 60 variant). The notation uses three
/// meta-symbols: `<>` (metalinguistic variable), `::=` (definition),
/// and `|` (alternation). All other marks are terminal literals.
pub const Scanner = @import("../abnf/Tokenizer.zig").CompileScanner(
    \\rulename   = "<" 1*(%x20-3D / %x3F-7E) ">"
    \\definition = "::="
    \\pipe       = "|"
    \\newline    = %x0D %x0A / %x0A
, .{ .skip = &.{ ' ', '\t' }, .catch_all = "terminal", .line_tag = "newline" });

const std = @import("std");

fn expectTags(source: []const u8, expected: []const Scanner.Tag) !void {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    const allocator = std.testing.allocator;
    const actual = try allocator.alloc(Scanner.Tag, tokens.len);
    defer allocator.free(actual);
    for (tokens, 0..) |t, i| actual[i] = t.tag;
    try std.testing.expectEqualSlices(Scanner.Tag, expected, actual);
}

test "simple rule" {
    try expectTags("<digit> ::= 0", &.{ .rulename, .definition, .terminal, .eof });
}

test "alternation" {
    try expectTags("<bit> ::= 0 | 1", &.{
        .rulename, .definition, .terminal, .pipe, .terminal, .eof,
    });
}

test "non-terminal reference" {
    try expectTags("<ab> ::= <a> <b>", &.{
        .rulename, .definition, .rulename, .rulename, .eof,
    });
}

test "multi-char terminal" {
    try expectTags("<kw> ::= while", &.{ .rulename, .definition, .terminal, .eof });
}

test "multiline" {
    try expectTags("<a> ::= x\n<b> ::= y", &.{
        .rulename, .definition, .terminal, .newline,
        .rulename, .definition, .terminal, .newline,
        .eof,
    });
}

test "empty rule" {
    try expectTags("<empty> ::=", &.{ .rulename, .definition, .eof });
}

test "ALGOL 60 example" {
    try expectTags("<ab> ::= ( | [ | <ab> ( | <ab> <d>", &.{
        .rulename, .definition,
        .terminal, .pipe,
        .terminal, .pipe,
        .rulename, .terminal,
        .pipe,     .rulename,
        .rulename, .eof,
    });
}

test "bare colon as terminal" {
    try expectTags("<x> ::= a:b", &.{ .rulename, .definition, .terminal, .eof });
}

test "unterminated rulename" {
    try expectTags("<oops", &.{ .invalid, .eof });
}

test "line tracking" {
    var scanner = Scanner.init("<a> ::= x\n<b> ::= y\n");
    const tokens = scanner.scanTokens();
    try std.testing.expectEqual(1, tokens[0].line); // <a>
    try std.testing.expectEqual(1, tokens[1].line); // ::=
    try std.testing.expectEqual(1, tokens[2].line); // x
    try std.testing.expectEqual(1, tokens[3].line); // \n
    try std.testing.expectEqual(2, tokens[4].line); // <b>
    try std.testing.expectEqual(2, tokens[5].line); // ::=
    try std.testing.expectEqual(2, tokens[6].line); // y
    try std.testing.expectEqual(2, tokens[7].line); // \n
    try std.testing.expectEqual(3, tokens[8].line); // eof
}
