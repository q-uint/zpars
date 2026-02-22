/// A token produced by the BNF scanner (ALGOL 60 variant).
/// Each token is a tagged slice into the original source text.
const Token = @This();

tag: Tag,
/// Byte offset into source where this token's lexeme starts.
start: usize,
/// Length of the lexeme in bytes.
len: usize,
/// Line number (1-based) where this token appears.
line: usize,

/// All possible token types in BNF.
pub const Tag = enum {
    rulename, // <name>
    definition, // ::=
    pipe, // |
    terminal, // bare literal text

    // Structural
    newline, // CRLF or LF

    // Special
    eof,
    invalid,
};

/// Returns the lexeme slice from the source text.
pub fn lexeme(self: Token, source: []const u8) []const u8 {
    return source[self.start .. self.start + self.len];
}
