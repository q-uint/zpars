/// A token produced by the ABNF scanner.
/// Each token is a tagged slice into the original source text.
const Token = @This();

tag: Tag,
/// Byte offset into source where this token's lexeme starts.
start: usize,
/// Length of the lexeme in bytes.
len: usize,
/// Line number (1-based) where this token appears.
line: usize,

/// All possible token types in ABNF (RFC 5234).
pub const Tag = enum {
    // Single-character tokens
    left_paren, // (
    right_paren, // )
    left_bracket, // [
    right_bracket, // ]
    slash, // /
    star, // *

    // One or two character tokens
    equals, // =
    equals_slash, // =/

    // Literals
    rulename, // e.g. "ALPHA", "my-rule"
    number, // e.g. "3" in "3*5"
    char_val, // "quoted string"
    prose_val, // <prose description>
    bin_val, // %b01010
    dec_val, // %d65
    hex_val, // %x41 or %x41-5A or %x41.42.43

    // Structural
    comment, // ; to end of line
    newline, // CRLF or LF

    // Special
    eof,
    invalid,
};

/// Returns the lexeme slice from the source text.
pub fn lexeme(self: Token, source: []const u8) []const u8 {
    return source[self.start..self.start + self.len];
}
