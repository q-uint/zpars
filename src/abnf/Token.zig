const token = @import("../token.zig");

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
    char_val, // "quoted string" (case-insensitive, RFC 5234)
    char_val_ci, // %i"quoted string" (case-insensitive, RFC 7405)
    char_val_cs, // %s"quoted string" (case-sensitive, RFC 7405)
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

/// A token produced by the ABNF scanner.
/// Each token is a tagged slice into the original source text.
pub const Token = token.Token(Tag);
