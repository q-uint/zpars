const token = @import("../token.zig");

/// All possible token types in PEG.
pub const Tag = enum {
    // Identifiers
    identifier, // e.g. "Grammar", "Digit", "ident_cont"

    // Operators
    left_arrow, // <-
    slash, // /
    @"and", // &
    not, // !
    question, // ?
    star, // *
    plus, // +
    left_paren, // (
    right_paren, // )
    dot, // .

    // Literals
    literal, // 'string' or "string"
    char_class, // [a-zA-Z0-9]

    // Trivia
    comment, // # to end of line
    newline, // LF or CRLF

    // Special
    eof,
    invalid,
};

/// A token produced by the PEG scanner.
/// Each token is a tagged slice into the original source text.
pub const Token = token.Token(Tag);
