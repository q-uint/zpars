const token = @import("../token.zig");

/// All possible token types in CFG notation.
pub const Tag = enum {
    /// Bare identifier (nonterminal reference).
    identifier,
    /// Quoted string literal: `"text"`.
    string,
    /// Case-sensitive string: `%s"text"`.
    string_cs,
    /// Case-insensitive string: `%i"text"`.
    string_ci,
    /// Hex byte: `%x41`.
    hex_byte,
    /// Hex range: `%x41-5A`.
    hex_range,
    /// Production arrow: `->`.
    arrow,
    /// Alternation: `|`.
    pipe,

    // Structural
    newline,

    // Special
    eof,
    invalid,
};

/// A token produced by the CFG scanner.
/// Each token is a tagged slice into the original source text.
pub const Token = token.Token(Tag);
