const token = @import("../token.zig");

/// All possible token types in BNF (ALGOL 60 variant).
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

/// A token produced by the BNF scanner.
/// Each token is a tagged slice into the original source text.
pub const Token = token.Token(Tag);
