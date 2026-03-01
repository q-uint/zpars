const token = @import("../token.zig");

/// All possible token types in POSIX Extended Regular Expressions.
pub const Tag = enum {
    // Atoms
    char, // ordinary or escaped character
    dot, // .
    caret, // ^
    dollar, // $

    // Quantifiers
    star, // *
    plus, // +
    question, // ?
    lbrace, // {
    rbrace, // }
    comma, // ,
    number, // digit sequence inside interval

    // Grouping
    left_paren, // (
    right_paren, // )

    // Alternation
    pipe, // |

    // Bracket expression (entire [...] including content)
    bracket_expr,

    // Special
    eof,
    invalid,
};

/// A token produced by the ERE scanner.
/// Each token is a tagged slice into the original source text.
pub const Token = token.Token(Tag);
