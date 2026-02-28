const token = @import("../token.zig");

/// All possible token types in RFC 9804 S-expressions.
pub const Tag = enum {
    // Delimiters
    lparen, // (
    rparen, // )
    lbracket, // [  (display hint open)
    rbracket, // ]  (display hint close)
    lbrace, // {  (base-64 wrapped sexp open)
    rbrace, // }  (base-64 wrapped sexp close)

    // Octet-string encodings
    verbatim, // decimal ":" octets (combined as one token)
    quoted_string, // "..." with escapes
    sexp_token, // bareword: alpha/punc followed by alnum/punc
    hexadecimal, // #hex-digits#
    base64, // |base64-chars|

    // Numeric prefix (for quoted_string, hex, base64 with length)
    decimal, // digits not followed by ':'

    // Trivia
    whitespace,

    // Special
    eof,
    invalid,
};

/// A token produced by the S-expression scanner.
pub const Token = token.Token(Tag);
