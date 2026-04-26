/// Token type for tree-sitter-style query syntax.
///
/// Queries are S-expression-shaped but use tree-sitter conventions
/// (`@captures`, `#predicates?`, `[alternation]`, quantifiers, anchors)
/// rather than RFC 9804 S-expressions, so they need their own scanner.
const token = @import("../token.zig");

pub const Tag = enum {
    /// `(`
    lparen,
    /// `)`
    rparen,
    /// `[` -- start of alternation group.
    lbracket,
    /// `]` -- end of alternation group.
    rbracket,
    /// `.` -- sibling anchor inside a child list.
    dot,
    /// `?` -- optional quantifier.
    question,
    /// `*` -- zero-or-more quantifier.
    star,
    /// `+` -- one-or-more quantifier.
    plus,
    /// `@name` -- capture binding. Lexeme includes the leading `@`.
    at_identifier,
    /// `#name`, `#name?`, or `#name!` -- predicate head.
    /// Lexeme includes the leading `#` and any trailing `?`/`!`.
    predicate,
    /// `"..."` quoted string literal. Lexeme includes the quotes.
    string,
    /// Bare identifier. Includes reserved words `_`, `ERROR`, `MISSING`,
    /// `partial`; the parser distinguishes them by lexeme.
    identifier,
    /// `; ...` line comment. Includes the leading `;` but not the trailing newline.
    comment,

    eof,
    invalid,
};

pub const Token = token.Token(Tag);
