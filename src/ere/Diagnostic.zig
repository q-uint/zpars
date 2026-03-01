const diagnostic = @import("../diagnostic.zig");
const Token = @import("Token.zig").Token;

pub const Expected = enum {
    expression,
    right_paren,
    right_brace,
    number,
    eof,
};

pub const Diagnostic = diagnostic.Diagnostic(Expected, Token.Tag);
