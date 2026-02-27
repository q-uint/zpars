const diagnostic = @import("../diagnostic.zig");
const Token = @import("Token.zig").Token;

pub const Expected = enum {
    identifier,
    expression,
    left_arrow,
    right_paren,
};

pub const Diagnostic = diagnostic.Diagnostic(Expected, Token.Tag);
