const diagnostic = @import("../diagnostic.zig");
const Token = @import("Token.zig").Token;

pub const Expected = enum {
    rulename,
    element,
    left_paren,
    right_paren,
    left_bracket,
    right_bracket,
};

pub const Diagnostic = diagnostic.Diagnostic(Expected, Token.Tag);
