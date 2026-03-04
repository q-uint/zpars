const diagnostic = @import("../diagnostic.zig");
const Token = @import("Scanner.zig").Scanner.Token;

pub const Expected = enum {
    rulename,
    definition,
    element,
};

pub const Diagnostic = diagnostic.Diagnostic(Expected, Token.Tag);
