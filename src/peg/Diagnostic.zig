const diagnostic = @import("../diagnostic.zig");
const Token = @import("Token.zig").Token;

pub const Expected = enum {
    identifier,
    expression,
    left_arrow,
    right_paren,
    /// `#@` comment whose content does not parse as a recovery directive.
    directive_malformed,
    /// `#@ throw` directive dropped because `max_throw_dirs` was reached.
    directive_throw_overflow,
    /// `#@ rule ... catches ...` directive dropped because `max_catch_dirs`
    /// was reached.
    directive_catch_overflow,

    /// Custom message hook honoured by `diagnostic.Diagnostic.format`.
    /// Returns true when this variant produced its own message and the
    /// generic "expected X, found Y" template should be skipped.
    pub fn writeMessage(self: Expected, writer: anytype, found_lexeme: []const u8) !bool {
        switch (self) {
            .directive_malformed => try writer.print(
                "malformed recovery directive: '{s}'",
                .{found_lexeme},
            ),
            .directive_throw_overflow => try writer.print(
                "throw directive dropped (max_throw_dirs reached): '{s}'",
                .{found_lexeme},
            ),
            .directive_catch_overflow => try writer.print(
                "catch directive dropped (max_catch_dirs reached): '{s}'",
                .{found_lexeme},
            ),
            else => return false,
        }
        return true;
    }
};

pub const Diagnostic = diagnostic.Diagnostic(Expected, Token.Tag);
