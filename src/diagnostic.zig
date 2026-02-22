/// Generic diagnostic type constructor for grammar parsers.
///
/// Each grammar format defines its own `Expected` enum and token tag type,
/// then instantiates this to get a concrete diagnostic with shared methods
/// for source location and error formatting.
pub fn Diagnostic(comptime ExpectedType: type, comptime FoundTagType: type) type {
    return struct {
        const Self = @This();
        pub const Expected = ExpectedType;
        pub const FoundTag = FoundTagType;

        /// What the parser expected at this position.
        expected: Expected,
        /// The token that was actually found.
        found_tag: FoundTag,
        /// Byte offset into source where the unexpected token starts.
        found_start: usize,
        /// Length of the found token's lexeme.
        found_len: usize,
        /// Line number (1-based).
        line: usize,

        /// Compute the 1-based column number by scanning backward from
        /// `found_start` to find the beginning of the line.
        pub fn column(self: Self, source: []const u8) usize {
            var line_start: usize = self.found_start;
            while (line_start > 0 and source[line_start - 1] != '\n') {
                line_start -= 1;
            }
            return self.found_start - line_start + 1;
        }

        /// Extract the full source line containing the error.
        pub fn sourceLine(self: Self, source: []const u8) []const u8 {
            var line_start: usize = self.found_start;
            while (line_start > 0 and source[line_start - 1] != '\n') {
                line_start -= 1;
            }
            var line_end: usize = self.found_start;
            while (line_end < source.len and source[line_end] != '\n' and source[line_end] != '\r') {
                line_end += 1;
            }
            return source[line_start..line_end];
        }

        /// Format the full diagnostic message to a writer.
        pub fn format(self: Self, source: []const u8, filename: []const u8, writer: anytype) !void {
            const col = self.column(source);
            const line = self.sourceLine(source);
            const found_lexeme = if (self.found_len > 0)
                source[self.found_start .. self.found_start + self.found_len]
            else
                "eof";

            try writer.print("{s}:{d}:{d}: error: expected {s}, found '{s}'\n", .{
                filename,
                self.line,
                col,
                @tagName(self.expected),
                found_lexeme,
            });
            try writer.print("   {s}\n", .{line});
            for (0..col - 1 + 3) |_| try writer.writeByte(' ');
            try writer.print("^\n", .{});
        }
    };
}

// --- Tests -------------------------------------------------------------------

const std = @import("std");

const TestExpected = enum { element, rulename };
const TestTag = enum { eof, other };
const TestDiag = Diagnostic(TestExpected, TestTag);

test "column on first line" {
    const source = "foo = (a / )";
    const diag = TestDiag{
        .expected = .element,
        .found_tag = .other,
        .found_start = 11,
        .found_len = 1,
        .line = 1,
    };
    try std.testing.expectEqual(12, diag.column(source));
}

test "column on second line" {
    const source = "first line\nsecond line";
    const diag = TestDiag{
        .expected = .element,
        .found_tag = .eof,
        .found_start = 15,
        .found_len = 0,
        .line = 2,
    };
    try std.testing.expectEqual(5, diag.column(source));
}

test "sourceLine extracts correct line" {
    const source = "first line\nsecond line\nthird line";
    const diag = TestDiag{
        .expected = .element,
        .found_tag = .eof,
        .found_start = 15,
        .found_len = 0,
        .line = 2,
    };
    try std.testing.expectEqualStrings("second line", diag.sourceLine(source));
}
