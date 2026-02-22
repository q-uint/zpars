/// Generic token type constructor for grammar scanners.
///
/// Each grammar format defines its own `Tag` enum and instantiates this
/// to get a concrete token type with shared field layout and methods.
pub fn Token(comptime TagType: type) type {
    return struct {
        const Self = @This();
        pub const Tag = TagType;

        tag: Tag,
        /// Byte offset into source where this token's lexeme starts.
        start: usize,
        /// Length of the lexeme in bytes.
        len: usize,
        /// Line number (1-based) where this token appears.
        line: usize,

        /// Returns the lexeme slice from the source text.
        pub fn lexeme(self: Self, source: []const u8) []const u8 {
            return source[self.start .. self.start + self.len];
        }
    };
}
