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

/// Index of the next non-trivia (non-comment, non-newline) token at or
/// after `start`. Lives outside `Token(Tag)` because its body references
/// `.comment`/`.newline` tag variants that not every grammar defines;
/// as a free generic it is only analyzed when actually called.
pub fn nextMeaningful(comptime TokenType: type, tokens: []const TokenType, start: usize) usize {
    var i = start;
    while (i < tokens.len) : (i += 1) {
        if (tokens[i].tag != .comment and tokens[i].tag != .newline) return i;
    }
    return i;
}

/// Advance past all tokens that belong to a rule/definition body
/// (everything up to but not including the next comment, newline that
/// isn't a continuation, or eof). A newline is a continuation if it's
/// followed by non-trivia tokens that don't start a new rule, as
/// determined by `isRuleStart`.
pub fn skipBodyTokens(
    comptime TokenType: type,
    tokens: []const TokenType,
    start: usize,
    comptime isRuleStart: fn ([]const TokenType, usize) bool,
) usize {
    var i = start;
    while (i < tokens.len) {
        const tag = tokens[i].tag;
        switch (tag) {
            .eof => return i,
            .comment => return i,
            .newline => {
                const next = nextMeaningful(TokenType, tokens, i + 1);
                if (next >= tokens.len or tokens[next].tag == .eof) return i;
                if (isRuleStart(tokens, next)) return i;
                i += 1;
            },
            else => i += 1,
        }
    }
    return i;
}
