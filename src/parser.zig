/// Generic parser primitive mixin for grammar parsers.
///
/// Each grammar parser defines its own token tag type, trivia tags, and
/// synchronization config, then imports the shared methods via const aliases:
///
///   const base = parser.ParserBase(Parser, Token, Diagnostic, ...);
///   const peek = base.peek;
///   const advance = base.advance;
///   // etc.
pub fn ParserBase(
    comptime Self: type,
    comptime TokenType: type,
    comptime DiagnosticType: type,
    comptime trivia_tags: []const TokenType.Tag,
    comptime sync_config: struct {
        name_tag: TokenType.Tag,
        def_tags: []const TokenType.Tag,
    },
) type {
    return struct {
        fn isTrivia(tag: TokenType.Tag) bool {
            inline for (trivia_tags) |t| {
                if (tag == t) return true;
            }
            return false;
        }

        fn isDef(tag: TokenType.Tag) bool {
            inline for (sync_config.def_tags) |t| {
                if (tag == t) return true;
            }
            return false;
        }

        pub fn peek(self: *Self) TokenType {
            return self.tokens[@min(self.pos, self.tokens.len - 1)];
        }

        pub fn peekAt(self: *Self, offset: usize) TokenType {
            const idx = self.pos + offset;
            if (idx >= self.tokens.len) return .{ .tag = .eof, .start = 0, .len = 0, .line = 0 };
            return self.tokens[idx];
        }

        pub fn advance(self: *Self) TokenType {
            // Clamp so that error-recovery paths that advance() past
            // eof don't push pos out of bounds. Once at eof, repeated
            // advance() calls keep returning eof.
            const i: usize = @min(self.pos, self.tokens.len - 1);
            const tok = self.tokens[i];
            self.pos = i + 1;
            return tok;
        }

        pub fn skipTrivia(self: *Self) void {
            while (isTrivia(self.peek().tag)) self.pos += 1;
        }

        /// Next non-trivia token tag after the current position.
        pub fn peekNextMeaningful(self: *Self) TokenType.Tag {
            var i = self.pos + 1;
            while (i < self.tokens.len) : (i += 1) {
                const tag = self.tokens[i].tag;
                if (!isTrivia(tag)) return tag;
            }
            return .eof;
        }

        /// Skip tokens until the start of the next rule or EOF.
        pub fn synchronize(self: *Self) void {
            while (self.peek().tag != .eof) {
                if (self.peek().tag == sync_config.name_tag) {
                    const next = self.peekNextMeaningful();
                    if (isDef(next)) return;
                }
                self.pos += 1;
            }
        }

        pub fn fail(self: *Self, expected: DiagnosticType.Expected, token: TokenType) void {
            _ = self.diagnostics.addOne(.{
                .expected = expected,
                .found_tag = token.tag,
                .found_start = token.start,
                .found_len = token.len,
                .line = token.line,
            });
        }
    };
}
