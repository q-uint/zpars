/// PEG parser -- produces an AST from a PEG token stream.
///
/// Grammar (Bryan Ford, 2004):
///   Grammar    <- Spacing Definition+ EndOfFile
///   Definition <- Identifier LEFTARROW Expression
///   Expression <- Sequence (SLASH Sequence)*
///   Sequence   <- Prefix*
///   Prefix     <- (AND / NOT)? Suffix
///   Suffix     <- Primary (QUESTION / STAR / PLUS)?
///   Primary    <- Identifier !LEFTARROW
///              /  OPEN Expression CLOSE
///              /  Literal / Class / DOT
const std = @import("std");
const Token = @import("Token.zig").Token;
const Ast = @import("../Ast.zig");
const Diagnostic = @import("Diagnostic.zig").Diagnostic;
const parser_base = @import("../parser.zig");
const Pool = @import("../pool.zig").Pool;
const char_flags = @import("../char_flags.zig");

pub const Config = struct {
    max_rules: usize = 256,
    max_nodes: usize = 4096,
    max_ranges: usize = 1024,
    max_bytes: usize = 1024,
    max_diagnostics: usize = 64,
    /// When true, line comments starting with `#@` are interpreted as
    /// recovery directives (`#@ throw <label>`, `#@ rule <name> catches
    /// <label> -> <handler>`, `#@ labels: ...`). When false (default),
    /// `#@` comments are treated as ordinary PEG comments with no
    /// recovery semantics - matching how every other PEG implementation
    /// reads such files. Gating preserves backwards compatibility for
    /// the common (no-recovery) case and avoids the directive tables'
    /// stack footprint when unused.
    recovery: bool = false,
    /// Maximum number of `#@ throw` directives in a single file. Only
    /// allocated when `recovery` is true.
    max_throw_dirs: usize = 128,
    /// Maximum number of `#@ rule ... catches ...` directives in a
    /// single file. Only allocated when `recovery` is true.
    max_catch_dirs: usize = 128,
    /// Maximum number of literals collected from `#@ tokens "..." ...`
    /// directives. Multiple directives accumulate into the same flat
    /// list. Only allocated when `recovery` is true.
    max_tagged_tokens: usize = 256,
    /// Maximum number of literal entries parsed from a single
    /// `#@ tokens` directive line. Limits the per-directive buffer
    /// `parseDirective` returns. Multiple directives can be used to
    /// exceed this without raising the overall cap.
    max_tagged_tokens_per_directive: usize = 32,
    /// Maximum number of `#@ field` directives in a single file. Only
    /// allocated when `recovery` is true.
    max_field_dirs: usize = 256,
};

pub const Parser = ParserWith(.{});

pub fn ParserWith(comptime config: Config) type {
    return struct {
        const Self = @This();

        pub const max_diagnostics = config.max_diagnostics;

        const primitives = parser_base.ParserBase(Self, Token, Diagnostic, &.{ .comment, .newline }, .{
            .name_tag = .identifier,
            .def_tags = &.{.left_arrow},
        });
        pub const peek = primitives.peek;
        const advance = primitives.advance;
        pub const skipTrivia = primitives.skipTrivia;
        pub const peekNextMeaningful = primitives.peekNextMeaningful;
        pub const synchronize = primitives.synchronize;
        const fail = primitives.fail;

        pub const ParseError = error{ SyntaxError, Overflow };

        tokens: []const Token,
        source: []const u8,
        pos: usize = 0,

        /// Pool for AST nodes.
        nodes: Pool(Ast.Node, config.max_nodes) = .{},

        /// Pool for ClassRange entries.
        ranges: Pool(Ast.ClassRange, config.max_ranges) = .{},

        /// Pool for decoded literal bytes.
        bytes: Pool(u8, config.max_bytes) = .{},

        /// Parsed rules.
        rules: Pool(Ast.Rule, config.max_rules) = .{},

        /// Accumulated parse diagnostics.
        diagnostics: Pool(Diagnostic, config.max_diagnostics) = .{},

        /// `#@ throw <label>` directives, collected from comment tokens
        /// in `parse()` before any rule is parsed. Each entry records the
        /// label name and the index of the comment token in
        /// `self.tokens`, used to determine which alternative the
        /// directive trails. Only allocated when `config.recovery`.
        throw_dirs: if (config.recovery) [config.max_throw_dirs]ThrowDir else void =
            if (config.recovery) undefined else {},
        throw_dir_count: if (config.recovery) usize else void =
            if (config.recovery) 0 else {},

        /// `#@ rule <name> catches <label> -> <handler>` directives.
        catch_dirs: if (config.recovery) [config.max_catch_dirs]CatchDir else void =
            if (config.recovery) undefined else {},
        catch_dir_count: if (config.recovery) usize else void =
            if (config.recovery) 0 else {},

        /// Flat list of literal byte slices accumulated from every
        /// `#@ tokens "..." ...` directive. Slices into `self.source`.
        /// Surfaced via `getTaggedTokens` for the VM compiler's
        /// `token_events = .tagged` mode.
        tagged_tokens: if (config.recovery) [config.max_tagged_tokens][]const u8 else void =
            if (config.recovery) undefined else {},
        tagged_token_count: if (config.recovery) usize else void =
            if (config.recovery) 0 else {},

        /// `#@ field <rule> <name> = <target>(#N)?` directives. Applied
        /// after each rule's AST is built by walking the body and
        /// wrapping the matching call site in `Ast.Node.field`.
        field_dirs: if (config.recovery) [config.max_field_dirs]FieldDir else void =
            if (config.recovery) undefined else {},
        field_dir_count: if (config.recovery) usize else void =
            if (config.recovery) 0 else {},

        pub const ThrowDir = struct {
            label: []const u8,
            /// Index into `self.tokens`.
            token_idx: usize,
        };

        pub const CatchDir = struct {
            rule_name: []const u8,
            label: []const u8,
            /// When `is_recover_missing` is true, the handler is the
            /// builtin `recover_missing` form and `handler_rule_name`
            /// is empty.
            handler_rule_name: []const u8,
            is_recover_missing: bool,
        };

        /// `#@ field <rule_name> <field_name> = <target>(#N)?` directive.
        /// Wraps the `ordinal`-th left-to-right occurrence of `target`
        /// in `rule_name`'s body with `Ast.Node.field { name = field_name }`.
        /// `target_is_literal` distinguishes a quoted literal from a
        /// rule reference. `ordinal` is 1-based; 1 is the default when
        /// the directive omits the `#N` suffix.
        pub const FieldDir = struct {
            rule_name: []const u8,
            field_name: []const u8,
            target: []const u8,
            target_is_literal: bool,
            ordinal: u16,
            /// Index into `self.tokens` of the comment that produced
            /// this directive, used to locate diagnostics.
            token_idx: usize,
            /// Set true when `applyFieldDirectives` finds a match;
            /// directives that never matched raise
            /// `directive_field_not_found` after parsing completes.
            applied: bool,
        };

        pub fn init(tokens: []const Token, source: []const u8) Self {
            return .{
                .tokens = tokens,
                .source = source,
            };
        }

        /// Parse all definitions from the token stream.
        pub fn parse(self: *Self) ParseError![]const Ast.Rule {
            if (config.recovery) self.collectDirectives();
            self.skipTrivia();
            while (self.peek().tag != .eof) {
                var rule = self.parseDefinition() catch |err| switch (err) {
                    error.SyntaxError => {
                        self.synchronize();
                        self.skipTrivia();
                        continue;
                    },
                    else => |e| return e,
                };
                if (config.recovery) self.applyFieldDirectives(&rule);
                _ = self.rules.addOne(rule);
                self.skipTrivia();
            }

            if (config.recovery) {
                for (self.field_dirs[0..self.field_dir_count]) |fd| {
                    if (!fd.applied) self.fail(.directive_field_not_found, self.tokens[fd.token_idx]);
                }
            }

            return self.rules.slice();
        }

        /// Walk `rule`'s body and rewrite the targeted call site (rule
        /// reference or quoted literal) for every `#@ field` directive
        /// whose `rule_name` matches. The Nth left-to-right occurrence
        /// is wrapped in an `Ast.Node.field`; mismatched directives are
        /// surfaced via `directive_field_not_found` after `parse()`
        /// finishes (so a typo'd target doesn't degrade silently).
        fn applyFieldDirectives(self: *Self, rule: *Ast.Rule) void {
            for (self.field_dirs[0..self.field_dir_count]) |*fd| {
                if (!std.mem.eql(u8, fd.rule_name, rule.name)) continue;
                var counter: u16 = 0;
                if (self.tagFirstMatch(&rule.node, fd, &counter)) fd.applied = true;
            }
        }

        /// Recursive walk that finds the `fd.ordinal`-th matching call
        /// site in `node` and replaces it with `Ast.Node.field`. Returns
        /// true once the target has been wrapped. `counter` carries the
        /// running count across recursion levels.
        fn tagFirstMatch(self: *Self, node: *Ast.Node, fd: *const FieldDir, counter: *u16) bool {
            if (self.matchesTarget(node.*, fd)) {
                counter.* += 1;
                if (counter.* == fd.ordinal) {
                    const original = self.nodes.addOne(node.*);
                    node.* = .{ .field = .{ .name = fd.field_name, .body = original } };
                    return true;
                }
                return false;
            }
            return switch (node.*) {
                .alternation => |alts| for (alts) |*alt| {
                    if (self.tagFirstMatch(@constCast(alt), fd, counter)) break true;
                } else false,
                .concatenation => |elems| for (elems) |*elem| {
                    if (self.tagFirstMatch(@constCast(elem), fd, counter)) break true;
                } else false,
                .repetition => |rep| self.tagFirstMatch(@constCast(rep.element), fd, counter),
                .and_predicate, .not_predicate => |inner| self.tagFirstMatch(@constCast(inner), fd, counter),
                // Descend through earlier-wrapped `.field` nodes so the
                // counter sees the original target inside, keeping
                // ordinals stable across the order in which directives
                // are applied.
                .field => |f| self.tagFirstMatch(@constCast(f.body), fd, counter),
                else => false,
            };
        }

        fn matchesTarget(self: *const Self, node: Ast.Node, fd: *const FieldDir) bool {
            _ = self;
            if (fd.target_is_literal) {
                return node == .char_val and std.mem.eql(u8, node.char_val.value, fd.target);
            }
            return node == .rulename and std.mem.eql(u8, node.rulename, fd.target);
        }

        /// Pre-pass over comment tokens to collect `#@` directives.
        /// Plain `#`-comments are skipped silently. Comments that begin
        /// with `#@` but fail the directive grammar, or that hit a
        /// directive-table capacity limit, raise a diagnostic so the
        /// dropped intent surfaces instead of degrading to "recovery
        /// just doesn't fire" at runtime. Only invoked when
        /// `config.recovery` is true.
        fn collectDirectives(self: *Self) void {
            for (self.tokens, 0..) |tok, i| {
                if (tok.tag != .comment) continue;
                const lex = tok.lexeme(self.source);
                if (lex.len < 2 or lex[0] != '#' or lex[1] != '@') continue;
                const dir = parseDirective(lex) orelse {
                    self.fail(.directive_malformed, tok);
                    continue;
                };
                switch (dir) {
                    .throw_d => |label| {
                        if (self.throw_dir_count >= self.throw_dirs.len) {
                            self.fail(.directive_throw_overflow, tok);
                            continue;
                        }
                        self.throw_dirs[self.throw_dir_count] = .{ .label = label, .token_idx = i };
                        self.throw_dir_count += 1;
                    },
                    .catch_d => |c| {
                        if (self.catch_dir_count >= self.catch_dirs.len) {
                            self.fail(.directive_catch_overflow, tok);
                            continue;
                        }
                        self.catch_dirs[self.catch_dir_count] = c;
                        self.catch_dir_count += 1;
                    },
                    .labels_d => {
                        // Recognized but not enforced. Accepted silently
                        // so that grammars carrying `#@ labels: ...` for
                        // future enforcement aren't flagged as broken;
                        // surface a diagnostic only once enforcement
                        // exists to back it up.
                    },
                    .tokens_d => |td| {
                        for (td.items[0..td.count]) |item| {
                            if (self.tagged_token_count >= self.tagged_tokens.len) {
                                self.fail(.directive_tokens_overflow, tok);
                                break;
                            }
                            self.tagged_tokens[self.tagged_token_count] = item;
                            self.tagged_token_count += 1;
                        }
                    },
                    .field_d => |fd| {
                        if (self.field_dir_count >= self.field_dirs.len) {
                            self.fail(.directive_field_overflow, tok);
                            continue;
                        }
                        var copy = fd;
                        copy.token_idx = i;
                        copy.applied = false;
                        self.field_dirs[self.field_dir_count] = copy;
                        self.field_dir_count += 1;
                    },
                }
            }
        }

        const Directive = union(enum) {
            throw_d: []const u8,
            catch_d: CatchDir,
            labels_d: void,
            tokens_d: TokensDir,
            field_d: FieldDir,
        };

        /// Up to `max_tagged_tokens_per_directive` literal slices
        /// extracted from a single `#@ tokens "..." ...` line. Slices
        /// reference the comment lexeme (which itself slices the
        /// source), so they outlive the parser as long as the source
        /// does.
        const TokensDir = struct {
            items: [config.max_tagged_tokens_per_directive][]const u8,
            count: u8,
        };

        /// Parse one directive from the comment text. The text starts
        /// with `#`; a leading `#@` (with optional spacing) introduces
        /// a directive. Recognized forms:
        ///   `#@ throw <label>`
        ///   `#@ rule <name> catches <label> -> <handler>`
        ///       where <handler> is `recover_missing` or a rule name
        ///   `#@ labels: <label> (',' <label>)*`
        /// Returns null for plain comments and for malformed directives.
        fn parseDirective(text: []const u8) ?Directive {
            if (text.len < 2 or text[0] != '#' or text[1] != '@') return null;
            var p: usize = 2;
            skipSp(text, &p);
            if (consumeWord(text, &p, "throw")) {
                if (!skipSpRequired(text, &p)) return null;
                const label = parseIdent(text, &p) orelse return null;
                skipSp(text, &p);
                if (p != text.len) return null;
                return .{ .throw_d = label };
            }
            if (consumeWord(text, &p, "rule")) {
                if (!skipSpRequired(text, &p)) return null;
                const rule_name = parseIdent(text, &p) orelse return null;
                if (!skipSpRequired(text, &p)) return null;
                if (!consumeWord(text, &p, "catches")) return null;
                if (!skipSpRequired(text, &p)) return null;
                const label = parseIdent(text, &p) orelse return null;
                if (!skipSpRequired(text, &p)) return null;
                if (p + 1 >= text.len or text[p] != '-' or text[p + 1] != '>') return null;
                p += 2;
                if (!skipSpRequired(text, &p)) return null;
                // Handler: 'recover_missing' !IdentCont / rule_name. The
                // word-boundary check is enforced by consumeWord, so a
                // false return there leaves p untouched and the rule_name
                // path can take over.
                if (consumeWord(text, &p, "recover_missing")) {
                    skipSp(text, &p);
                    if (p != text.len) return null;
                    return .{ .catch_d = .{
                        .rule_name = rule_name,
                        .label = label,
                        .handler_rule_name = "",
                        .is_recover_missing = true,
                    } };
                }
                const handler_rule = parseIdent(text, &p) orelse return null;
                skipSp(text, &p);
                if (p != text.len) return null;
                return .{ .catch_d = .{
                    .rule_name = rule_name,
                    .label = label,
                    .handler_rule_name = handler_rule,
                    .is_recover_missing = false,
                } };
            }
            if (consumeWord(text, &p, "field")) {
                if (!skipSpRequired(text, &p)) return null;
                const rule_name = parseIdent(text, &p) orelse return null;
                if (!skipSpRequired(text, &p)) return null;
                const field_name = parseIdent(text, &p) orelse return null;
                skipSp(text, &p);
                if (p >= text.len or text[p] != '=') return null;
                p += 1;
                skipSp(text, &p);

                // Target: a rule reference or a quoted literal.
                var target: []const u8 = "";
                var target_is_literal = false;
                if (p < text.len and (text[p] == '"' or text[p] == '\'')) {
                    const quote = text[p];
                    p += 1;
                    const start = p;
                    while (p < text.len and text[p] != quote) : (p += 1) {}
                    if (p >= text.len) return null;
                    target = text[start..p];
                    target_is_literal = true;
                    p += 1;
                } else {
                    target = parseIdent(text, &p) orelse return null;
                }

                // Optional `#N` ordinal.
                var ordinal: u16 = 1;
                skipSp(text, &p);
                if (p < text.len and text[p] == '#') {
                    p += 1;
                    if (p >= text.len or text[p] < '0' or text[p] > '9') return null;
                    var n: u32 = 0;
                    while (p < text.len and text[p] >= '0' and text[p] <= '9') : (p += 1) {
                        n = n * 10 + (text[p] - '0');
                        if (n > 65535) return null;
                    }
                    if (n == 0) return null;
                    ordinal = @intCast(n);
                }

                skipSp(text, &p);
                if (p != text.len) return null;
                return .{
                    .field_d = .{
                        .rule_name = rule_name,
                        .field_name = field_name,
                        .target = target,
                        .target_is_literal = target_is_literal,
                        .ordinal = ordinal,
                        .token_idx = 0, // filled by collectDirectives
                        .applied = false,
                    },
                };
            }
            if (consumeWord(text, &p, "tokens")) {
                if (!skipSpRequired(text, &p)) return null;
                var td: TokensDir = .{ .items = undefined, .count = 0 };
                while (p < text.len) {
                    skipSp(text, &p);
                    if (p >= text.len) break;
                    if (text[p] != '"') return null;
                    p += 1;
                    const start = p;
                    while (p < text.len and text[p] != '"') : (p += 1) {}
                    if (p >= text.len) return null; // unterminated
                    const literal = text[start..p];
                    if (literal.len == 0) return null;
                    if (td.count >= td.items.len) return null;
                    td.items[td.count] = literal;
                    td.count += 1;
                    p += 1; // closing quote
                }
                if (td.count == 0) return null;
                return .{ .tokens_d = td };
            }
            if (consumeWord(text, &p, "labels")) {
                // Label sets aren't enforced, but reject garbage after
                // the keyword so typos surface as `directive_malformed`
                // instead of being silently swallowed. Allowed trailing
                // form: optional ':' then any mix of whitespace, commas,
                // and identifiers.
                if (p < text.len and text[p] == ':') p += 1;
                while (p < text.len) {
                    const c = text[p];
                    if (c == ' ' or c == '\t' or c == ',') {
                        p += 1;
                    } else if (char_flags.isIdentStart(c)) {
                        _ = parseIdent(text, &p);
                    } else return null;
                }
                return .{ .labels_d = {} };
            }
            return null;
        }

        fn skipSp(text: []const u8, p: *usize) void {
            while (p.* < text.len and (text[p.*] == ' ' or text[p.*] == '\t')) : (p.* += 1) {}
        }

        fn skipSpRequired(text: []const u8, p: *usize) bool {
            const before = p.*;
            skipSp(text, p);
            return p.* > before;
        }

        fn parseIdent(text: []const u8, p: *usize) ?[]const u8 {
            if (p.* >= text.len or !char_flags.isIdentStart(text[p.*])) return null;
            const start = p.*;
            p.* += 1;
            while (p.* < text.len and char_flags.isIdentCont(text[p.*])) : (p.* += 1) {}
            return text[start..p.*];
        }

        fn consumeWord(text: []const u8, p: *usize, word: []const u8) bool {
            if (p.* + word.len > text.len) return false;
            if (!std.mem.eql(u8, text[p.* .. p.* + word.len], word)) return false;
            const after = p.* + word.len;
            if (after < text.len and char_flags.isIdentCont(text[after])) return false;
            p.* = after;
            return true;
        }

        /// Find the throw label (if any) attached to the trivia range
        /// `[from, to)` in `self.tokens`. Only one throw per range is
        /// recognized; the spec disallows more.
        fn throwInRange(self: *const Self, from: usize, to: usize) ?[]const u8 {
            if (!config.recovery) return null;
            for (self.throw_dirs[0..self.throw_dir_count]) |td| {
                if (td.token_idx >= from and td.token_idx < to) return td.label;
            }
            return null;
        }

        /// Look up a catch directive for the given rule name.
        fn lookupCatch(self: *const Self, name: []const u8) ?CatchDir {
            if (!config.recovery) return null;
            for (self.catch_dirs[0..self.catch_dir_count]) |cd| {
                if (std.mem.eql(u8, cd.rule_name, name)) return cd;
            }
            return null;
        }

        pub fn getDiagnostics(self: *const Self) []const Diagnostic {
            return self.diagnostics.slice();
        }

        /// Literal slices accumulated from `#@ tokens "..." ...` directives.
        /// Empty when `config.recovery` is false or no such directives
        /// were seen. Slices reference the source the parser was
        /// initialised with.
        pub fn getTaggedTokens(self: *const Self) []const []const u8 {
            if (!config.recovery) return &.{};
            return self.tagged_tokens[0..self.tagged_token_count];
        }

        /// Definition <- Identifier LEFTARROW Expression
        fn parseDefinition(self: *Self) ParseError!Ast.Rule {
            if (self.peek().tag != .identifier) {
                self.fail(.identifier, self.peek());
                return error.SyntaxError;
            }
            const name = self.advance().lexeme(self.source);
            self.skipTrivia();

            if (self.peek().tag != .left_arrow) {
                self.fail(.left_arrow, self.peek());
                return error.SyntaxError;
            }
            _ = self.advance(); // consume <-
            self.skipTrivia();

            const node = try self.parseExpression();
            // After the expression we must be at eof or the start of the next
            // definition (`identifier <-`). Anything else (stray `)`, `/`
            // without a follow-up sequence, etc.) is a syntax error that
            // should abort this definition so the error-recovery path in
            // `parse()` can sync to the next rule.
            self.skipTrivia();
            if (!self.isDefinitionBoundary()) {
                self.fail(.expression, self.peek());
                return error.SyntaxError;
            }
            const wrapped = if (config.recovery) self.applyCatch(name, node) else node;
            return .{ .name = name, .node = wrapped, .incremental = false };
        }

        /// If a `#@ rule <name> catches ...` directive targets `name`,
        /// wrap `body` in an `lcatch` node. Otherwise return `body`
        /// unchanged.
        fn applyCatch(self: *Self, name: []const u8, body: Ast.Node) Ast.Node {
            const cd = self.lookupCatch(name) orelse return body;
            const handler_node: Ast.Node = if (cd.is_recover_missing)
                .{ .missing_label = cd.label }
            else
                .{ .rulename = cd.handler_rule_name };
            return .{ .lcatch = .{
                .label = cd.label,
                .body = self.nodes.addOne(body),
                .handler = self.nodes.addOne(handler_node),
            } };
        }

        /// True when the current position marks the end of a definition body:
        /// either eof, or an `identifier` immediately followed by `<-`.
        fn isDefinitionBoundary(self: *Self) bool {
            const tag = self.peek().tag;
            if (tag == .eof) return true;
            if (tag != .identifier) return false;
            return self.peekNextMeaningful() == .left_arrow;
        }

        /// Expression <- Sequence (SLASH Sequence)*
        fn parseExpression(self: *Self) ParseError!Ast.Node {
            var buf: [256]Ast.Node = undefined;
            var count: usize = 0;

            buf[0] = try self.parseSequence();
            count = 1;

            while (true) {
                self.skipTrivia();
                if (self.peek().tag != .slash) break;
                _ = self.advance();
                self.skipTrivia();
                buf[count] = try self.parseSequence();
                count += 1;
            }

            if (count == 1) return buf[0];
            return .{ .alternation = self.nodes.addSlice(buf[0..count]) };
        }

        /// Sequence <- Prefix*
        fn parseSequence(self: *Self) ParseError!Ast.Node {
            var buf: [256]Ast.Node = undefined;
            var count: usize = 0;
            // The trivia range immediately after the most recent prefix.
            // After the loop exits, this range is the trailing trivia of
            // the sequence, where a `#@ throw` directive may attach.
            var last_trivia_from: usize = self.pos;
            var last_trivia_to: usize = self.pos;

            while (self.isAtPrefix()) {
                buf[count] = try self.parsePrefix();
                count += 1;
                last_trivia_from = self.pos;
                self.skipTrivia();
                last_trivia_to = self.pos;
            }

            // If a #@ throw directive sits in the trailing trivia of the
            // last prefix, append a throw_label node to the sequence.
            // Mid-sequence directives (between prefixes that were
            // followed by more prefixes) are silently ignored - the
            // spec requires the directive to trail a *complete*
            // alternative.
            if (config.recovery) {
                if (count > 0) {
                    if (self.throwInRange(last_trivia_from, last_trivia_to)) |label| {
                        if (count >= buf.len) return error.Overflow;
                        buf[count] = .{ .throw_label = label };
                        count += 1;
                    }
                }
            }

            if (count == 0) {
                // Empty sequence -- matches empty string. Represent as empty concat.
                return .{ .concatenation = self.nodes.addSlice(buf[0..0]) };
            }
            if (count == 1) return buf[0];
            return .{ .concatenation = self.nodes.addSlice(buf[0..count]) };
        }

        /// Prefix <- (AND / NOT)? Suffix
        fn parsePrefix(self: *Self) ParseError!Ast.Node {
            const tag = self.peek().tag;
            if (tag == .@"and") {
                _ = self.advance();
                self.skipTrivia();
                const inner = try self.parseSuffix();
                return .{ .and_predicate = self.nodes.addOne(inner) };
            }
            if (tag == .not) {
                _ = self.advance();
                self.skipTrivia();
                const inner = try self.parseSuffix();
                return .{ .not_predicate = self.nodes.addOne(inner) };
            }
            return self.parseSuffix();
        }

        /// Suffix <- Primary (QUESTION / STAR / PLUS)?
        fn parseSuffix(self: *Self) ParseError!Ast.Node {
            const primary = try self.parsePrimary();

            return switch (self.peek().tag) {
                .question => {
                    _ = self.advance();
                    return .{ .repetition = .{ .min = 0, .max = 1, .element = self.nodes.addOne(primary) } };
                },
                .star => {
                    _ = self.advance();
                    return .{ .repetition = .{ .min = 0, .max = null, .element = self.nodes.addOne(primary) } };
                },
                .plus => {
                    _ = self.advance();
                    return .{ .repetition = .{ .min = 1, .max = null, .element = self.nodes.addOne(primary) } };
                },
                else => primary,
            };
        }

        /// Primary <- Identifier !LEFTARROW
        ///          / OPEN Expression CLOSE
        ///          / Literal / Class / DOT
        fn parsePrimary(self: *Self) ParseError!Ast.Node {
            return switch (self.peek().tag) {
                .identifier => {
                    // Only treat as reference if not followed by <-
                    const next = self.peekNextMeaningful();
                    if (next == .left_arrow) {
                        self.fail(.expression, self.peek());
                        return error.SyntaxError;
                    }
                    return .{ .rulename = self.advance().lexeme(self.source) };
                },
                .left_paren => {
                    _ = self.advance(); // consume (
                    self.skipTrivia();
                    const expr = try self.parseExpression();
                    self.skipTrivia();
                    if (self.peek().tag != .right_paren) {
                        self.fail(.right_paren, self.peek());
                        return error.SyntaxError;
                    }
                    _ = self.advance(); // consume )
                    return expr;
                },
                .literal => self.parseLiteral(),
                .char_class => self.parseCharClass(),
                .dot => {
                    _ = self.advance();
                    return .any;
                },
                else => {
                    self.fail(.expression, self.peek());
                    return error.SyntaxError;
                },
            };
        }

        fn parseLiteral(self: *Self) Ast.Node {
            const lex = self.advance().lexeme(self.source);
            // Strip surrounding quotes, decode escapes.
            const inner = lex[1 .. lex.len - 1];
            const decoded = self.decodeEscapes(inner);
            return .{ .char_val = .{ .value = decoded, .case_sensitive = true } };
        }

        fn decodeEscapes(self: *Self, raw: []const u8) []const u8 {
            // Fast path: no backslashes.
            if (std.mem.indexOfScalar(u8, raw, '\\') == null) return raw;

            const start = self.bytes.count;
            var i: usize = 0;
            while (i < raw.len) {
                if (raw[i] == '\\' and i + 1 < raw.len) {
                    i += 1;
                    const c = raw[i];
                    const decoded: u8 = switch (c) {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\'', '"', '[', ']', '\\' => c,
                        '0'...'2' => blk: {
                            // Possible octal: up to 3 digits.
                            const result = self.decodeOctal(raw, &i);
                            if (result) |val| break :blk val;
                            // Not a valid octal sequence, treat as literal.
                            _ = self.bytes.addOne('\\');
                            break :blk c;
                        },
                        else => blk: {
                            // Unknown escape -- keep backslash.
                            _ = self.bytes.addOne('\\');
                            break :blk c;
                        },
                    };
                    _ = self.bytes.addOne(decoded);
                    i += 1;
                } else {
                    _ = self.bytes.addOne(raw[i]);
                    i += 1;
                }
            }
            return self.bytes.items[start..self.bytes.count];
        }

        /// Try to decode an octal escape starting at raw[*i].
        /// *i points to the first octal digit.
        /// Returns the decoded byte if valid, null otherwise.
        /// On success, *i is advanced to the last digit (caller will do +1).
        fn decodeOctal(self: *Self, raw: []const u8, i: *usize) ?u8 {
            _ = self;
            const start = i.*;
            // Need at least 2 digits for a valid octal escape.
            if (start + 1 >= raw.len) return null;
            if (!isOctalDigit(raw[start + 1])) return null;

            // 3-digit octal (0-2, 0-7, 0-7)
            if (start + 2 < raw.len and isOctalDigit(raw[start + 2])) {
                const val = (@as(u16, raw[start] - '0') * 64) +
                    (@as(u16, raw[start + 1] - '0') * 8) +
                    @as(u16, raw[start + 2] - '0');
                if (val <= 255) {
                    i.* = start + 2; // advance past all 3 digits (caller does +1)
                    return @intCast(val);
                }
                return null;
            }

            // 2-digit octal
            const val = (raw[start] - '0') * 8 + (raw[start + 1] - '0');
            i.* = start + 1;
            return val;
        }

        fn isOctalDigit(c: u8) bool {
            return c >= '0' and c <= '7';
        }

        fn parseCharClass(self: *Self) Ast.Node {
            const lex = self.advance().lexeme(self.source);
            // Strip surrounding brackets: [content]
            const inner = lex[1 .. lex.len - 1];

            const start = self.ranges.count;
            var i: usize = 0;
            while (i < inner.len) {
                const lo = self.decodeClassChar(inner, &i) orelse break;
                // Check for range: char '-' char
                if (i + 1 < inner.len and inner[i] == '-') {
                    i += 1; // skip '-'
                    const hi = self.decodeClassChar(inner, &i) orelse {
                        // Malformed range -- treat '-' as literal.
                        _ = self.ranges.addOne(.{ .lo = lo, .hi = lo });
                        _ = self.ranges.addOne(.{ .lo = '-', .hi = '-' });
                        break;
                    };
                    _ = self.ranges.addOne(.{ .lo = lo, .hi = hi });
                } else {
                    _ = self.ranges.addOne(.{ .lo = lo, .hi = lo });
                }
            }

            return .{ .char_class = self.ranges.items[start..self.ranges.count] };
        }

        /// Decode one character from a character class body, advancing *i past it.
        fn decodeClassChar(self: *Self, raw: []const u8, i: *usize) ?u8 {
            if (i.* >= raw.len) return null;
            if (raw[i.*] == '\\' and i.* + 1 < raw.len) {
                i.* += 1;
                const c = raw[i.*];
                i.* += 1;
                return switch (c) {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\'', '"', '[', ']', '\\' => c,
                    '0'...'2' => {
                        // Try octal.
                        var oi = i.* - 1;
                        if (self.decodeOctal(raw, &oi)) |val| {
                            i.* = oi + 1;
                            return val;
                        }
                        return c;
                    },
                    else => c,
                };
            }
            const c = raw[i.*];
            i.* += 1;
            return c;
        }

        /// Can the current position start a Prefix?
        fn isAtPrefix(self: *Self) bool {
            return switch (self.peek().tag) {
                .@"and", .not => true,
                else => self.isAtPrimary(),
            };
        }

        /// Can the current position start a Primary?
        fn isAtPrimary(self: *Self) bool {
            return switch (self.peek().tag) {
                .left_paren, .literal, .char_class, .dot => true,
                .identifier => {
                    // An identifier followed by <- starts a new definition, not a reference.
                    const next = self.peekNextMeaningful();
                    return next != .left_arrow;
                },
                else => false,
            };
        }
    };
}

const Scanner = @import("Scanner.zig").Scanner;

fn parseSource(source: []const u8) Parser.ParseError!struct { parser: Parser, rules: []const Ast.Rule } {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    if (parser.diagnostics.count != 0) return error.SyntaxError;
    return .{ .parser = parser, .rules = rules };
}

test "simple definition" {
    const result = try parseSource("A <- B");
    try std.testing.expectEqual(1, result.rules.len);
    try std.testing.expectEqualStrings("A", result.rules[0].name);
    try std.testing.expectEqualStrings("B", result.rules[0].node.rulename);
}

test "ordered choice" {
    const result = try parseSource("A <- B / C / D");
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(3, alts.len);
    try std.testing.expectEqualStrings("B", alts[0].rulename);
    try std.testing.expectEqualStrings("C", alts[1].rulename);
    try std.testing.expectEqualStrings("D", alts[2].rulename);
}

test "sequence" {
    const result = try parseSource("A <- B C D");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(3, cat.len);
    try std.testing.expectEqualStrings("B", cat[0].rulename);
    try std.testing.expectEqualStrings("C", cat[1].rulename);
    try std.testing.expectEqualStrings("D", cat[2].rulename);
}

test "star suffix" {
    const result = try parseSource("A <- B*");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(null, rep.max);
    try std.testing.expectEqualStrings("B", rep.element.rulename);
}

test "plus suffix" {
    const result = try parseSource("A <- B+");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(1, rep.min);
    try std.testing.expectEqual(null, rep.max);
    try std.testing.expectEqualStrings("B", rep.element.rulename);
}

test "question suffix" {
    const result = try parseSource("A <- B?");
    const rep = result.rules[0].node.repetition;
    try std.testing.expectEqual(0, rep.min);
    try std.testing.expectEqual(1, rep.max.?);
    try std.testing.expectEqualStrings("B", rep.element.rulename);
}

test "and predicate" {
    const result = try parseSource("A <- &B");
    try std.testing.expectEqualStrings("B", result.rules[0].node.and_predicate.rulename);
}

test "not predicate" {
    const result = try parseSource("A <- !B");
    try std.testing.expectEqualStrings("B", result.rules[0].node.not_predicate.rulename);
}

test "dot wildcard" {
    const result = try parseSource("A <- .");
    try std.testing.expectEqual(.any, result.rules[0].node);
}

test "literal (single quotes)" {
    const result = try parseSource("A <- 'hello'");
    try std.testing.expectEqualStrings("hello", result.rules[0].node.char_val.value);
    try std.testing.expectEqual(true, result.rules[0].node.char_val.case_sensitive);
}

test "literal (double quotes)" {
    const result = try parseSource("A <- \"hello\"");
    try std.testing.expectEqualStrings("hello", result.rules[0].node.char_val.value);
}

test "character class simple" {
    const result = try parseSource("A <- [abc]");
    const ranges = result.rules[0].node.char_class;
    try std.testing.expectEqual(3, ranges.len);
    try std.testing.expectEqual('a', ranges[0].lo);
    try std.testing.expectEqual('a', ranges[0].hi);
    try std.testing.expectEqual('b', ranges[1].lo);
    try std.testing.expectEqual('c', ranges[2].lo);
}

test "character class range" {
    const result = try parseSource("A <- [a-z]");
    const ranges = result.rules[0].node.char_class;
    try std.testing.expectEqual(1, ranges.len);
    try std.testing.expectEqual('a', ranges[0].lo);
    try std.testing.expectEqual('z', ranges[0].hi);
}

test "character class mixed" {
    const result = try parseSource("A <- [a-zA-Z_]");
    const ranges = result.rules[0].node.char_class;
    try std.testing.expectEqual(3, ranges.len);
    try std.testing.expectEqual('a', ranges[0].lo);
    try std.testing.expectEqual('z', ranges[0].hi);
    try std.testing.expectEqual('A', ranges[1].lo);
    try std.testing.expectEqual('Z', ranges[1].hi);
    try std.testing.expectEqual('_', ranges[2].lo);
    try std.testing.expectEqual('_', ranges[2].hi);
}

test "grouped expression" {
    const result = try parseSource("A <- (B / C) D");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(2, cat.len);
    try std.testing.expectEqual(2, cat[0].alternation.len);
    try std.testing.expectEqualStrings("D", cat[1].rulename);
}

test "multiple definitions" {
    const result = try parseSource("A <- B\nC <- D");
    try std.testing.expectEqual(2, result.rules.len);
    try std.testing.expectEqualStrings("A", result.rules[0].name);
    try std.testing.expectEqualStrings("C", result.rules[1].name);
}

test "not predicate with dot (end of file)" {
    const result = try parseSource("EOF <- !.");
    const inner = result.rules[0].node.not_predicate.*;
    try std.testing.expectEqual(.any, inner);
}

test "complex: identifier with lookahead" {
    // Primary <- Identifier !LEFTARROW
    const result = try parseSource("P <- A !B C");
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(3, cat.len);
    try std.testing.expectEqualStrings("A", cat[0].rulename);
    try std.testing.expectEqualStrings("B", cat[1].not_predicate.rulename);
    try std.testing.expectEqualStrings("C", cat[2].rulename);
}

test "incremental is always false" {
    const result = try parseSource("A <- B");
    try std.testing.expectEqual(false, result.rules[0].incremental);
}

test "recovery: error in first definition, second parsed" {
    var scanner = Scanner.init("A <- )\nB <- C");
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, "A <- )\nB <- C");
    const rules = try parser.parse();
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("B", rules[0].name);
    try std.testing.expect(parser.getDiagnostics().len > 0);
}

test "recovery: missing left arrow" {
    var scanner = Scanner.init("A B\nC <- D");
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, "A B\nC <- D");
    const rules = try parser.parse();
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("C", rules[0].name);
}

test "multi-line alternation continuation" {
    const result = try parseSource(
        \\Primary <- Identifier
        \\         / Literal
        \\         / DOT
    );
    try std.testing.expectEqual(1, result.rules.len);
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(3, alts.len);
}

test "Class rule from PEG grammar" {
    const result = try parseSource("Class <- '[' (!']' Range)* ']' Spacing");
    try std.testing.expectEqual(1, result.rules.len);
    try std.testing.expectEqualStrings("Class", result.rules[0].name);
}

test "multi-rule PEG grammar subset" {
    const result = try parseSource(
        \\Identifier <- IdentStart IdentCont* Spacing
        \\IdentStart <- [a-zA-Z_]
        \\IdentCont  <- IdentStart / [0-9]
    );
    try std.testing.expectEqual(3, result.rules.len);
}

test "Literal rule with quotes and escapes" {
    const result = try parseSource(
        \\Literal <- ['] (!['] Char)* ['] Spacing
    );
    try std.testing.expectEqual(1, result.rules.len);
}

test "Char rule multi-line" {
    const result = try parseSource(
        \\Char <- '\\' [nrt'"\[\]\\]
        \\     / '\\' [0-2] [0-7] [0-7]
        \\     / '\\' [0-7] [0-7]?
        \\     / !'\\' .
    );
    try std.testing.expectEqual(1, result.rules.len);
}

const RecoveryParser = ParserWith(.{ .recovery = true });

fn parseRecoverySource(source: []const u8) RecoveryParser.ParseError!struct { parser: RecoveryParser, rules: []const Ast.Rule } {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = RecoveryParser.init(tokens, source);
    const rules = try parser.parse();
    if (parser.diagnostics.count != 0) return error.SyntaxError;
    return .{ .parser = parser, .rules = rules };
}

test "directive: #@ comments are plain comments when recovery=false" {
    // Default Parser does not recognize #@, so the AST is unchanged.
    const result = try parseSource(
        \\A <- 'a'   #@ throw missing_a
    );
    try std.testing.expectEqual(1, result.rules.len);
    // The body should be just a literal, no throw_label appended.
    try std.testing.expectEqualStrings("a", result.rules[0].node.char_val.value);
}

test "directive: #@ throw appends throw_label to alternative" {
    const result = try parseRecoverySource(
        \\A <- 'a' #@ throw missing_a
    );
    try std.testing.expectEqual(1, result.rules.len);
    // Body becomes a concatenation of [literal 'a', throw_label "missing_a"].
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(@as(usize, 2), cat.len);
    try std.testing.expectEqualStrings("a", cat[0].char_val.value);
    try std.testing.expectEqualStrings("missing_a", cat[1].throw_label);
}

test "directive: #@ throw on its own trailing line still attaches" {
    const result = try parseRecoverySource(
        \\A <- 'a'
        \\     #@ throw L
    );
    try std.testing.expectEqual(1, result.rules.len);
    const cat = result.rules[0].node.concatenation;
    try std.testing.expectEqual(@as(usize, 2), cat.len);
    try std.testing.expectEqualStrings("L", cat[1].throw_label);
}

test "directive: #@ throw attaches to specific alternative in /-list" {
    // The throw should attach to the SECOND alternative only.
    const result = try parseRecoverySource(
        \\A <- 'a'
        \\   / 'b' #@ throw L
        \\   / 'c'
    );
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(@as(usize, 3), alts.len);
    // alt 0: just 'a'
    try std.testing.expectEqualStrings("a", alts[0].char_val.value);
    // alt 1: 'b' then throw L
    const cat = alts[1].concatenation;
    try std.testing.expectEqual(@as(usize, 2), cat.len);
    try std.testing.expectEqualStrings("b", cat[0].char_val.value);
    try std.testing.expectEqualStrings("L", cat[1].throw_label);
    // alt 2: just 'c'
    try std.testing.expectEqualStrings("c", alts[2].char_val.value);
}

test "directive: #@ rule ... catches ... -> recover_missing wraps body in lcatch" {
    const result = try parseRecoverySource(
        \\#@ rule A catches L -> recover_missing
        \\A <- 'a' #@ throw L
    );
    try std.testing.expectEqual(1, result.rules.len);
    const lcatch = result.rules[0].node.lcatch;
    try std.testing.expectEqualStrings("L", lcatch.label);
    // handler is missing_label "L" (the recover_missing builtin form).
    try std.testing.expectEqualStrings("L", lcatch.handler.missing_label);
    // body is the original concatenation.
    const cat = lcatch.body.concatenation;
    try std.testing.expectEqual(@as(usize, 2), cat.len);
    try std.testing.expectEqualStrings("L", cat[1].throw_label);
}

test "directive: #@ rule ... catches ... -> rule_name wraps with rule handler" {
    const result = try parseRecoverySource(
        \\#@ rule A catches L -> recover
        \\A <- 'a' #@ throw L
        \\recover <- 'x'
    );
    try std.testing.expectEqual(2, result.rules.len);
    const a_rule = result.rules[0];
    try std.testing.expectEqualStrings("A", a_rule.name);
    const lcatch = a_rule.node.lcatch;
    try std.testing.expectEqualStrings("L", lcatch.label);
    // handler is rulename "recover".
    try std.testing.expectEqualStrings("recover", lcatch.handler.rulename);
}

test "directive: malformed directive emits diagnostic" {
    // `#@ throw` without a label is malformed; the rule still parses
    // (the AST is unaffected because the directive is dropped) but a
    // diagnostic surfaces so the user can tell their directive was
    // not applied.
    var scanner = Scanner.init("A <- 'a'   #@ trow missing_a");
    const tokens = scanner.scanTokens();
    var parser = RecoveryParser.init(tokens, "A <- 'a'   #@ trow missing_a");
    const rules = try parser.parse();
    try std.testing.expectEqual(1, rules.len);
    try std.testing.expectEqualStrings("a", rules[0].node.char_val.value);
    const diags = parser.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqual(Diagnostic.Expected.directive_malformed, diags[0].expected);
}

test "directive: plain # comments do not emit directive diagnostics" {
    // Only `#@`-prefixed comments are treated as directives. A bare
    // `# foo` comment must not raise a malformed-directive diagnostic.
    const result = try parseRecoverySource(
        \\# regular comment
        \\A <- 'a'
    );
    try std.testing.expectEqual(1, result.rules.len);
}

test "directive: #@ rule ... catches ... can sit anywhere" {
    // The catch directive may appear after the rule it targets.
    const result = try parseRecoverySource(
        \\A <- 'a' #@ throw L
        \\#@ rule A catches L -> recover_missing
    );
    try std.testing.expectEqual(1, result.rules.len);
    const lcatch = result.rules[0].node.lcatch;
    try std.testing.expectEqualStrings("L", lcatch.label);
    try std.testing.expect(lcatch.handler.* == .missing_label);
}

test "directive: #@ labels declarations are accepted silently" {
    // `#@ labels: ...` is recognized syntactically but not enforced.
    // Until enforcement exists, a clean grammar using the directive
    // must not raise a diagnostic - the only diagnostic surface is
    // hard errors, so flagging every labels line would treat correct
    // grammars as broken.
    var scanner = Scanner.init("#@ labels: a, b, c\nA <- 'a'");
    const tokens = scanner.scanTokens();
    var parser = RecoveryParser.init(tokens, "#@ labels: a, b, c\nA <- 'a'");
    const rules = try parser.parse();
    try std.testing.expectEqual(@as(usize, 1), rules.len);
    try std.testing.expectEqual(@as(usize, 0), parser.getDiagnostics().len);
}

test "directive: keyword glued to identifier reports malformed, not unsupported" {
    // `#@ labelsxyz` must not be interpreted as a labels directive: the
    // word-boundary check inside consumeWord makes it fall through to
    // `directive_malformed` so the user notices the typo.
    const src = "#@ labelsxyz\nA <- 'a'";
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = RecoveryParser.init(tokens, src);
    _ = try parser.parse();
    const diags = parser.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqual(Diagnostic.Expected.directive_malformed, diags[0].expected);
}

test "directive: throw keyword glued to identifier reports malformed" {
    // Mirror of the labels case: `#@ throwfoo bar` must be rejected as
    // malformed rather than treated as `throw` with a stray suffix.
    const src = "A <- 'a' #@ throwfoo bar";
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = RecoveryParser.init(tokens, src);
    _ = try parser.parse();
    const diags = parser.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqual(Diagnostic.Expected.directive_malformed, diags[0].expected);
}

test "directive: #@ labels followed by non-list garbage is malformed" {
    // Trailing content after `labels` must look like a (possibly empty)
    // ident list — anything else surfaces as `directive_malformed`.
    const src = "#@ labels @@!!\nA <- 'a'";
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = RecoveryParser.init(tokens, src);
    _ = try parser.parse();
    const diags = parser.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqual(Diagnostic.Expected.directive_malformed, diags[0].expected);
}

test "directive: throw overflow emits diagnostic" {
    // Configure a parser that only accepts a single throw directive,
    // then feed it two so the second triggers overflow.
    const TinyThrow = ParserWith(.{ .recovery = true, .max_throw_dirs = 1 });
    const src =
        \\A <- 'a' #@ throw L1
        \\B <- 'b' #@ throw L2
    ;
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = TinyThrow.init(tokens, src);
    _ = try parser.parse();
    const diags = parser.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqual(Diagnostic.Expected.directive_throw_overflow, diags[0].expected);
}

test "directive: catch overflow emits diagnostic" {
    const TinyCatch = ParserWith(.{ .recovery = true, .max_catch_dirs = 1 });
    const src =
        \\#@ rule A catches L -> recover_missing
        \\#@ rule B catches L -> recover_missing
        \\A <- 'a' #@ throw L
        \\B <- 'b' #@ throw L
    ;
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = TinyCatch.init(tokens, src);
    _ = try parser.parse();
    const diags = parser.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqual(Diagnostic.Expected.directive_catch_overflow, diags[0].expected);
}

test "directive: tokens collects literals across multiple directives" {
    const RP = ParserWith(.{ .recovery = true });
    const src =
        \\#@ tokens "function" "if"
        \\#@ tokens "+"
        \\Stmt <- "function" Ident
        \\Ident <- [a-z]+
    ;
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = RP.init(tokens, src);
    _ = try parser.parse();
    try std.testing.expectEqual(@as(usize, 0), parser.getDiagnostics().len);
    const tagged = parser.getTaggedTokens();
    try std.testing.expectEqual(@as(usize, 3), tagged.len);
    try std.testing.expectEqualStrings("function", tagged[0]);
    try std.testing.expectEqualStrings("if", tagged[1]);
    try std.testing.expectEqualStrings("+", tagged[2]);
}

test "directive: tokens with no entries is malformed" {
    const RP = ParserWith(.{ .recovery = true });
    const src =
        \\#@ tokens
        \\A <- "a"
    ;
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = RP.init(tokens, src);
    _ = try parser.parse();
    const diags = parser.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqual(Diagnostic.Expected.directive_malformed, diags[0].expected);
}

test "directive: field wraps the matching rule reference" {
    const RP = ParserWith(.{ .recovery = true });
    const src =
        \\#@ field Function name = Identifier
        \\Function <- "function" Identifier
        \\Identifier <- [a-z]+
    ;
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = RP.init(tokens, src);
    const rules = try parser.parse();
    try std.testing.expectEqual(@as(usize, 0), parser.getDiagnostics().len);

    // First rule is Function; its body is `concat("function", Identifier)`.
    // The second element should now be wrapped in `.field`.
    const body = rules[0].node;
    try std.testing.expect(body == .concatenation);
    const elems = body.concatenation;
    try std.testing.expectEqual(@as(usize, 2), elems.len);
    try std.testing.expect(elems[0] == .char_val);
    try std.testing.expect(elems[1] == .field);
    try std.testing.expectEqualStrings("name", elems[1].field.name);
    try std.testing.expectEqualStrings("Identifier", elems[1].field.body.rulename);
}

test "directive: field with ordinal selects Nth occurrence" {
    const RP = ParserWith(.{ .recovery = true });
    const src =
        \\#@ field Bin left = Expr#1
        \\#@ field Bin right = Expr#2
        \\Bin <- Expr "+" Expr
        \\Expr <- [a-z]
    ;
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = RP.init(tokens, src);
    const rules = try parser.parse();
    try std.testing.expectEqual(@as(usize, 0), parser.getDiagnostics().len);

    const elems = rules[0].node.concatenation;
    try std.testing.expectEqualStrings("left", elems[0].field.name);
    try std.testing.expectEqualStrings("right", elems[2].field.name);
    try std.testing.expect(elems[1] == .char_val); // "+" untouched
}

test "directive: field tags a quoted literal" {
    const RP = ParserWith(.{ .recovery = true });
    const src =
        \\#@ field Stmt keyword = "function"
        \\Stmt <- "function" [a-z]+
    ;
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = RP.init(tokens, src);
    const rules = try parser.parse();
    try std.testing.expectEqual(@as(usize, 0), parser.getDiagnostics().len);

    const elems = rules[0].node.concatenation;
    try std.testing.expect(elems[0] == .field);
    try std.testing.expectEqualStrings("keyword", elems[0].field.name);
    try std.testing.expectEqualStrings("function", elems[0].field.body.char_val.value);
}

test "directive: field with bad target reports not-found" {
    const RP = ParserWith(.{ .recovery = true });
    const src =
        \\#@ field Stmt missing = Nonexistent
        \\Stmt <- "x"
    ;
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = RP.init(tokens, src);
    _ = try parser.parse();
    const diags = parser.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqual(Diagnostic.Expected.directive_field_not_found, diags[0].expected);
}

test "directive: field overflow emits diagnostic" {
    const TinyField = ParserWith(.{ .recovery = true, .max_field_dirs = 1 });
    const src =
        \\#@ field A x = B
        \\#@ field A y = C
        \\A <- B C
        \\B <- "b"
        \\C <- "c"
    ;
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = TinyField.init(tokens, src);
    _ = try parser.parse();
    const diags = parser.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqual(Diagnostic.Expected.directive_field_overflow, diags[0].expected);
}

test "directive: tokens overflow emits diagnostic" {
    const TinyTokens = ParserWith(.{ .recovery = true, .max_tagged_tokens = 1 });
    const src =
        \\#@ tokens "a" "b"
        \\X <- "a"
    ;
    var scanner = Scanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = TinyTokens.init(tokens, src);
    _ = try parser.parse();
    const diags = parser.getDiagnostics();
    try std.testing.expectEqual(@as(usize, 1), diags.len);
    try std.testing.expectEqual(Diagnostic.Expected.directive_tokens_overflow, diags[0].expected);
}
