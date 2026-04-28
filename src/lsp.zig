//! zpars LSP server.
//!
//! A from-scratch Language Server Protocol implementation that reuses the
//! zpars scanner / parser / validator / formatter / matcher infrastructure
//! to provide rich editor support for ABNF, BNF, PEG, CFG, S-expression,
//! and ERE grammar files.
//!
//! Capabilities:
//!   - Diagnostics (parse errors + validator semantic checks)
//!   - Semantic tokens
//!   - Completion (rule names already in the document)
//!   - Document symbols (one per rule)
//!   - Go-to-definition (rule reference -> defining rule)
//!   - Find references (all occurrences of a rule name)
//!   - Hover (formatted body of the rule under the cursor)
//!   - Document formatting (whole-document)
//!   - Custom requests:
//!       zpars/match  - run a rule against input text, return match span
//!       zpars/tree   - PEG-only capture-tree JSON
//!
//! Transport: JSON-RPC 2.0 over stdin/stdout with Content-Length framing.

const std = @import("std");
const zpars = @import("zpars");

const Allocator = std.mem.Allocator;
const Io = std.Io;

const Language = enum { abnf, bnf, peg, cfg, sexp, ere };

fn detectLanguage(uri: []const u8) ?Language {
    if (std.mem.endsWith(u8, uri, ".abnf")) return .abnf;
    if (std.mem.endsWith(u8, uri, ".bnf")) return .bnf;
    if (std.mem.endsWith(u8, uri, ".peg")) return .peg;
    if (std.mem.endsWith(u8, uri, ".cfg")) return .cfg;
    if (std.mem.endsWith(u8, uri, ".sexp")) return .sexp;
    if (std.mem.endsWith(u8, uri, ".ere")) return .ere;
    return null;
}

// Semantic token tag maps.
//
// Legend indices:
//   0: type        (rule names / identifiers)
//   1: string      (literals, prose values)
//   2: number      (numeric values)
//   3: comment
//   4: operator    (definition symbols, arrows)
//   5: regexp      (character classes)

const skip: i8 = -1;

const abnf_tag_map = [_]i8{
    skip, skip, skip, skip, // parens / brackets
    4, 4, 4, 4, // slash, star, equals, equals_slash
    0, // rulename
    2, // number
    1, 1, 1, 1, // char_val variants + prose_val
    2, 2, 2, // bin/dec/hex_val
    3, // comment
    skip, skip, skip, // newline, eof, invalid
};

const bnf_tag_map = [_]i8{
    0, // rulename
    4, // definition
    4, // pipe
    1, // terminal
    skip, skip, skip, // newline, eof, invalid
};

const peg_tag_map = [_]i8{
    0, // identifier
    4, 4, 4, 4, 4, 4, 4, // left_arrow, slash, and, not, question, star, plus
    skip, skip, // left_paren, right_paren
    4, // dot
    1, // literal
    5, // char_class
    3, // comment
    skip, skip, skip, // newline, eof, invalid
};

const cfg_tag_map = [_]i8{
    0, // identifier
    1, 1, 1, // string, string_cs, string_ci
    2, 2, // hex_byte, hex_range
    4, 4, // arrow, pipe
    skip, skip, skip, // newline, eof, invalid
};

// SEXP Tag enum order:
//   0: lparen, 1: rparen, 2: lbracket, 3: rbracket,
//   4: lbrace, 5: rbrace, 6: verbatim, 7: quoted_string,
//   8: sexp_token, 9: hexadecimal, 10: base64, 11: decimal,
//   12: whitespace, 13: eof, 14: invalid
const sexp_tag_map = [_]i8{
    skip, skip, skip, skip, skip, skip, // brackets
    1, 1, // verbatim, quoted_string
    0, // sexp_token
    2,    2,    2, // hexadecimal, base64, decimal
    skip, skip, skip,
};

// ERE Tag enum order:
//   0: char, 1: dot, 2: caret, 3: dollar,
//   4: star, 5: plus, 6: question, 7: lbrace, 8: rbrace, 9: comma,
//   10: number, 11: left_paren, 12: right_paren, 13: pipe,
//   14: bracket_expr, 15: eof, 16: invalid
const ere_tag_map = [_]i8{
    1, // char -> string
    4, 4, 4, // dot, caret, dollar -> operator
    4, 4, 4, // *, +, ?
    skip, skip, skip, // {, }, ,
    2, // number
    skip, skip, // parens
    4, // pipe
    5, // bracket_expr -> regexp
    skip,
    skip,
};

fn tagMapFor(lang: Language) []const i8 {
    return switch (lang) {
        .abnf => &abnf_tag_map,
        .bnf => &bnf_tag_map,
        .peg => &peg_tag_map,
        .cfg => &cfg_tag_map,
        .sexp => &sexp_tag_map,
        .ere => &ere_tag_map,
    };
}

fn nameTagOf(comptime Tag: type) ?u32 {
    inline for (@typeInfo(Tag).@"enum".fields) |f| {
        if (std.mem.eql(u8, f.name, "rulename")) return f.value;
        if (std.mem.eql(u8, f.name, "identifier")) return f.value;
    }
    return null;
}

fn isTrivia(comptime Tag: type, tag_value: u32) bool {
    inline for (@typeInfo(Tag).@"enum".fields) |f| {
        if (f.value == tag_value) {
            return std.mem.eql(u8, f.name, "newline") or std.mem.eql(u8, f.name, "comment");
        }
    }
    return false;
}

/// True if `name` (the @tagName of a token tag) is one of the
/// definition operators for this language.
fn isDefOp(name: []const u8) bool {
    return std.mem.eql(u8, name, "equals") or
        std.mem.eql(u8, name, "equals_slash") or
        std.mem.eql(u8, name, "definition") or
        std.mem.eql(u8, name, "left_arrow") or
        std.mem.eql(u8, name, "arrow");
}

const JsonWriter = struct {
    buf: *std.ArrayListUnmanaged(u8),
    allocator: Allocator,

    fn init(buf: *std.ArrayListUnmanaged(u8), allocator: Allocator) JsonWriter {
        return .{ .buf = buf, .allocator = allocator };
    }

    fn writeByte(self: JsonWriter, byte: u8) !void {
        try self.buf.append(self.allocator, byte);
    }

    fn writeAll(self: JsonWriter, bytes: []const u8) !void {
        try self.buf.appendSlice(self.allocator, bytes);
    }

    fn writeInt(self: JsonWriter, value: anytype) !void {
        var num_buf: [20]u8 = undefined;
        const s = std.fmt.bufPrint(&num_buf, "{d}", .{value}) catch unreachable;
        try self.writeAll(s);
    }

    fn writeBool(self: JsonWriter, b: bool) !void {
        try self.writeAll(if (b) "true" else "false");
    }

    fn writeString(self: JsonWriter, s: []const u8) !void {
        try self.writeByte('"');
        for (s) |c| {
            switch (c) {
                '"' => try self.writeAll("\\\""),
                '\\' => try self.writeAll("\\\\"),
                '\n' => try self.writeAll("\\n"),
                '\r' => try self.writeAll("\\r"),
                '\t' => try self.writeAll("\\t"),
                else => {
                    if (c < 0x20) {
                        var esc_buf: [6]u8 = undefined;
                        const esc = std.fmt.bufPrint(&esc_buf, "\\u{x:0>4}", .{c}) catch unreachable;
                        try self.writeAll(esc);
                    } else {
                        try self.writeByte(c);
                    }
                },
            }
        }
        try self.writeByte('"');
    }

    fn writeJsonValue(self: JsonWriter, val: std.json.Value) !void {
        switch (val) {
            .null => try self.writeAll("null"),
            .bool => |b| try self.writeBool(b),
            .integer => |i| try self.writeInt(i),
            .float => |f| {
                var float_buf: [32]u8 = undefined;
                const s = std.fmt.bufPrint(&float_buf, "{d}", .{f}) catch unreachable;
                try self.writeAll(s);
            },
            .string => |s| try self.writeString(s),
            .number_string => |s| try self.writeAll(s),
            .array => |arr| {
                try self.writeByte('[');
                for (arr.items, 0..) |item, idx| {
                    if (idx > 0) try self.writeByte(',');
                    try self.writeJsonValue(item);
                }
                try self.writeByte(']');
            },
            .object => |obj| {
                try self.writeByte('{');
                var first = true;
                var it = obj.iterator();
                while (it.next()) |entry| {
                    if (!first) try self.writeByte(',');
                    first = false;
                    try self.writeString(entry.key_ptr.*);
                    try self.writeByte(':');
                    try self.writeJsonValue(entry.value_ptr.*);
                }
                try self.writeByte('}');
            },
        }
    }

    fn writeRange(self: JsonWriter, source: []const u8, start: usize, end: usize) !void {
        const start_pos = offsetToPosition(source, start);
        const end_pos = offsetToPosition(source, end);
        try self.writeAll("{\"start\":{\"line\":");
        try self.writeInt(start_pos.line);
        try self.writeAll(",\"character\":");
        try self.writeInt(start_pos.character);
        try self.writeAll("},\"end\":{\"line\":");
        try self.writeInt(end_pos.line);
        try self.writeAll(",\"character\":");
        try self.writeInt(end_pos.character);
        try self.writeAll("}}");
    }
};

const RuleDef = struct {
    name: []const u8,
    /// Range of the name token at the def site.
    name_start: usize,
    name_len: usize,
    /// Inclusive range covering the whole definition
    /// (name through end of body, before the next rule).
    full_start: usize,
    full_end: usize,
};

const RuleRef = struct {
    name: []const u8,
    start: usize,
    len: usize,
    /// True if this token is at a definition site.
    is_def: bool,
};

const max_rules_in_doc = 512;
const max_refs_in_doc = 4096;

const RuleIndex = struct {
    defs: []const RuleDef,
    refs: []const RuleRef,
};

/// Collect rule definitions and references for a given source. Returns
/// slices into static storage to avoid allocation; subsequent calls
/// invalidate previously returned slices.
fn collectRules(lang: Language, source: []const u8) RuleIndex {
    return switch (lang) {
        .abnf => collectRulesGeneric(zpars.abnf.Scanner, source),
        .bnf => collectRulesGeneric(zpars.bnf.Scanner, source),
        .peg => collectRulesGeneric(zpars.peg.Scanner, source),
        .cfg => collectRulesGeneric(zpars.cfg.Scanner, source),
        .sexp, .ere => .{ .defs = &.{}, .refs = &.{} },
    };
}

fn collectRulesGeneric(comptime Scanner: type, source: []const u8) RuleIndex {
    const Tok = @TypeOf(@as(Scanner, undefined).tokens[0]);
    const Tag = Tok.Tag;

    const S = struct {
        var defs: [max_rules_in_doc]RuleDef = undefined;
        var refs: [max_refs_in_doc]RuleRef = undefined;
    };

    const name_tag = nameTagOf(Tag) orelse return .{ .defs = &.{}, .refs = &.{} };

    if (source.len == 0) return .{ .defs = &.{}, .refs = &.{} };

    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();

    var def_count: usize = 0;
    var ref_count: usize = 0;

    var is_def: [Scanner.max_tokens]bool = undefined;
    for (0..tokens.len) |i| is_def[i] = false;

    for (tokens, 0..) |tok, i| {
        if (@intFromEnum(tok.tag) != name_tag) continue;
        var j = i + 1;
        while (j < tokens.len) : (j += 1) {
            if (isTrivia(Tag, @intFromEnum(tokens[j].tag))) continue;
            break;
        }
        if (j >= tokens.len) continue;
        if (isDefOp(@tagName(tokens[j].tag))) is_def[i] = true;
    }

    // Second pass: build the defs and refs lists in one go.
    var current_def_idx: ?usize = null;
    for (tokens, 0..) |tok, i| {
        if (@intFromEnum(tok.tag) != name_tag) continue;
        const lex = source[tok.start .. tok.start + tok.len];

        if (ref_count < max_refs_in_doc) {
            S.refs[ref_count] = .{
                .name = lex,
                .start = tok.start,
                .len = tok.len,
                .is_def = is_def[i],
            };
            ref_count += 1;
        }

        if (is_def[i] and def_count < max_rules_in_doc) {
            // Close the previous def's body range.
            if (current_def_idx) |idx| {
                S.defs[idx].full_end = tok.start;
            }
            S.defs[def_count] = .{
                .name = lex,
                .name_start = tok.start,
                .name_len = tok.len,
                .full_start = tok.start,
                .full_end = source.len,
            };
            current_def_idx = def_count;
            def_count += 1;
        }
    }

    return .{ .defs = S.defs[0..def_count], .refs = S.refs[0..ref_count] };
}

const Document = struct {
    text: []u8,
    lang: Language,
};

const DiagInfo = struct {
    start: usize,
    len: usize,
    severity: u8, // 1=Error 2=Warning 3=Information 4=Hint
    message: []const u8,
};

const RuleNamePos = struct {
    name: []const u8,
    start: usize,
    len: usize,
};

fn validatorSeverity(kind: zpars.Validator.Validation.Kind) u8 {
    return switch (kind) {
        .duplicate_rule, .undefined_rule, .unproductive_rule, .left_recursive_rule, .zero_width_loop => 1,
        .unused_rule => 2,
    };
}

fn validatorMessage(arena: Allocator, v: zpars.Validator.Validation) ![]const u8 {
    return switch (v.kind) {
        .duplicate_rule => std.fmt.allocPrint(arena, "duplicate definition of rule '{s}'", .{v.rule_name}),
        .undefined_rule => std.fmt.allocPrint(arena, "undefined rule '{s}'", .{v.ref_name orelse v.rule_name}),
        .unused_rule => std.fmt.allocPrint(arena, "rule '{s}' is never referenced", .{v.rule_name}),
        .unproductive_rule => std.fmt.allocPrint(arena, "rule '{s}' is unproductive (cannot derive any string)", .{v.rule_name}),
        .left_recursive_rule => std.fmt.allocPrint(arena, "rule '{s}' is left-recursive", .{v.rule_name}),
        .zero_width_loop => std.fmt.allocPrint(arena, "rule '{s}' contains a zero-width loop", .{v.rule_name}),
    };
}

const SrcRange = struct { start: usize, len: usize };

fn locateRuleName(defs: []const RuleDef, name: []const u8) SrcRange {
    for (defs) |d| {
        if (std.ascii.eqlIgnoreCase(d.name, name)) {
            return .{ .start = d.name_start, .len = d.name_len };
        }
    }
    return .{ .start = 0, .len = 0 };
}

fn locateRuleRef(refs: []const RuleRef, name: []const u8) ?SrcRange {
    for (refs) |r| {
        if (r.is_def) continue;
        if (std.ascii.eqlIgnoreCase(r.name, name)) {
            return .{ .start = r.start, .len = r.len };
        }
    }
    return null;
}

fn collectDiagnostics(arena: Allocator, doc: Document) ![]const DiagInfo {
    return switch (doc.lang) {
        .abnf => try collectDiagsValidated(zpars.abnf.Scanner, zpars.abnf.Parser, doc.text, .abnf, arena),
        .peg => try collectDiagsValidated(zpars.peg.Scanner, zpars.peg.Parser, doc.text, .peg, arena),
        .bnf => try collectDiagsValidated(zpars.bnf.Scanner, zpars.bnf.Parser, doc.text, .bnf, arena),
        .ere => try collectDiagsParseOnly(zpars.ere.Scanner, zpars.ere.Parser, doc.text, arena),
        .cfg => try collectDiagsParseOnly(zpars.cfg.Scanner, zpars.cfg.Parser, doc.text, arena),
        .sexp => try collectDiagsSexp(doc.text, arena),
    };
}

fn formatParseDiagMessage(
    arena: Allocator,
    expected_name: []const u8,
    found_lexeme: []const u8,
) ![]const u8 {
    if (found_lexeme.len > 0) {
        return std.fmt.allocPrint(arena, "expected {s}, found '{s}'", .{ expected_name, found_lexeme });
    } else {
        return std.fmt.allocPrint(arena, "expected {s}", .{expected_name});
    }
}

fn collectDiagsParseOnly(
    comptime Scanner: type,
    comptime Parser: type,
    source: []const u8,
    arena: Allocator,
) ![]const DiagInfo {
    if (source.len == 0) return &.{};

    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    _ = parser.parse() catch {};
    const raw = parser.getDiagnostics();

    const out = try arena.alloc(DiagInfo, raw.len);
    for (raw, 0..) |d, i| {
        const lex = if (d.found_len > 0)
            source[d.found_start..@min(d.found_start + d.found_len, source.len)]
        else
            "";
        out[i] = .{
            .start = d.found_start,
            .len = d.found_len,
            .severity = 1,
            .message = try formatParseDiagMessage(arena, @tagName(d.expected), lex),
        };
    }
    return out;
}

fn collectDiagsValidated(
    comptime Scanner: type,
    comptime Parser: type,
    source: []const u8,
    lang: Language,
    arena: Allocator,
) ![]const DiagInfo {
    if (source.len == 0) return &.{};

    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = parser.parse() catch null;
    const parse_diags = parser.getDiagnostics();

    var out: std.ArrayList(DiagInfo) = .empty;

    for (parse_diags) |d| {
        const lex = if (d.found_len > 0)
            source[d.found_start..@min(d.found_start + d.found_len, source.len)]
        else
            "";
        try out.append(arena, .{
            .start = d.found_start,
            .len = d.found_len,
            .severity = 1,
            .message = try formatParseDiagMessage(arena, @tagName(d.expected), lex),
        });
    }

    // Run validator only if parse succeeded with at least one rule.
    if (rules) |ruleset| {
        if (ruleset.len > 0) {
            var validator = zpars.Validator.init(arena, ruleset);
            _ = validator.validate() catch {
                return try out.toOwnedSlice(arena);
            };
            defer validator.freeMerges();
            const index = collectRules(lang, source);
            for (validator.diagnostics.items) |v| {
                var final_range = locateRuleName(index.defs, v.rule_name);
                if (v.kind == .undefined_rule) {
                    if (v.ref_name) |rname| {
                        if (locateRuleRef(index.refs, rname)) |r| final_range = r;
                    }
                }
                try out.append(arena, .{
                    .start = final_range.start,
                    .len = if (final_range.len == 0) 1 else final_range.len,
                    .severity = validatorSeverity(v.kind),
                    .message = try validatorMessage(arena, v),
                });
            }
        }
    }

    return try out.toOwnedSlice(arena);
}

fn collectDiagsSexp(source: []const u8, arena: Allocator) ![]const DiagInfo {
    if (source.len == 0) return &.{};

    var scanner = zpars.sexp.Scanner.init(source);
    var tokens: std.ArrayList(zpars.sexp.Token.Token) = .empty;
    while (true) {
        const tok = scanner.next();
        try tokens.append(arena, tok);
        if (tok.tag == .eof) break;
        if (tokens.items.len > 8192) break;
    }
    const toks = tokens.items;

    var out: std.ArrayList(DiagInfo) = .empty;
    const Tag = zpars.sexp.Token.Tag;

    for (toks) |tok| {
        if (tok.tag == .invalid) {
            const lex = if (tok.len > 0) source[tok.start .. tok.start + tok.len] else "";
            const msg = if (lex.len > 0)
                try std.fmt.allocPrint(arena, "invalid token '{s}'", .{lex})
            else
                try arena.dupe(u8, "invalid token");
            try out.append(arena, .{
                .start = tok.start,
                .len = tok.len,
                .severity = 1,
                .message = msg,
            });
        }
    }

    // Bracket balance.
    var stack: std.ArrayList(struct { tag: Tag, start: usize, len: usize }) = .empty;
    for (toks) |tok| {
        if (tok.tag == .lparen or tok.tag == .lbracket) {
            try stack.append(arena, .{ .tag = tok.tag, .start = tok.start, .len = tok.len });
        } else if (tok.tag == .rparen or tok.tag == .rbracket) {
            const expected_open: Tag = if (tok.tag == .rparen) .lparen else .lbracket;
            if (stack.items.len > 0 and stack.items[stack.items.len - 1].tag == expected_open) {
                _ = stack.pop();
            } else {
                const lex = source[tok.start .. tok.start + tok.len];
                try out.append(arena, .{
                    .start = tok.start,
                    .len = tok.len,
                    .severity = 1,
                    .message = try std.fmt.allocPrint(arena, "unmatched '{s}'", .{lex}),
                });
            }
        }
    }
    while (stack.items.len > 0) {
        const open = stack.pop().?;
        const lex = source[open.start .. open.start + open.len];
        try out.append(arena, .{
            .start = open.start,
            .len = open.len,
            .severity = 1,
            .message = try std.fmt.allocPrint(arena, "unmatched '{s}'", .{lex}),
        });
    }

    return try out.toOwnedSlice(arena);
}

const Server = struct {
    allocator: Allocator,
    io: Io,
    documents: std.StringHashMap(Document),
    stdout: Io.File,

    fn init(allocator: Allocator, io: Io) Server {
        return .{
            .allocator = allocator,
            .io = io,
            .documents = std.StringHashMap(Document).init(allocator),
            .stdout = Io.File.stdout(),
        };
    }

    fn deinit(self: *Server) void {
        var it = self.documents.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*.text);
        }
        self.documents.deinit();
    }

    fn send(self: *Server, json: []const u8) !void {
        var header_buf: [64]u8 = undefined;
        const header = std.fmt.bufPrint(&header_buf, "Content-Length: {d}\r\n\r\n", .{json.len}) catch unreachable;
        try self.stdout.writeStreamingAll(self.io, header);
        try self.stdout.writeStreamingAll(self.io, json);
    }

    fn sendJson(self: *Server, buf: *std.ArrayListUnmanaged(u8)) !void {
        try self.send(buf.items);
    }

    fn handleMessage(self: *Server, msg: std.json.Value) !void {
        const obj = switch (msg) {
            .object => |o| o,
            else => return,
        };

        const method_val = obj.get("method") orelse return;
        const method = switch (method_val) {
            .string => |s| s,
            else => return,
        };

        const id = obj.get("id");
        const params = obj.get("params") orelse .null;

        if (std.mem.eql(u8, method, "initialize")) {
            try self.handleInitialize(id.?);
        } else if (std.mem.eql(u8, method, "initialized")) {
            // no-op
        } else if (std.mem.eql(u8, method, "shutdown")) {
            try self.respondNull(id.?);
        } else if (std.mem.eql(u8, method, "exit")) {
            std.process.exit(0);
        } else if (std.mem.eql(u8, method, "textDocument/didOpen")) {
            try self.handleDidOpen(params);
        } else if (std.mem.eql(u8, method, "textDocument/didChange")) {
            try self.handleDidChange(params);
        } else if (std.mem.eql(u8, method, "textDocument/didClose")) {
            try self.handleDidClose(params);
        } else if (std.mem.eql(u8, method, "textDocument/semanticTokens/full")) {
            try self.handleSemanticTokens(id.?, params);
        } else if (std.mem.eql(u8, method, "textDocument/completion")) {
            try self.handleCompletion(id.?, params);
        } else if (std.mem.eql(u8, method, "textDocument/definition")) {
            try self.handleDefinition(id.?, params);
        } else if (std.mem.eql(u8, method, "textDocument/references")) {
            try self.handleReferences(id.?, params);
        } else if (std.mem.eql(u8, method, "textDocument/hover")) {
            try self.handleHover(id.?, params);
        } else if (std.mem.eql(u8, method, "textDocument/documentSymbol")) {
            try self.handleDocumentSymbol(id.?, params);
        } else if (std.mem.eql(u8, method, "textDocument/formatting")) {
            try self.handleFormatting(id.?, params);
        } else if (std.mem.eql(u8, method, "zpars/match")) {
            try self.handleMatch(id.?, params);
        } else if (std.mem.eql(u8, method, "zpars/tree")) {
            try self.handleTree(id.?, params);
        } else if (id != null) {
            try self.respondNull(id.?);
        }
    }

    fn respondNull(self: *Server, id: std.json.Value) !void {
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);
        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try jw.writeJsonValue(id);
        try jw.writeAll(",\"result\":null}");
        try self.sendJson(&buf);
    }

    fn respondError(self: *Server, id: std.json.Value, code: i32, message: []const u8) !void {
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);
        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try jw.writeJsonValue(id);
        try jw.writeAll(",\"error\":{\"code\":");
        try jw.writeInt(code);
        try jw.writeAll(",\"message\":");
        try jw.writeString(message);
        try jw.writeAll("}}");
        try self.sendJson(&buf);
    }

    fn handleInitialize(self: *Server, id: std.json.Value) !void {
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);

        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try jw.writeJsonValue(id);
        try jw.writeAll(",\"result\":{");

        // capabilities
        try jw.writeAll("\"capabilities\":{");
        try jw.writeAll("\"textDocumentSync\":{\"openClose\":true,\"change\":1},");
        try jw.writeAll("\"semanticTokensProvider\":{\"legend\":{\"tokenTypes\":[\"type\",\"string\",\"number\",\"comment\",\"operator\",\"regexp\"],\"tokenModifiers\":[\"declaration\"]},\"full\":true},");
        try jw.writeAll("\"completionProvider\":{},");
        try jw.writeAll("\"definitionProvider\":true,");
        try jw.writeAll("\"referencesProvider\":true,");
        try jw.writeAll("\"hoverProvider\":true,");
        try jw.writeAll("\"documentSymbolProvider\":true,");
        try jw.writeAll("\"documentFormattingProvider\":true");
        try jw.writeAll("},");

        // serverInfo
        try jw.writeAll("\"serverInfo\":{\"name\":\"zpars-lsp\",\"version\":\"0.2.0\"}");
        try jw.writeAll("}}");

        try self.sendJson(&buf);
    }

    fn handleDidOpen(self: *Server, params: std.json.Value) !void {
        const td = jsonGet(params, "textDocument") orelse return;
        const uri = jsonGetString(td, "uri") orelse return;
        const text = jsonGetString(td, "text") orelse return;

        const lang = detectLanguage(uri) orelse return;

        const uri_copy = try self.allocator.dupe(u8, uri);
        errdefer self.allocator.free(uri_copy);

        const text_copy = try self.allocator.dupe(u8, text);
        errdefer self.allocator.free(text_copy);

        const gop = try self.documents.getOrPut(uri_copy);
        if (gop.found_existing) {
            self.allocator.free(gop.value_ptr.*.text);
            self.allocator.free(uri_copy);
        }
        gop.value_ptr.* = .{ .text = text_copy, .lang = lang };

        const key = if (gop.found_existing) gop.key_ptr.* else uri_copy;
        try self.publishDiagnostics(key, gop.value_ptr.*);
    }

    fn handleDidChange(self: *Server, params: std.json.Value) !void {
        const td = jsonGet(params, "textDocument") orelse return;
        const uri = jsonGetString(td, "uri") orelse return;

        const changes = switch (jsonGet(params, "contentChanges") orelse return) {
            .array => |a| a,
            else => return,
        };
        if (changes.items.len == 0) return;

        const new_text = jsonGetString(changes.items[changes.items.len - 1], "text") orelse return;

        const entry = self.documents.getEntry(uri) orelse return;

        const text_copy = try self.allocator.dupe(u8, new_text);
        self.allocator.free(entry.value_ptr.*.text);
        entry.value_ptr.*.text = text_copy;

        try self.publishDiagnostics(entry.key_ptr.*, entry.value_ptr.*);
    }

    fn handleDidClose(self: *Server, params: std.json.Value) !void {
        const td = jsonGet(params, "textDocument") orelse return;
        const uri = jsonGetString(td, "uri") orelse return;

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);
        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":");
        try jw.writeString(uri);
        try jw.writeAll(",\"diagnostics\":[]}}");
        try self.sendJson(&buf);

        const kv = self.documents.fetchRemove(uri) orelse return;
        self.allocator.free(kv.key);
        self.allocator.free(kv.value.text);
    }

    fn publishDiagnostics(self: *Server, uri: []const u8, doc: Document) !void {
        var arena_alloc = std.heap.ArenaAllocator.init(self.allocator);
        defer arena_alloc.deinit();
        const arena = arena_alloc.allocator();

        const diags = collectDiagnostics(arena, doc) catch &.{};

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);

        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":");
        try jw.writeString(uri);
        try jw.writeAll(",\"diagnostics\":[");

        for (diags, 0..) |d, i| {
            if (i > 0) try jw.writeByte(',');
            try jw.writeAll("{\"range\":");
            try jw.writeRange(doc.text, d.start, @min(d.start + @max(d.len, 1), doc.text.len));
            try jw.writeAll(",\"severity\":");
            try jw.writeInt(d.severity);
            try jw.writeAll(",\"source\":\"zpars\",\"message\":");
            try jw.writeString(d.message);
            try jw.writeByte('}');
        }

        try jw.writeAll("]}}");
        try self.sendJson(&buf);
    }

    fn handleSemanticTokens(self: *Server, id: std.json.Value, params: std.json.Value) !void {
        const td = jsonGet(params, "textDocument") orelse return;
        const uri = jsonGetString(td, "uri") orelse return;

        const doc = self.documents.get(uri) orelse {
            try self.respondNull(id);
            return;
        };

        const data = encodeSemanticTokens(doc.lang, doc.text);

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);

        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try jw.writeJsonValue(id);
        try jw.writeAll(",\"result\":{\"data\":[");

        for (data, 0..) |v, i| {
            if (i > 0) try jw.writeByte(',');
            try jw.writeInt(v);
        }

        try jw.writeAll("]}}");
        try self.sendJson(&buf);
    }

    fn handleCompletion(self: *Server, id: std.json.Value, params: std.json.Value) !void {
        const td = jsonGet(params, "textDocument") orelse return;
        const uri = jsonGetString(td, "uri") orelse return;

        const doc = self.documents.get(uri) orelse {
            try self.respondNull(id);
            return;
        };

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);

        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try jw.writeJsonValue(id);
        try jw.writeAll(",\"result\":{\"items\":[");

        const index = collectRules(doc.lang, doc.text);
        var first = true;
        for (index.defs) |d| {
            if (!first) try jw.writeByte(',');
            first = false;
            try jw.writeAll("{\"label\":");
            try jw.writeString(d.name);
            try jw.writeAll(",\"kind\":6,\"detail\":\"rule\"}");
        }

        try jw.writeAll("]}}");
        try self.sendJson(&buf);
    }

    fn handleDefinition(self: *Server, id: std.json.Value, params: std.json.Value) !void {
        const td = jsonGet(params, "textDocument") orelse return;
        const uri = jsonGetString(td, "uri") orelse return;
        const doc = self.documents.get(uri) orelse {
            try self.respondNull(id);
            return;
        };

        const pos = jsonGetPosition(jsonGet(params, "position") orelse {
            try self.respondNull(id);
            return;
        }) orelse {
            try self.respondNull(id);
            return;
        };
        const offset = positionToOffset(doc.text, pos);

        const index = collectRules(doc.lang, doc.text);
        const ref = findRefAtOffset(index.refs, offset) orelse {
            try self.respondNull(id);
            return;
        };

        // Resolve to a def site.
        for (index.defs) |d| {
            if (std.ascii.eqlIgnoreCase(d.name, ref.name)) {
                var buf: std.ArrayListUnmanaged(u8) = .empty;
                defer buf.deinit(self.allocator);
                const jw = JsonWriter.init(&buf, self.allocator);
                try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
                try jw.writeJsonValue(id);
                try jw.writeAll(",\"result\":{\"uri\":");
                try jw.writeString(uri);
                try jw.writeAll(",\"range\":");
                try jw.writeRange(doc.text, d.name_start, d.name_start + d.name_len);
                try jw.writeAll("}}");
                try self.sendJson(&buf);
                return;
            }
        }

        try self.respondNull(id);
    }

    fn handleReferences(self: *Server, id: std.json.Value, params: std.json.Value) !void {
        const td = jsonGet(params, "textDocument") orelse return;
        const uri = jsonGetString(td, "uri") orelse return;
        const doc = self.documents.get(uri) orelse {
            try self.respondNull(id);
            return;
        };

        const pos = jsonGetPosition(jsonGet(params, "position") orelse {
            try self.respondNull(id);
            return;
        }) orelse {
            try self.respondNull(id);
            return;
        };
        const offset = positionToOffset(doc.text, pos);

        // includeDeclaration default = true.
        var include_declaration = true;
        if (jsonGet(params, "context")) |ctx| {
            if (jsonGet(ctx, "includeDeclaration")) |v| {
                if (v == .bool) include_declaration = v.bool;
            }
        }

        const index = collectRules(doc.lang, doc.text);
        const at = findRefAtOffset(index.refs, offset) orelse {
            try self.respondNull(id);
            return;
        };

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);
        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try jw.writeJsonValue(id);
        try jw.writeAll(",\"result\":[");

        var first = true;
        for (index.refs) |r| {
            if (!std.ascii.eqlIgnoreCase(r.name, at.name)) continue;
            if (r.is_def and !include_declaration) continue;
            if (!first) try jw.writeByte(',');
            first = false;
            try jw.writeAll("{\"uri\":");
            try jw.writeString(uri);
            try jw.writeAll(",\"range\":");
            try jw.writeRange(doc.text, r.start, r.start + r.len);
            try jw.writeByte('}');
        }

        try jw.writeAll("]}");
        try self.sendJson(&buf);
    }

    fn handleHover(self: *Server, id: std.json.Value, params: std.json.Value) !void {
        const td = jsonGet(params, "textDocument") orelse return;
        const uri = jsonGetString(td, "uri") orelse return;
        const doc = self.documents.get(uri) orelse {
            try self.respondNull(id);
            return;
        };

        const pos = jsonGetPosition(jsonGet(params, "position") orelse {
            try self.respondNull(id);
            return;
        }) orelse {
            try self.respondNull(id);
            return;
        };
        const offset = positionToOffset(doc.text, pos);

        const index = collectRules(doc.lang, doc.text);
        const at = findRefAtOffset(index.refs, offset) orelse {
            try self.respondNull(id);
            return;
        };

        for (index.defs) |d| {
            if (!std.ascii.eqlIgnoreCase(d.name, at.name)) continue;
            const slice = std.mem.trim(u8, doc.text[d.full_start..d.full_end], " \t\r\n");

            var md: std.ArrayListUnmanaged(u8) = .empty;
            defer md.deinit(self.allocator);
            try md.appendSlice(self.allocator, "```");
            try md.appendSlice(self.allocator, @tagName(doc.lang));
            try md.append(self.allocator, '\n');
            try md.appendSlice(self.allocator, slice);
            try md.appendSlice(self.allocator, "\n```");

            var buf: std.ArrayListUnmanaged(u8) = .empty;
            defer buf.deinit(self.allocator);
            const jw = JsonWriter.init(&buf, self.allocator);
            try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
            try jw.writeJsonValue(id);
            try jw.writeAll(",\"result\":{\"contents\":{\"kind\":\"markdown\",\"value\":");
            try jw.writeString(md.items);
            try jw.writeAll("},\"range\":");
            try jw.writeRange(doc.text, at.start, at.start + at.len);
            try jw.writeAll("}}");
            try self.sendJson(&buf);
            return;
        }

        try self.respondNull(id);
    }

    fn handleDocumentSymbol(self: *Server, id: std.json.Value, params: std.json.Value) !void {
        const td = jsonGet(params, "textDocument") orelse return;
        const uri = jsonGetString(td, "uri") orelse return;
        const doc = self.documents.get(uri) orelse {
            try self.respondNull(id);
            return;
        };

        const index = collectRules(doc.lang, doc.text);

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);
        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try jw.writeJsonValue(id);
        try jw.writeAll(",\"result\":[");

        for (index.defs, 0..) |d, i| {
            if (i > 0) try jw.writeByte(',');
            try jw.writeAll("{\"name\":");
            try jw.writeString(d.name);
            // SymbolKind.Function = 12; close enough for "rule".
            try jw.writeAll(",\"kind\":12,\"range\":");
            try jw.writeRange(doc.text, d.full_start, d.full_end);
            try jw.writeAll(",\"selectionRange\":");
            try jw.writeRange(doc.text, d.name_start, d.name_start + d.name_len);
            try jw.writeByte('}');
        }

        try jw.writeAll("]}");
        try self.sendJson(&buf);
    }

    fn handleFormatting(self: *Server, id: std.json.Value, params: std.json.Value) !void {
        const td = jsonGet(params, "textDocument") orelse return;
        const uri = jsonGetString(td, "uri") orelse return;
        const doc = self.documents.get(uri) orelse {
            try self.respondNull(id);
            return;
        };

        var arena_alloc = std.heap.ArenaAllocator.init(self.allocator);
        defer arena_alloc.deinit();
        const arena = arena_alloc.allocator();

        const formatted = formatDocument(arena, doc) catch null;
        if (formatted == null) {
            try self.respondNull(id);
            return;
        }

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);
        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try jw.writeJsonValue(id);
        try jw.writeAll(",\"result\":[{\"range\":");
        try jw.writeRange(doc.text, 0, doc.text.len);
        try jw.writeAll(",\"newText\":");
        try jw.writeString(formatted.?);
        try jw.writeAll("}]}");
        try self.sendJson(&buf);
    }

    fn handleMatch(self: *Server, id: std.json.Value, params: std.json.Value) !void {
        const uri = jsonGetString(params, "uri") orelse {
            try self.respondError(id, -32602, "missing 'uri' parameter");
            return;
        };
        const rule_name = jsonGetString(params, "rule") orelse {
            try self.respondError(id, -32602, "missing 'rule' parameter");
            return;
        };
        const input = jsonGetString(params, "input") orelse "";

        const doc = self.documents.get(uri) orelse {
            try self.respondError(id, -32602, "unknown document");
            return;
        };

        var arena_alloc = std.heap.ArenaAllocator.init(self.allocator);
        defer arena_alloc.deinit();
        const arena = arena_alloc.allocator();

        const result = runMatch(arena, doc, rule_name, input) catch {
            try self.respondError(id, -32603, "match failed");
            return;
        };

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);
        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try jw.writeJsonValue(id);
        try jw.writeAll(",\"result\":{\"matched\":");
        try jw.writeBool(result.matched);
        if (result.matched) {
            try jw.writeAll(",\"value\":");
            try jw.writeString(result.value);
            try jw.writeAll(",\"rest\":");
            try jw.writeString(result.rest);
        }
        try jw.writeAll("}}");
        try self.sendJson(&buf);
    }

    fn handleTree(self: *Server, id: std.json.Value, params: std.json.Value) !void {
        const uri = jsonGetString(params, "uri") orelse {
            try self.respondError(id, -32602, "missing 'uri' parameter");
            return;
        };
        const input = jsonGetString(params, "input") orelse "";

        const doc = self.documents.get(uri) orelse {
            try self.respondError(id, -32602, "unknown document");
            return;
        };

        if (doc.lang != .peg) {
            try self.respondError(id, -32602, "tree is PEG-only");
            return;
        }

        var arena_alloc = std.heap.ArenaAllocator.init(self.allocator);
        defer arena_alloc.deinit();
        const arena = arena_alloc.allocator();

        const json = runTree(arena, doc.text, input) catch {
            try self.respondError(id, -32603, "tree failed");
            return;
        };

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);
        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try jw.writeJsonValue(id);
        try jw.writeAll(",\"result\":{\"json\":");
        if (json) |j| {
            try jw.writeString(j);
        } else {
            try jw.writeString("");
        }
        try jw.writeAll("}}");
        try self.sendJson(&buf);
    }
};

fn encodeSemanticTokens(lang: Language, source: []const u8) []const u32 {
    return switch (lang) {
        .abnf => encodeSemanticTokensGeneric(zpars.abnf.Scanner, &abnf_tag_map, source, true),
        .bnf => encodeSemanticTokensGeneric(zpars.bnf.Scanner, &bnf_tag_map, source, true),
        .peg => encodeSemanticTokensGeneric(zpars.peg.Scanner, &peg_tag_map, source, true),
        .cfg => encodeSemanticTokensGeneric(zpars.cfg.Scanner, &cfg_tag_map, source, true),
        .ere => encodeSemanticTokensGeneric(zpars.ere.Scanner, &ere_tag_map, source, false),
        .sexp => encodeSemanticTokensSexp(source),
    };
}

fn encodeSemanticTokensGeneric(
    comptime Scanner: type,
    tag_map: []const i8,
    source: []const u8,
    comptime supports_def_modifier: bool,
) []const u32 {
    if (source.len == 0) return &.{};

    const max_entries = Scanner.max_tokens * 5;
    const S = struct {
        var data: [max_entries]u32 = undefined;
    };

    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();

    const Tok = @TypeOf(tokens[0]);
    const Tag = Tok.Tag;
    const name_tag = if (supports_def_modifier) nameTagOf(Tag) else null;

    var is_def: [Scanner.max_tokens]bool = undefined;
    if (name_tag) |nt| {
        for (0..tokens.len) |i| is_def[i] = false;
        for (tokens, 0..) |tok, i| {
            if (@intFromEnum(tok.tag) != nt) continue;
            var j = i + 1;
            while (j < tokens.len) : (j += 1) {
                if (isTrivia(Tag, @intFromEnum(tokens[j].tag))) continue;
                break;
            }
            if (j >= tokens.len) continue;
            const next_name = @tagName(tokens[j].tag);
            if (std.mem.eql(u8, next_name, "equals") or
                std.mem.eql(u8, next_name, "equals_slash") or
                std.mem.eql(u8, next_name, "definition") or
                std.mem.eql(u8, next_name, "left_arrow") or
                std.mem.eql(u8, next_name, "arrow"))
            {
                is_def[i] = true;
            }
        }
    }

    var di: usize = 0;
    var prev_line: u32 = 0;
    var prev_char: u32 = 0;

    for (tokens, 0..) |tok, i| {
        const tag_idx = @intFromEnum(tok.tag);
        if (tag_idx >= tag_map.len or tag_map[tag_idx] < 0) continue;

        const pos = offsetToPosition(source, tok.start);
        const delta_line = pos.line - prev_line;
        const delta_char = if (delta_line == 0) pos.character - prev_char else pos.character;

        S.data[di] = delta_line;
        S.data[di + 1] = delta_char;
        S.data[di + 2] = @intCast(tok.len);
        S.data[di + 3] = @intCast(tag_map[tag_idx]);
        var modifiers: u32 = 0;
        if (name_tag) |nt| {
            if (tag_idx == nt and is_def[i]) modifiers = 1; // declaration
        }
        S.data[di + 4] = modifiers;
        di += 5;

        prev_line = pos.line;
        prev_char = pos.character;
    }

    return S.data[0..di];
}

fn encodeSemanticTokensSexp(source: []const u8) []const u32 {
    if (source.len == 0) return &.{};

    const max_tokens_sexp = 4096;
    const S = struct {
        var data: [max_tokens_sexp * 5]u32 = undefined;
    };

    var scanner = zpars.sexp.Scanner.init(source);
    var di: usize = 0;
    var prev_line: u32 = 0;
    var prev_char: u32 = 0;
    var emitted: usize = 0;

    while (emitted < max_tokens_sexp) {
        const tok = scanner.next();
        if (tok.tag == .eof) break;

        const tag_idx = @intFromEnum(tok.tag);
        if (tag_idx < sexp_tag_map.len and sexp_tag_map[tag_idx] >= 0) {
            const pos = offsetToPosition(source, tok.start);
            const delta_line = pos.line - prev_line;
            const delta_char = if (delta_line == 0) pos.character - prev_char else pos.character;

            S.data[di] = delta_line;
            S.data[di + 1] = delta_char;
            S.data[di + 2] = @intCast(tok.len);
            S.data[di + 3] = @intCast(sexp_tag_map[tag_idx]);
            S.data[di + 4] = 0;
            di += 5;

            prev_line = pos.line;
            prev_char = pos.character;
        }
        emitted += 1;
    }

    return S.data[0..di];
}

fn formatDocument(arena: Allocator, doc: Document) ![]const u8 {
    var aw: Io.Writer.Allocating = .init(arena);
    defer aw.deinit();

    switch (doc.lang) {
        .abnf => try formatGrammarWithTokens(zpars.abnf.Scanner, zpars.abnf.Parser, zpars.abnf.Formatter, doc.text, &aw.writer),
        .peg => try formatGrammarWithTokens(zpars.peg.Scanner, zpars.peg.Parser, zpars.peg.Formatter, doc.text, &aw.writer),
        .bnf => try formatGrammarPlain(zpars.bnf.Scanner, zpars.bnf.Parser, zpars.bnf.Formatter, doc.text, &aw.writer),
        .ere => try formatEre(doc.text, &aw.writer),
        .cfg, .sexp => return error.NotImplemented,
    }

    return arena.dupe(u8, aw.writer.buffered());
}

fn formatGrammarWithTokens(
    comptime Scanner: type,
    comptime Parser: type,
    comptime Formatter: type,
    source: []const u8,
    writer: anytype,
) !void {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = parser.parse() catch return error.ParseFailed;
    if (rules.len == 0) return error.ParseFailed;
    try Formatter.formatGrammar(rules, tokens, source, writer);
}

fn formatGrammarPlain(
    comptime Scanner: type,
    comptime Parser: type,
    comptime Formatter: type,
    source: []const u8,
    writer: anytype,
) !void {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = parser.parse() catch return error.ParseFailed;
    if (rules.len == 0) return error.ParseFailed;
    try Formatter.formatGrammar(rules, writer);
}

fn formatEre(source: []const u8, writer: anytype) !void {
    var scanner = zpars.ere.Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = zpars.ere.Parser.init(tokens, source);
    const rules = parser.parse() catch return error.ParseFailed;
    if (rules.len == 0) return error.ParseFailed;
    try zpars.ere.Formatter.formatRule(rules[0], writer);
}

const MatchResult = struct {
    matched: bool,
    value: []const u8 = "",
    rest: []const u8 = "",
};

fn runMatch(arena: Allocator, doc: Document, rule: []const u8, input: []const u8) !MatchResult {
    return switch (doc.lang) {
        .abnf => try runMatchGeneric(zpars.abnf.Scanner, zpars.abnf.Parser, doc.text, rule, input, arena),
        .bnf => try runMatchGeneric(zpars.bnf.Scanner, zpars.bnf.Parser, doc.text, rule, input, arena),
        .peg => try runMatchGeneric(zpars.peg.Scanner, zpars.peg.Parser, doc.text, rule, input, arena),
        .ere => try runMatchGeneric(zpars.ere.Scanner, zpars.ere.Parser, doc.text, rule, input, arena),
        .cfg, .sexp => .{ .matched = false },
    };
}

fn runMatchGeneric(
    comptime Scanner: type,
    comptime Parser: type,
    grammar: []const u8,
    rule_name: []const u8,
    input: []const u8,
    arena: Allocator,
) !MatchResult {
    var scanner = Scanner.init(grammar);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, grammar);
    const rules = parser.parse() catch return MatchResult{ .matched = false };

    var validator = zpars.Validator.init(arena, rules);
    const merged = validator.validate() catch return MatchResult{ .matched = false };
    defer validator.freeMerges();

    var matcher = try zpars.Matcher.init(arena, merged);
    defer matcher.deinit();

    const r = matcher.match(rule_name, input) orelse return MatchResult{ .matched = false };
    return .{ .matched = true, .value = r.value, .rest = r.rest };
}

fn runTree(arena: Allocator, grammar: []const u8, input: []const u8) !?[]const u8 {
    if (grammar.len == 0) return null;

    const RecoveryPegParser = zpars.peg.ParserWith(.{ .recovery = true });

    var scanner = zpars.peg.Scanner.init(grammar);
    const tokens = scanner.scanTokens();
    var parser = RecoveryPegParser.init(tokens, grammar);
    const rules = parser.parse() catch return null;
    if (rules.len == 0) return null;

    var compiler = zpars.vm.Compiler.compileOpts(rules, .{ .rules_as_captures = true }) catch return null;

    const EventVm = zpars.vm.VmWith(.{ .capture_events = true });
    var vm = EventVm.initEvents(
        arena,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        input,
    );
    defer vm.deinit();

    var aw: Io.Writer.Allocating = .init(arena);
    defer aw.deinit();

    const exec_result = vm.execute() catch return null;
    if (exec_result == null) return "";

    var captured = vm.buildCaptureTree(arena) catch return null;
    defer captured.deinit();

    const max_names = 256;
    var rule_names_buf: [max_names][]const u8 = undefined;
    const rule_names = rule_names_buf[0..compiler.rule_count];
    for (0..compiler.rule_count) |i| rule_names[i] = compiler.getRuleName(@intCast(i));

    var label_names_buf: [max_names][]const u8 = undefined;
    const label_names = label_names_buf[0..compiler.label_count];
    for (0..compiler.label_count) |i| label_names[i] = compiler.getLabelName(@intCast(i));

    captured.writeJson(&aw.writer, .{ .rules = rule_names, .labels = label_names }) catch return null;

    return try arena.dupe(u8, aw.writer.buffered());
}

const Position = struct {
    line: u32,
    character: u32,
};

fn offsetToPosition(source: []const u8, offset: usize) Position {
    var line: u32 = 0;
    var col: u32 = 0;
    for (source[0..@min(offset, source.len)]) |c| {
        if (c == '\n') {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    return .{ .line = line, .character = col };
}

fn positionToOffset(source: []const u8, pos: Position) usize {
    var line: u32 = 0;
    var col: u32 = 0;
    for (source, 0..) |c, i| {
        if (line == pos.line and col == pos.character) return i;
        if (c == '\n') {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    return source.len;
}

fn findRefAtOffset(refs: []const RuleRef, offset: usize) ?RuleRef {
    for (refs) |r| {
        if (offset >= r.start and offset <= r.start + r.len) return r;
    }
    return null;
}

fn jsonGet(val: std.json.Value, key: []const u8) ?std.json.Value {
    return switch (val) {
        .object => |obj| obj.get(key),
        else => null,
    };
}

fn jsonGetString(val: std.json.Value, key: []const u8) ?[]const u8 {
    const v = jsonGet(val, key) orelse return null;
    return switch (v) {
        .string => |s| s,
        else => null,
    };
}

fn jsonGetInt(val: std.json.Value, key: []const u8) ?i64 {
    const v = jsonGet(val, key) orelse return null;
    return switch (v) {
        .integer => |i| i,
        else => null,
    };
}

fn jsonGetPosition(val: std.json.Value) ?Position {
    const line = jsonGetInt(val, "line") orelse return null;
    const character = jsonGetInt(val, "character") orelse return null;
    return .{ .line = @intCast(line), .character = @intCast(character) };
}

fn readMessage(stdin: *Io.Reader, allocator: Allocator) !?std.json.Parsed(std.json.Value) {
    var content_length: ?usize = null;
    var header_buf: [256]u8 = undefined;
    var header_pos: usize = 0;

    while (true) {
        const b = stdin.takeByte() catch |err| switch (err) {
            error.EndOfStream => return null,
            else => return err,
        };

        if (b == '\n') {
            const line = header_buf[0..header_pos];
            const trimmed = if (line.len > 0 and line[line.len - 1] == '\r')
                line[0 .. line.len - 1]
            else
                line;

            if (trimmed.len == 0) break;

            const prefix = "Content-Length: ";
            if (trimmed.len > prefix.len and std.ascii.startsWithIgnoreCase(trimmed, prefix)) {
                content_length = std.fmt.parseInt(usize, trimmed[prefix.len..], 10) catch null;
            }

            header_pos = 0;
        } else {
            if (header_pos < header_buf.len) {
                header_buf[header_pos] = b;
                header_pos += 1;
            }
        }
    }

    const len = content_length orelse return null;
    const body = try allocator.alloc(u8, len);
    defer allocator.free(body);

    stdin.readSliceAll(body) catch |err| switch (err) {
        error.EndOfStream => return null,
        else => return err,
    };

    return try std.json.parseFromSlice(std.json.Value, allocator, body, .{
        .allocate = .alloc_always,
    });
}

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const allocator = init.gpa;

    var server = Server.init(allocator, io);
    defer server.deinit();

    var stdin_buf: [4096]u8 = undefined;
    var stdin_reader = Io.File.stdin().reader(io, &stdin_buf);
    const stdin = &stdin_reader.interface;

    while (true) {
        var parsed = readMessage(stdin, allocator) catch continue orelse break;
        defer parsed.deinit();
        server.handleMessage(parsed.value) catch continue;
    }
}
