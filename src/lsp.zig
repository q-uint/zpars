//! zpars LSP server.
//!
//! A from-scratch Language Server Protocol implementation that reuses the
//! zpars scanner/parser infrastructure to provide diagnostics, semantic
//! tokens, and completion for ABNF, BNF, PEG, and CFG grammar files.
//!
//! Transport: JSON-RPC 2.0 over stdin/stdout with Content-Length framing.

const std = @import("std");
const zpars = @import("zpars");

const Allocator = std.mem.Allocator;
const Io = std.Io;

const Language = enum { abnf, bnf, peg, cfg };

fn detectLanguage(uri: []const u8) ?Language {
    if (std.mem.endsWith(u8, uri, ".abnf")) return .abnf;
    if (std.mem.endsWith(u8, uri, ".bnf")) return .bnf;
    if (std.mem.endsWith(u8, uri, ".peg")) return .peg;
    if (std.mem.endsWith(u8, uri, ".cfg")) return .cfg;
    return null;
}

// Semantic token tag maps (ported from editors/vsx/src/extension.ts).
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
    skip, // left_paren
    skip, // right_paren
    skip, // left_bracket
    skip, // right_bracket
    4, // slash → operator
    4, // star → operator
    4, // equals → operator
    4, // equals_slash → operator
    0, // rulename → type
    2, // number → number
    1, // char_val → string
    1, // char_val_ci → string
    1, // char_val_cs → string
    1, // prose_val → string
    2, // bin_val → number
    2, // dec_val → number
    2, // hex_val → number
    3, // comment → comment
    skip, // newline
    skip, // eof
    skip, // invalid
};

const bnf_tag_map = [_]i8{
    0, // rulename → type
    4, // definition → operator
    4, // pipe → operator
    1, // terminal → string
    skip, // newline
    skip, // eof
    skip, // invalid
};

const peg_tag_map = [_]i8{
    0, // identifier → type
    4, // left_arrow → operator
    4, // slash → operator
    4, // and → operator
    4, // not → operator
    4, // question → operator
    4, // star → operator
    4, // plus → operator
    skip, // left_paren
    skip, // right_paren
    4, // dot → operator
    1, // literal → string
    5, // char_class → regexp
    3, // comment → comment
    skip, // newline
    skip, // eof
    skip, // invalid
};

const cfg_tag_map = [_]i8{
    0, // identifier → type
    1, // string → string
    1, // string_cs → string
    1, // string_ci → string
    2, // hex_byte → number
    2, // hex_range → number
    4, // arrow → operator
    4, // pipe → operator
    skip, // newline
    skip, // eof
    skip, // invalid
};

const abnf_rulename_tag: u32 = 8;
const bnf_rulename_tag: u32 = 0;
const peg_rulename_tag: u32 = 0;
const cfg_rulename_tag: u32 = 0;

const Document = struct {
    text: []u8,
    lang: Language,
};

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
            .bool => |b| try self.writeAll(if (b) "true" else "false"),
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
};

const DiagInfo = struct {
    start: usize,
    len: usize,
    message: []const u8,
};

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
        try jw.writeAll("\"semanticTokensProvider\":{");
        try jw.writeAll("\"legend\":{");
        try jw.writeAll("\"tokenTypes\":[\"type\",\"string\",\"number\",\"comment\",\"operator\",\"regexp\"],");
        try jw.writeAll("\"tokenModifiers\":[]},");
        try jw.writeAll("\"full\":true},");
        try jw.writeAll("\"completionProvider\":{}},");

        // serverInfo
        try jw.writeAll("\"serverInfo\":{\"name\":\"zpars-lsp\",\"version\":\"0.1.0\"}");
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

        // Clear diagnostics.
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

        const diags: []const DiagInfo = switch (doc.lang) {
            .abnf => collectDiags(zpars.abnf.Scanner, zpars.abnf.Parser, doc.text, arena) catch &.{},
            .bnf => collectDiags(zpars.bnf.Scanner, zpars.bnf.Parser, doc.text, arena) catch &.{},
            .peg => collectDiags(zpars.peg.Scanner, zpars.peg.Parser, doc.text, arena) catch &.{},
            .cfg => collectDiags(zpars.cfg.Scanner, zpars.cfg.Parser, doc.text, arena) catch &.{},
        };

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);

        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":");
        try jw.writeString(uri);
        try jw.writeAll(",\"diagnostics\":[");

        for (diags, 0..) |d, i| {
            if (i > 0) try jw.writeByte(',');
            const start_pos = offsetToPosition(doc.text, d.start);
            const end_pos = offsetToPosition(doc.text, @min(d.start + @max(d.len, 1), doc.text.len));

            try jw.writeAll("{\"range\":{\"start\":{\"line\":");
            try jw.writeInt(start_pos.line);
            try jw.writeAll(",\"character\":");
            try jw.writeInt(start_pos.character);
            try jw.writeAll("},\"end\":{\"line\":");
            try jw.writeInt(end_pos.line);
            try jw.writeAll(",\"character\":");
            try jw.writeInt(end_pos.character);
            try jw.writeAll("}},\"severity\":1,\"source\":\"zpars\",\"message\":");
            try jw.writeString(d.message);
            try jw.writeByte('}');
        }

        try jw.writeAll("]}}");
        try self.sendJson(&buf);
    }

    fn collectDiags(
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
        const raw_diags = parser.getDiagnostics();

        const result = try arena.alloc(DiagInfo, raw_diags.len);
        for (raw_diags, 0..) |d, i| {
            const lexeme = if (d.found_len > 0)
                source[d.found_start..@min(d.found_start + d.found_len, source.len)]
            else
                "";

            const message = if (lexeme.len > 0)
                try std.fmt.allocPrint(arena, "expected {s}, found '{s}'", .{ @tagName(d.expected), lexeme })
            else
                try std.fmt.allocPrint(arena, "expected {s}", .{@tagName(d.expected)});

            result[i] = .{ .start = d.found_start, .len = d.found_len, .message = message };
        }
        return result;
    }

    fn handleSemanticTokens(self: *Server, id: std.json.Value, params: std.json.Value) !void {
        const td = jsonGet(params, "textDocument") orelse return;
        const uri = jsonGetString(td, "uri") orelse return;

        const doc = self.documents.get(uri) orelse {
            try self.respondNull(id);
            return;
        };

        const data = switch (doc.lang) {
            .abnf => encodeSemanticTokensGeneric(zpars.abnf.Scanner, &abnf_tag_map, doc.text),
            .bnf => encodeSemanticTokensGeneric(zpars.bnf.Scanner, &bnf_tag_map, doc.text),
            .peg => encodeSemanticTokensGeneric(zpars.peg.Scanner, &peg_tag_map, doc.text),
            .cfg => encodeSemanticTokensGeneric(zpars.cfg.Scanner, &cfg_tag_map, doc.text),
        };

        var buf: std.ArrayListUnmanaged(u8) = .empty;
        defer buf.deinit(self.allocator);
        const jw = JsonWriter.init(&buf, self.allocator);

        try jw.writeAll("{\"jsonrpc\":\"2.0\",\"id\":");
        try jw.writeJsonValue(id);
        try jw.writeAll(",\"result\":{\"data\":[");

        var first = true;
        for (data) |v| {
            if (!first) try jw.writeByte(',');
            first = false;
            try jw.writeInt(v);
        }

        try jw.writeAll("]}}");
        try self.sendJson(&buf);
    }

    fn encodeSemanticTokensGeneric(
        comptime Scanner: type,
        tag_map: []const i8,
        source: []const u8,
    ) []const u32 {
        if (source.len == 0) return &.{};

        var scanner = Scanner.init(source);
        const tokens = scanner.scanTokens();

        // We use a static buffer — max 4096 tokens × 5 = 20480 u32s.
        const max_entries = Scanner.max_tokens * 5;
        const S = struct {
            var data: [max_entries]u32 = undefined;
        };

        var di: usize = 0;
        var prev_line: u32 = 0;
        var prev_char: u32 = 0;

        for (tokens) |tok| {
            const tag_idx = @intFromEnum(tok.tag);
            if (tag_idx >= tag_map.len or tag_map[tag_idx] < 0) continue;

            const pos = offsetToPosition(source, tok.start);
            const delta_line = pos.line - prev_line;
            const delta_char = if (delta_line == 0) pos.character - prev_char else pos.character;

            S.data[di] = delta_line;
            S.data[di + 1] = delta_char;
            S.data[di + 2] = @intCast(tok.len);
            S.data[di + 3] = @intCast(tag_map[tag_idx]);
            S.data[di + 4] = 0;
            di += 5;

            prev_line = pos.line;
            prev_char = pos.character;
        }

        return S.data[0..di];
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

        switch (doc.lang) {
            .abnf => {
                var s = zpars.abnf.Scanner.init(doc.text);
                try writeCompletionItems(jw, s.scanTokens(), abnf_rulename_tag, doc.text);
            },
            .bnf => {
                var s = zpars.bnf.Scanner.init(doc.text);
                try writeCompletionItems(jw, s.scanTokens(), bnf_rulename_tag, doc.text);
            },
            .peg => {
                var s = zpars.peg.Scanner.init(doc.text);
                try writeCompletionItems(jw, s.scanTokens(), peg_rulename_tag, doc.text);
            },
            .cfg => {
                var s = zpars.cfg.Scanner.init(doc.text);
                try writeCompletionItems(jw, s.scanTokens(), cfg_rulename_tag, doc.text);
            },
        }

        try jw.writeAll("]}}");
        try self.sendJson(&buf);
    }

    fn writeCompletionItems(jw: JsonWriter, tokens: anytype, rulename_tag: u32, source: []const u8) !void {
        // Collect unique names — use a simple linear scan since we have at most 4096 tokens.
        var names: [256][]const u8 = undefined;
        var name_count: usize = 0;

        for (tokens) |tok| {
            if (@intFromEnum(tok.tag) == rulename_tag) {
                const name = source[tok.start .. tok.start + tok.len];
                var found = false;
                for (names[0..name_count]) |existing| {
                    if (std.mem.eql(u8, existing, name)) {
                        found = true;
                        break;
                    }
                }
                if (!found and name_count < names.len) {
                    names[name_count] = name;
                    name_count += 1;
                }
            }
        }

        for (names[0..name_count], 0..) |name, i| {
            if (i > 0) try jw.writeByte(',');
            try jw.writeAll("{\"label\":");
            try jw.writeString(name);
            try jw.writeAll(",\"kind\":6,\"detail\":\"rule\"}");
        }
    }
};

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
