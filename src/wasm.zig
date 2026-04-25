//! WASM FFI layer for zpars scanners.
//!
//! Exports a single `analyze` function that accepts a language tag and
//! pointer+length to source text in WASM linear memory. Returns a pointer
//! to a result buffer containing serialized tokens and diagnostics.
//!
//! Result format (little-endian):
//!   bytes 0..3  - token count (u32)
//!   bytes 4..7  - diagnostic count (u32)
//!   then for each token (12 bytes):
//!     bytes 0..3  - tag (u32)
//!     bytes 4..7  - start offset (u32)
//!     bytes 8..11 - length (u32)
//!   then for each diagnostic (16 bytes):
//!     bytes 0..3  - start offset (u32)
//!     bytes 4..7  - length (u32)
//!     bytes 8..11 - message offset (u32, relative to message data start)
//!     bytes 12..15 - message length (u32)
//!   then: concatenated message strings

const std = @import("std");
const root = @import("root.zig");

const allocator = std.heap.wasm_allocator;

export fn alloc(len: usize) ?[*]u8 {
    const buf = allocator.alloc(u8, len) catch return null;
    return buf.ptr;
}

export fn free(ptr: [*]u8, len: usize) void {
    allocator.free(ptr[0..len]);
}

const Language = enum(u8) {
    abnf = 0,
    bnf = 1,
    peg = 2,
    cfg = 3,
    sexp = 4,
    ere = 5,
};

fn analyzeGeneric(comptime Scanner: type, comptime Parser: type, source_ptr: [*]const u8, source_len: usize) ?[*]const u8 {
    if (source_len == 0) return null;
    const source = source_ptr[0..source_len];

    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();

    var parser = Parser.init(tokens, source);
    _ = parser.parse() catch {};
    const diags = parser.getDiagnostics();

    // Format diagnostic messages.
    var msg_bufs: [Parser.max_diagnostics][128]u8 = undefined;
    var msg_lens: [Parser.max_diagnostics]usize = undefined;
    var total_msg_len: usize = 0;

    for (diags, 0..) |diag, i| {
        const lexeme = if (diag.found_len > 0)
            source[diag.found_start .. diag.found_start + diag.found_len]
        else
            "";
        const msg = if (lexeme.len > 0)
            std.fmt.bufPrint(&msg_bufs[i], "expected {s}, found '{s}'", .{ @tagName(diag.expected), lexeme }) catch ""
        else
            std.fmt.bufPrint(&msg_bufs[i], "expected {s}", .{@tagName(diag.expected)}) catch "";
        msg_lens[i] = msg.len;
        total_msg_len += msg.len;
    }

    // Compute sizes.
    const header_len = 8; // token_count + diag_count
    const token_data_len = tokens.len * 12;
    const diag_data_len = diags.len * 16;
    const result_len = header_len + token_data_len + diag_data_len + total_msg_len;
    const buf = allocator.alloc(u8, result_len) catch return null;

    // Write header.
    const token_count: u32 = @intCast(tokens.len);
    const diag_count: u32 = @intCast(diags.len);
    @memcpy(buf[0..4], std.mem.asBytes(&token_count));
    @memcpy(buf[4..8], std.mem.asBytes(&diag_count));

    // Write tokens.
    for (tokens, 0..) |tok, i| {
        const off = header_len + i * 12;
        const tag: u32 = @intFromEnum(tok.tag);
        const start: u32 = @intCast(tok.start);
        const len: u32 = @intCast(tok.len);
        @memcpy(buf[off .. off + 4], std.mem.asBytes(&tag));
        @memcpy(buf[off + 4 .. off + 8], std.mem.asBytes(&start));
        @memcpy(buf[off + 8 .. off + 12], std.mem.asBytes(&len));
    }

    // Write diagnostics.
    var msg_offset: u32 = 0;
    for (diags, 0..) |diag, i| {
        const off = header_len + token_data_len + i * 16;
        const start: u32 = @intCast(diag.found_start);
        const dlen: u32 = @intCast(diag.found_len);
        const mlen: u32 = @intCast(msg_lens[i]);
        @memcpy(buf[off .. off + 4], std.mem.asBytes(&start));
        @memcpy(buf[off + 4 .. off + 8], std.mem.asBytes(&dlen));
        @memcpy(buf[off + 8 .. off + 12], std.mem.asBytes(&msg_offset));
        @memcpy(buf[off + 12 .. off + 16], std.mem.asBytes(&mlen));
        msg_offset += mlen;
    }

    // Write message strings.
    const msg_start = header_len + token_data_len + diag_data_len;
    var offset: usize = 0;
    for (0..diags.len) |i| {
        @memcpy(buf[msg_start + offset .. msg_start + offset + msg_lens[i]], msg_bufs[i][0..msg_lens[i]]);
        offset += msg_lens[i];
    }

    return buf.ptr;
}

const max_tokens = 4096;
const max_sexp_diagnostics = 64;

fn analyzeSexp(source_ptr: [*]const u8, source_len: usize) ?[*]const u8 {
    if (source_len == 0) return null;
    const source = source_ptr[0..source_len];

    // Collect tokens from streaming scanner.
    var scanner = root.sexp.Scanner.init(source);
    var tokens: [max_tokens]root.sexp.Token.Token = undefined;
    var token_count: usize = 0;
    while (token_count < max_tokens) {
        const tok = scanner.next();
        tokens[token_count] = tok;
        token_count += 1;
        if (tok.tag == .eof) break;
    }
    const toks = tokens[0..token_count];

    // Collect diagnostics: invalid tokens + unmatched brackets.
    var diag_starts: [max_sexp_diagnostics]usize = undefined;
    var diag_lens: [max_sexp_diagnostics]usize = undefined;
    var msg_bufs: [max_sexp_diagnostics][128]u8 = undefined;
    var msg_lens: [max_sexp_diagnostics]usize = undefined;
    var diag_count: usize = 0;
    var total_msg_len: usize = 0;

    const Tag = root.sexp.Token.Tag;

    for (toks) |tok| {
        if (tok.tag == .invalid and diag_count < max_sexp_diagnostics) {
            diag_starts[diag_count] = tok.start;
            diag_lens[diag_count] = tok.len;
            const lexeme = if (tok.len > 0) source[tok.start .. tok.start + tok.len] else "";
            const msg = if (lexeme.len > 0)
                std.fmt.bufPrint(&msg_bufs[diag_count], "invalid token '{s}'", .{lexeme}) catch ""
            else
                std.fmt.bufPrint(&msg_bufs[diag_count], "invalid token", .{}) catch "";
            msg_lens[diag_count] = msg.len;
            total_msg_len += msg.len;
            diag_count += 1;
        }
    }

    // Check bracket balancing.
    const max_depth = 256;
    var stack: [max_depth]struct { tag: Tag, start: usize, len: usize } = undefined;
    var depth: usize = 0;

    for (toks) |tok| {
        if (tok.tag == .lparen or tok.tag == .lbracket) {
            if (depth < max_depth) {
                stack[depth] = .{ .tag = tok.tag, .start = tok.start, .len = tok.len };
                depth += 1;
            }
        } else if (tok.tag == .rparen or tok.tag == .rbracket) {
            const expected_open: Tag = if (tok.tag == .rparen) .lparen else .lbracket;
            if (depth > 0 and stack[depth - 1].tag == expected_open) {
                depth -= 1;
            } else if (diag_count < max_sexp_diagnostics) {
                diag_starts[diag_count] = tok.start;
                diag_lens[diag_count] = tok.len;
                const msg = std.fmt.bufPrint(&msg_bufs[diag_count], "unmatched '{s}'", .{
                    source[tok.start .. tok.start + tok.len],
                }) catch "";
                msg_lens[diag_count] = msg.len;
                total_msg_len += msg.len;
                diag_count += 1;
            }
        }
    }
    // Report unclosed openers still on the stack.
    while (depth > 0 and diag_count < max_sexp_diagnostics) {
        depth -= 1;
        const open = stack[depth];
        diag_starts[diag_count] = open.start;
        diag_lens[diag_count] = open.len;
        const msg = std.fmt.bufPrint(&msg_bufs[diag_count], "unmatched '{s}'", .{
            source[open.start .. open.start + open.len],
        }) catch "";
        msg_lens[diag_count] = msg.len;
        total_msg_len += msg.len;
        diag_count += 1;
    }

    // Compute sizes.
    const header_len = 8;
    const token_data_len = token_count * 12;
    const diag_data_len = diag_count * 16;
    const result_len = header_len + token_data_len + diag_data_len + total_msg_len;
    const buf = allocator.alloc(u8, result_len) catch return null;

    // Write header.
    const tc: u32 = @intCast(token_count);
    const dc: u32 = @intCast(diag_count);
    @memcpy(buf[0..4], std.mem.asBytes(&tc));
    @memcpy(buf[4..8], std.mem.asBytes(&dc));

    // Write tokens.
    for (toks, 0..) |tok, i| {
        const off = header_len + i * 12;
        const tag: u32 = @intFromEnum(tok.tag);
        const start: u32 = @intCast(tok.start);
        const len: u32 = @intCast(tok.len);
        @memcpy(buf[off .. off + 4], std.mem.asBytes(&tag));
        @memcpy(buf[off + 4 .. off + 8], std.mem.asBytes(&start));
        @memcpy(buf[off + 8 .. off + 12], std.mem.asBytes(&len));
    }

    // Write diagnostics.
    var msg_offset: u32 = 0;
    for (0..diag_count) |i| {
        const off = header_len + token_data_len + i * 16;
        const start: u32 = @intCast(diag_starts[i]);
        const dlen: u32 = @intCast(diag_lens[i]);
        const mlen: u32 = @intCast(msg_lens[i]);
        @memcpy(buf[off .. off + 4], std.mem.asBytes(&start));
        @memcpy(buf[off + 4 .. off + 8], std.mem.asBytes(&dlen));
        @memcpy(buf[off + 8 .. off + 12], std.mem.asBytes(&msg_offset));
        @memcpy(buf[off + 12 .. off + 16], std.mem.asBytes(&mlen));
        msg_offset += mlen;
    }

    // Write message strings.
    const msg_start = header_len + token_data_len + diag_data_len;
    var offset: usize = 0;
    for (0..diag_count) |i| {
        @memcpy(buf[msg_start + offset .. msg_start + offset + msg_lens[i]], msg_bufs[i][0..msg_lens[i]]);
        offset += msg_lens[i];
    }

    return buf.ptr;
}

export fn analyze(lang: u8, ptr: [*]const u8, len: usize) ?[*]const u8 {
    const language = std.enums.fromInt(Language, lang) orelse return null;
    return switch (language) {
        .abnf => analyzeGeneric(root.abnf.Scanner, root.abnf.Parser, ptr, len),
        .bnf => analyzeGeneric(root.bnf.Scanner, root.bnf.Parser, ptr, len),
        .peg => analyzeGeneric(root.peg.Scanner, root.peg.Parser, ptr, len),
        .cfg => analyzeGeneric(root.cfg.Scanner, root.cfg.Parser, ptr, len),
        .sexp => analyzeSexp(ptr, len),
        .ere => analyzeGeneric(root.ere.Scanner, root.ere.Parser, ptr, len),
    };
}

/// Format result: raw UTF-8 bytes of the formatted grammar.
/// Returns null on parse error. Caller must free with free(ptr, len).
/// The length is written as a u32 in the first 4 bytes, followed by the string.
fn formatGeneric(
    comptime Scanner: type,
    comptime Parser: type,
    comptime Formatter: type,
    comptime with_tokens: bool,
    source_ptr: [*]const u8,
    source_len: usize,
) ?[*]const u8 {
    if (source_len == 0) return null;
    const source = source_ptr[0..source_len];

    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = parser.parse() catch return null;
    if (rules.len == 0) return null;

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    // Reserve 4 bytes for length header.
    aw.writer.writeAll(&[_]u8{ 0, 0, 0, 0 }) catch return null;

    if (with_tokens) {
        Formatter.formatGrammar(rules, tokens, source, &aw.writer) catch return null;
    } else {
        Formatter.formatGrammar(rules, &aw.writer) catch return null;
    }

    const written = aw.writer.buffered();
    const str_len: u32 = @intCast(written.len - 4);
    @memcpy(written[0..4], std.mem.asBytes(&str_len));

    // Transfer ownership to caller.
    const slice = aw.toOwnedSlice() catch return null;
    return slice.ptr;
}

fn formatEre(source_ptr: [*]const u8, source_len: usize) ?[*]const u8 {
    if (source_len == 0) return null;
    const source = source_ptr[0..source_len];

    var scanner = root.ere.Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = root.ere.Parser.init(tokens, source);
    const rules = parser.parse() catch return null;
    if (rules.len == 0) return null;

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();

    aw.writer.writeAll(&[_]u8{ 0, 0, 0, 0 }) catch return null;
    root.ere.Formatter.formatRule(rules[0], &aw.writer) catch return null;

    const written = aw.writer.buffered();
    const str_len: u32 = @intCast(written.len - 4);
    @memcpy(written[0..4], std.mem.asBytes(&str_len));

    const slice = aw.toOwnedSlice() catch return null;
    return slice.ptr;
}

export fn format(lang: u8, ptr: [*]const u8, len: usize) ?[*]const u8 {
    const language = std.enums.fromInt(Language, lang) orelse return null;
    return switch (language) {
        .abnf => formatGeneric(root.abnf.Scanner, root.abnf.Parser, root.abnf.Formatter, true, ptr, len),
        .bnf => formatGeneric(root.bnf.Scanner, root.bnf.Parser, root.bnf.Formatter, false, ptr, len),
        .peg => formatGeneric(root.peg.Scanner, root.peg.Parser, root.peg.Formatter, true, ptr, len),
        .ere => formatEre(ptr, len),
        .cfg, .sexp => null,
    };
}

/// Match result format (little-endian):
///   byte 0       - 1 if matched, 0 if not
///   bytes 1..4   - matched value length (u32), only present if matched
///   bytes 5..    - matched value bytes
///
/// Caller must free the returned buffer.
fn matchGeneric(
    comptime Scanner: type,
    comptime Parser: type,
    grammar_ptr: [*]const u8,
    grammar_len: usize,
    rule_ptr: [*]const u8,
    rule_len: usize,
    input_ptr: [*]const u8,
    input_len: usize,
) ?[*]const u8 {
    if (grammar_len == 0) return null;
    const grammar = grammar_ptr[0..grammar_len];
    const rule_name = rule_ptr[0..rule_len];
    const input = input_ptr[0..input_len];

    // Parse the grammar.
    var scanner = Scanner.init(grammar);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, grammar);
    const rules = parser.parse() catch return null;

    // Validate and merge.
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var validator = root.Validator.init(arena.allocator(), rules);
    const merged = validator.validate() catch return null;

    // Match.
    var matcher = root.Matcher.init(arena.allocator(), merged) catch return null;
    const result = matcher.match(rule_name, input) orelse {
        // No match - return single byte 0.
        const buf = allocator.alloc(u8, 1) catch return null;
        buf[0] = 0;
        return buf.ptr;
    };

    // Build result buffer.
    const val_len: u32 = @intCast(result.value.len);
    const buf = allocator.alloc(u8, 1 + 4 + result.value.len) catch return null;
    buf[0] = 1;
    @memcpy(buf[1..5], std.mem.asBytes(&val_len));
    @memcpy(buf[5 .. 5 + result.value.len], result.value);
    return buf.ptr;
}

export fn match(
    lang: u8,
    grammar_ptr: [*]const u8,
    grammar_len: usize,
    rule_ptr: [*]const u8,
    rule_len: usize,
    input_ptr: [*]const u8,
    input_len: usize,
) ?[*]const u8 {
    const language = std.enums.fromInt(Language, lang) orelse return null;
    return switch (language) {
        .abnf => matchGeneric(root.abnf.Scanner, root.abnf.Parser, grammar_ptr, grammar_len, rule_ptr, rule_len, input_ptr, input_len),
        .peg => matchGeneric(root.peg.Scanner, root.peg.Parser, grammar_ptr, grammar_len, rule_ptr, rule_len, input_ptr, input_len),
        .ere => matchGeneric(root.ere.Scanner, root.ere.Parser, grammar_ptr, grammar_len, rule_ptr, rule_len, input_ptr, input_len),
        .bnf => matchGeneric(root.bnf.Scanner, root.bnf.Parser, grammar_ptr, grammar_len, rule_ptr, rule_len, input_ptr, input_len),
        .cfg, .sexp => null,
    };
}

/// Tree result format (little-endian):
///   bytes 0..3   - JSON length (u32). 0 means "no match" (empty body).
///   bytes 4..    - JSON bytes (capture-tree serialization). Each node is
///                  `{"type":..,"span":[s,e]}` for normal rules, plus
///                  `"partial":true` for partial subtrees, or a fixed
///                  `"type":"ERROR"` / `"type":"MISSING"` plus a `"label"`
///                  field for synthesized recovery nodes.
///
/// Returns null on grammar parse error or compile failure. PEG only;
/// recovery directives (`#@ ...`) are recognized.
export fn tree(
    grammar_ptr: [*]const u8,
    grammar_len: usize,
    input_ptr: [*]const u8,
    input_len: usize,
) ?[*]const u8 {
    if (grammar_len == 0) return null;
    const grammar = grammar_ptr[0..grammar_len];
    const input = input_ptr[0..input_len];

    // Recovery-enabled PEG parser - the only front-end that produces
    // `#@` directive AST nodes. Other formats fall through to the
    // ordinary capture-tree path with no recovery semantics; for now,
    // tree() is PEG-only.
    const RecoveryPegParser = root.peg.ParserWith(.{ .recovery = true });

    var scanner = root.peg.Scanner.init(grammar);
    const tokens = scanner.scanTokens();
    var parser = RecoveryPegParser.init(tokens, grammar);
    const rules = parser.parse() catch return null;
    if (rules.len == 0) return null;

    var compiler = root.vm.Compiler.compileOpts(rules, .{ .rules_as_captures = true }) catch return null;

    const EventVm = root.vm.VmWith(.{ .capture_events = true });
    var vm = EventVm.initEvents(
        allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        input,
    );
    defer vm.deinit();

    // Build the result buffer with a 4-byte length header. On a failed
    // match we still return a buffer (length = 0) so the JS side can
    // distinguish "no match" from "grammar didn't compile" (null).
    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    aw.writer.writeAll(&[_]u8{ 0, 0, 0, 0 }) catch return null;

    const exec_result = vm.execute() catch return null;
    if (exec_result == null) {
        // No match: write 0-length header and return.
        const slice = aw.toOwnedSlice() catch return null;
        return slice.ptr;
    }

    var captured = vm.buildCaptureTree(allocator) catch return null;
    defer captured.deinit();

    // Build name slices indexed by rule_id and label_id for the renderer.
    const max_names = 256;
    var rule_names_buf: [max_names][]const u8 = undefined;
    const rule_names = rule_names_buf[0..compiler.rule_count];
    for (0..compiler.rule_count) |i| rule_names[i] = compiler.getRuleName(@intCast(i));

    var label_names_buf: [max_names][]const u8 = undefined;
    const label_names = label_names_buf[0..compiler.label_count];
    for (0..compiler.label_count) |i| label_names[i] = compiler.getLabelName(@intCast(i));

    captured.writeJson(&aw.writer, .{ .rules = rule_names, .labels = label_names }) catch return null;

    const written = aw.writer.buffered();
    const json_len: u32 = @intCast(written.len - 4);
    @memcpy(written[0..4], std.mem.asBytes(&json_len));

    const slice = aw.toOwnedSlice() catch return null;
    return slice.ptr;
}
