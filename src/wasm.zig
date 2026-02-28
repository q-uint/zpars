//! WASM FFI layer for zpars scanners.
//!
//! Exports a single `analyze` function that accepts a language tag and
//! pointer+length to source text in WASM linear memory. Returns a pointer
//! to a result buffer containing serialized tokens and diagnostics.
//!
//! Result format (little-endian):
//!   bytes 0..3  — token count (u32)
//!   bytes 4..7  — diagnostic count (u32)
//!   then for each token (12 bytes):
//!     bytes 0..3  — tag (u32)
//!     bytes 4..7  — start offset (u32)
//!     bytes 8..11 — length (u32)
//!   then for each diagnostic (16 bytes):
//!     bytes 0..3  — start offset (u32)
//!     bytes 4..7  — length (u32)
//!     bytes 8..11 — message offset (u32, relative to message data start)
//!     bytes 12..15 — message length (u32)
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
    const language = std.meta.intToEnum(Language, lang) catch return null;
    return switch (language) {
        .abnf => analyzeGeneric(root.abnf.Scanner, root.abnf.Parser, ptr, len),
        .bnf => analyzeGeneric(root.bnf.Scanner, root.bnf.Parser, ptr, len),
        .peg => analyzeGeneric(root.peg.Scanner, root.peg.Parser, ptr, len),
        .cfg => analyzeGeneric(root.cfg.Scanner, root.cfg.Parser, ptr, len),
        .sexp => analyzeSexp(ptr, len),
    };
}
