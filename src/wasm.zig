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

export fn analyze(lang: u8, ptr: [*]const u8, len: usize) ?[*]const u8 {
    const language = std.meta.intToEnum(Language, lang) catch return null;
    return switch (language) {
        .abnf => analyzeGeneric(root.abnf.Scanner, root.abnf.Parser, ptr, len),
        .bnf => analyzeGeneric(root.bnf.Scanner, root.bnf.Parser, ptr, len),
        .peg => analyzeGeneric(root.peg.Scanner, root.peg.Parser, ptr, len),
        .cfg => analyzeGeneric(root.cfg.Scanner, root.cfg.Parser, ptr, len),
    };
}
