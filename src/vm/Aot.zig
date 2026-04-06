/// AOT (ahead-of-time) compiler for the grammar parsing VM.
///
/// Compiles bytecode into a self-contained binary blob containing native
/// machine code and all static data needed for execution. The blob can be
/// loaded by the AOT runtime to execute without the bytecode compiler.
const std = @import("std");
const I = @import("Instruction.zig");
const Jit = @import("Jit.zig");

const Aot = @This();

const backend = switch (@import("builtin").cpu.arch) {
    .aarch64 => @import("JitAarch64.zig"),
    .x86_64 => @import("JitX86.zig"),
    else => @compileError("AOT not supported for this architecture"),
};

pub const Arch = enum(u8) {
    x86_64 = 0,
    aarch64 = 1,
};

pub const current_arch: Arch = switch (@import("builtin").cpu.arch) {
    .x86_64 => .x86_64,
    .aarch64 => .aarch64,
    else => @compileError("AOT not supported for this architecture"),
};

pub const magic = [4]u8{ 'Z', 'P', 'A', 'R' };
pub const version: u32 = 1;

pub const Header = extern struct {
    magic: [4]u8,
    version: u32,
    arch: Arch,
    _pad: [3]u8 = .{ 0, 0, 0 },
    code_len: u32,
    charsets_count: u32,
    string_data_len: u32,
    jump_table_entries: u32,
    capture_count: u32,
};

comptime {
    if (@sizeOf(Header) != 32) @compileError("Header must be 32 bytes");
}

pub const Blob = struct {
    header: Header,
    native_code: []const u8,
    charsets: []const I.Charset,
    string_data: []const u8,
    jump_table: []const u64,
};

pub fn compileToBlob(
    allocator: std.mem.Allocator,
    code: []const I.Inst,
    charsets: []const I.Charset,
    string_data: []const u8,
    capture_count: u16,
) !Blob {
    const est = backend.estimateSize(code.len);
    const buf = try allocator.alloc(u8, est);

    const result = backend.generate(code, buf.ptr);

    // Shrink to actual size.
    const native_code = allocator.realloc(buf, result.native_len) catch buf[0..result.native_len];

    const jt = try allocator.alloc(u64, code.len);
    @memcpy(jt, result.jump_table[0..code.len]);

    // Copy charsets and string_data so the blob owns all its data.
    const owned_charsets = try allocator.alloc(I.Charset, charsets.len);
    @memcpy(owned_charsets, charsets);

    const owned_string_data = try allocator.alloc(u8, string_data.len);
    @memcpy(owned_string_data, string_data);

    return .{
        .header = .{
            .magic = magic,
            .version = version,
            .arch = current_arch,
            .code_len = @intCast(result.native_len),
            .charsets_count = @intCast(charsets.len),
            .string_data_len = @intCast(string_data.len),
            .jump_table_entries = @intCast(code.len),
            .capture_count = capture_count,
        },
        .native_code = native_code,
        .charsets = owned_charsets,
        .string_data = owned_string_data,
        .jump_table = jt,
    };
}

pub fn serializeBlob(allocator: std.mem.Allocator, blob: Blob) ![]u8 {
    const charsets_bytes = std.mem.sliceAsBytes(blob.charsets);
    const jt_bytes = std.mem.sliceAsBytes(blob.jump_table);
    const total = @sizeOf(Header) + blob.native_code.len +
        charsets_bytes.len + blob.string_data.len + jt_bytes.len;

    const buf = try allocator.alloc(u8, total);
    var off: usize = 0;

    @memcpy(buf[off..][0..@sizeOf(Header)], std.mem.asBytes(&blob.header));
    off += @sizeOf(Header);
    @memcpy(buf[off..][0..blob.native_code.len], blob.native_code);
    off += blob.native_code.len;
    @memcpy(buf[off..][0..charsets_bytes.len], charsets_bytes);
    off += charsets_bytes.len;
    @memcpy(buf[off..][0..blob.string_data.len], blob.string_data);
    off += blob.string_data.len;
    @memcpy(buf[off..][0..jt_bytes.len], jt_bytes);

    return buf;
}

pub fn deserializeBlob(allocator: std.mem.Allocator, data: []const u8) !Blob {
    if (data.len < @sizeOf(Header)) return error.UnexpectedEof;

    const header: *const Header = @ptrCast(@alignCast(data.ptr));

    if (!std.mem.eql(u8, &header.magic, &magic)) return error.InvalidMagic;
    if (header.version != version) return error.UnsupportedVersion;
    if (header.arch != current_arch) return error.ArchMismatch;

    var off: usize = @sizeOf(Header);

    const code_end = off + header.code_len;
    if (code_end > data.len) return error.UnexpectedEof;
    const native_code = try allocator.alloc(u8, header.code_len);
    errdefer allocator.free(native_code);
    @memcpy(native_code, data[off..code_end]);
    off = code_end;

    const charsets_byte_len = header.charsets_count * @sizeOf(I.Charset);
    const cs_end = off + charsets_byte_len;
    if (cs_end > data.len) return error.UnexpectedEof;
    const charsets = try allocator.alloc(I.Charset, header.charsets_count);
    errdefer allocator.free(charsets);
    @memcpy(std.mem.sliceAsBytes(charsets), data[off..cs_end]);
    off = cs_end;

    const sd_end = off + header.string_data_len;
    if (sd_end > data.len) return error.UnexpectedEof;
    const string_data = try allocator.alloc(u8, header.string_data_len);
    errdefer allocator.free(string_data);
    @memcpy(string_data, data[off..sd_end]);
    off = sd_end;

    const jt_byte_len = header.jump_table_entries * @sizeOf(u64);
    const jt_end = off + jt_byte_len;
    if (jt_end > data.len) return error.UnexpectedEof;
    const jump_table = try allocator.alloc(u64, header.jump_table_entries);
    errdefer allocator.free(jump_table);
    @memcpy(std.mem.sliceAsBytes(jump_table), data[off..jt_end]);

    return .{
        .header = header.*,
        .native_code = native_code,
        .charsets = charsets,
        .string_data = string_data,
        .jump_table = jump_table,
    };
}

pub fn freeBlob(allocator: std.mem.Allocator, blob: *Blob) void {
    allocator.free(blob.native_code);
    allocator.free(blob.charsets);
    allocator.free(blob.string_data);
    allocator.free(blob.jump_table);
}

const testing = std.testing;
const Compiler = @import("Compiler.zig");
const AotRuntime = @import("AotRuntime.zig");
const EreScanner = @import("../ere/Scanner.zig").Scanner;
const EreParser = @import("../ere/Parser.zig");
const PegScanner = @import("../peg/Scanner.zig").Scanner;
const PegParser = @import("../peg/Parser.zig");

fn compileEre(source: []const u8) Compiler {
    var scanner = EreScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, source);
    const rules = parser.parse() catch return Compiler{};
    return Compiler.compile(rules);
}

fn compilePeg(source: []const u8) Compiler {
    var scanner = PegScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, source);
    const rules = parser.parse() catch return Compiler{};
    return Compiler.compile(rules);
}

fn expectAotMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = compileEre(source);
    var blob = try compileToBlob(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        compiler.getCaptureCount(),
    );
    defer freeBlob(testing.allocator, &blob);

    // Round-trip through serialization.
    const data = try serializeBlob(testing.allocator, blob);
    defer testing.allocator.free(data);
    var blob2 = try deserializeBlob(testing.allocator, data);
    defer freeBlob(testing.allocator, &blob2);

    const result = AotRuntime.run(blob2, input);
    try testing.expectEqual(expected, result);
}

fn expectAotPegMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = compilePeg(source);
    var blob = try compileToBlob(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        compiler.getCaptureCount(),
    );
    defer freeBlob(testing.allocator, &blob);

    const data = try serializeBlob(testing.allocator, blob);
    defer testing.allocator.free(data);
    var blob2 = try deserializeBlob(testing.allocator, data);
    defer freeBlob(testing.allocator, &blob2);

    const result = AotRuntime.run(blob2, input);
    try testing.expectEqual(expected, result);
}

test "aot: literal match" {
    try expectAotMatch("abc", "abc", 3);
    try expectAotMatch("abc", "abx", null);
    try expectAotMatch("abc", "ab", null);
}

test "aot: alternation" {
    try expectAotMatch("a|b", "a", 1);
    try expectAotMatch("a|b", "b", 1);
    try expectAotMatch("a|b", "c", null);
}

test "aot: star repetition" {
    try expectAotMatch("a*", "", 0);
    try expectAotMatch("a*", "aaa", 3);
    try expectAotMatch("a*b", "aaab", 4);
}

test "aot: character class" {
    try expectAotMatch("[a-z]+", "hello", 5);
    try expectAotMatch("[a-z]+", "HELLO", null);
}

test "aot: peg rule references" {
    try expectAotPegMatch(
        \\Main  <- Greeting " " Name
        \\Greeting <- "hi" / "hello"
        \\Name <- [a-z]+
    , "hi world", 8);
}

test "aot: peg recursive rules" {
    try expectAotPegMatch(
        \\Expr   <- Term ("+" Term)*
        \\Term   <- Factor ("*" Factor)*
        \\Factor <- "(" Expr ")" / [0-9]+
    , "1+2*3", 5);
}

test "aot: blob header validation" {
    var bad_data = [_]u8{0} ** 32;
    const result = deserializeBlob(testing.allocator, &bad_data);
    try testing.expectError(error.InvalidMagic, result);
}

test "aot: blob too short" {
    var short_data = [_]u8{0} ** 16;
    const result = deserializeBlob(testing.allocator, &short_data);
    try testing.expectError(error.UnexpectedEof, result);
}
