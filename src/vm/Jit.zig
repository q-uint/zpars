/// JIT compiler for the grammar parsing VM.
///
/// Translates bytecode produced by the Compiler into native machine code,
/// eliminating the interpreter dispatch overhead. The architecture-specific
/// backend is selected at comptime.
const std = @import("std");
const I = @import("Instruction.zig");
const Vm = @import("Vm.zig");

const Jit = @This();

pub const FixupTarget = enum(u32) {
    backtrack = 0xFFFF_FFFE,
    fail = 0xFFFF_FFFD,
    success = 0xFFFF_FFFC,
    _, // bytecode PC

    pub fn bytecodePC(pc: u32) FixupTarget {
        return @enumFromInt(pc);
    }
};

pub const max_stack = 1024;
pub const max_captures = 64;
pub const null_cap = std.math.maxInt(u64);
pub const page_size = std.heap.page_size_min;

pub const StackEntry = extern struct {
    tag: u64, // 0=choice, 1=ret, 2=save
    val1: u64,
    val2: u64,
    _pad: u64 = 0,
};

comptime {
    if (@sizeOf(StackEntry) != 32) @compileError("StackEntry must be 32 bytes");
}

pub const JitCtx = extern struct {
    input_ptr: u64,
    input_len: u64,
    charsets_ptr: u64,
    string_data_ptr: u64,
    captures_ptr: u64,
    stack_ptr: u64,
    jump_table_ptr: u64,
    code_base_ptr: u64,
    helper_string_match: u64,
    helper_charset_match: u64,
};
code: []const I.Inst,
charsets: []const I.Charset,
string_data: []const u8,
input: []const u8,
native_code: []align(page_size) u8,
native_len: usize,
jump_table: [4096]u64,
captures_buf: [max_captures]u64,
stack_buf: [max_stack]StackEntry,

const backend = switch (@import("builtin").cpu.arch) {
    .aarch64 => @import("JitAarch64.zig"),
    .x86_64 => @import("JitX86.zig"),
    else => @compileError("JIT not supported for this architecture"),
};

pub fn init(
    code: []const I.Inst,
    charsets: []const I.Charset,
    string_data: []const u8,
    input: []const u8,
) !Jit {
    var self = Jit{
        .code = code,
        .charsets = charsets,
        .string_data = string_data,
        .input = input,
        .native_code = undefined,
        .native_len = 0,
        .jump_table = [_]u64{0} ** 4096,
        .captures_buf = [_]u64{null_cap} ** max_captures,
        .stack_buf = undefined,
    };
    try backend.compile(&self);
    return self;
}

pub fn deinit(self: *Jit) void {
    std.posix.munmap(self.native_code);
}

pub fn execute(self: *Jit) ?usize {
    @memset(&self.captures_buf, null_cap);

    const ctx = JitCtx{
        .input_ptr = @intFromPtr(self.input.ptr),
        .input_len = self.input.len,
        .charsets_ptr = @intFromPtr(self.charsets.ptr),
        .string_data_ptr = @intFromPtr(self.string_data.ptr),
        .captures_ptr = @intFromPtr(&self.captures_buf),
        .stack_ptr = @intFromPtr(&self.stack_buf),
        .jump_table_ptr = @intFromPtr(&self.jump_table),
        .code_base_ptr = @intFromPtr(self.native_code.ptr),
        .helper_string_match = @intFromPtr(&helperStringMatch),
        .helper_charset_match = @intFromPtr(&helperCharsetMatch),
    };

    const jit_fn: *const fn (*const JitCtx) callconv(.c) u64 =
        @ptrCast(self.native_code.ptr);
    const result = jit_fn(&ctx);

    if (result == null_cap) return null;
    return @intCast(result);
}

pub fn getCapture(self: *const Jit, i: u16) ?Vm.Span {
    const s = self.captures_buf[i * 2];
    if (s == null_cap) return null;
    const e = self.captures_buf[i * 2 + 1];
    if (e == null_cap) return null;
    return .{ .start = @intCast(s), .end = @intCast(e) };
}

pub fn getCaptureSlice(self: *const Jit, i: u16) ?[]const u8 {
    const span = self.getCapture(i) orelse return null;
    return self.input[span.start..span.end];
}

pub fn helperStringMatch(
    input_ptr: [*]const u8,
    input_len: usize,
    pos_arg: usize,
    str_data: [*]const u8,
    str_off: usize,
    str_len: usize,
) callconv(.c) usize {
    if (pos_arg + str_len > input_len) return 0;
    const a = input_ptr[pos_arg..][0..str_len];
    const b = str_data[str_off..][0..str_len];
    return if (std.mem.eql(u8, a, b)) 1 else 0;
}

pub fn helperCharsetMatch(
    charsets_ptr: [*]const I.Charset,
    idx: usize,
    byte: usize,
) callconv(.c) usize {
    return if (I.charsetContains(charsets_ptr[idx], @intCast(byte))) 1 else 0;
}

const testing = std.testing;
const Compiler = @import("Compiler.zig");
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

fn expectMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = compileEre(source);
    var jit = try Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), input);
    defer jit.deinit();
    const result = jit.execute();
    try testing.expectEqual(expected, result);
}

fn expectPegMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = compilePeg(source);
    var jit = try Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), input);
    defer jit.deinit();
    const result = jit.execute();
    try testing.expectEqual(expected, result);
}

test "jit: literal match" {
    try expectMatch("abc", "abc", 3);
    try expectMatch("abc", "abx", null);
    try expectMatch("abc", "ab", null);
}

test "jit: alternation" {
    try expectMatch("a|b", "a", 1);
    try expectMatch("a|b", "b", 1);
    try expectMatch("a|b", "c", null);
}

test "jit: star repetition" {
    try expectMatch("a*", "", 0);
    try expectMatch("a*", "aaa", 3);
    try expectMatch("a*b", "aaab", 4);
    try expectMatch("a*b", "b", 1);
}

test "jit: plus repetition" {
    try expectMatch("a+", "", null);
    try expectMatch("a+", "aaa", 3);
}

test "jit: optional" {
    try expectMatch("a?b", "ab", 2);
    try expectMatch("a?b", "b", 1);
}

test "jit: character class" {
    try expectMatch("[a-z]+", "hello", 5);
    try expectMatch("[a-z]+", "HELLO", null);
    try expectMatch("[0-9]+", "42", 2);
}

test "jit: negated character class" {
    try expectMatch("[^0-9]+", "abc", 3);
    try expectMatch("[^0-9]+", "123", null);
}

test "jit: dot wildcard" {
    try expectMatch("a.c", "abc", 3);
    try expectMatch("a.c", "aXc", 3);
    try expectMatch("a.c", "ac", null);
}

test "jit: grouped alternation" {
    try expectMatch("(ab|cd)e", "abe", 3);
    try expectMatch("(ab|cd)e", "cde", 3);
    try expectMatch("(ab|cd)e", "ace", null);
}

test "jit: interval repetition" {
    try expectMatch("a{2,4}", "a", null);
    try expectMatch("a{2,4}", "aa", 2);
    try expectMatch("a{2,4}", "aaa", 3);
    try expectMatch("a{2,4}", "aaaa", 4);
    try expectMatch("a{2,4}", "aaaaa", 4);
}

test "jit: alternation with common prefix" {
    try expectMatch("https|http", "https", 5);
    try expectMatch("https|http", "http", 4);
    try expectMatch("https|http", "httq", null);
}

test "jit: peg single rule" {
    try expectPegMatch("Main <- \"hello\"", "hello", 5);
    try expectPegMatch("Main <- \"hello\"", "world", null);
}

test "jit: peg rule references" {
    try expectPegMatch(
        \\Main  <- Greeting " " Name
        \\Greeting <- "hi" / "hello"
        \\Name <- [a-z]+
    , "hi world", 8);
}

test "jit: peg recursive rules" {
    try expectPegMatch(
        \\Expr   <- Term ("+" Term)*
        \\Term   <- Factor ("*" Factor)*
        \\Factor <- "(" Expr ")" / [0-9]+
    , "1+2*3", 5);
}

test "jit: capture single group" {
    var compiler = compileEre("a(bc)d");
    var jit = try Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "abcd");
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 4), jit.execute());
    try testing.expectEqualStrings("bc", jit.getCaptureSlice(0).?);
}

test "jit: capture multiple groups" {
    var compiler = compileEre("(a+)(b+)");
    var jit = try Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "aaabb");
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 5), jit.execute());
    try testing.expectEqualStrings("aaa", jit.getCaptureSlice(0).?);
    try testing.expectEqualStrings("bb", jit.getCaptureSlice(1).?);
}
