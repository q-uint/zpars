/// AOT runtime: loads a .zpar blob and executes the compiled parser.
///
/// Use `Engine` for repeated execution (mmaps once). Use `run` for
/// one-shot execution (mmaps and munmaps each call).
const std = @import("std");
const I = @import("Instruction.zig");
const Jit = @import("Jit.zig");
const Aot = @import("Aot.zig");
const Vm = @import("Vm.zig").Vm;

const page_size = Jit.page_size;
const JitCtx = Jit.JitCtx;

pub const Engine = struct {
    exec_mem: []align(page_size) u8,
    blob: Aot.Blob,
    jump_table: [4096]u64,
    captures_buf: [Jit.max_captures]u64,
    stack_buf: [Jit.max_stack]Jit.StackEntry,

    pub fn init(blob: Aot.Blob) !Engine {
        const size = std.mem.alignForward(usize, blob.native_code.len, page_size);
        const exec_mem = try std.posix.mmap(
            null,
            size,
            .{ .READ = true, .WRITE = true },
            .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
            -1,
            0,
        );
        @memcpy(exec_mem[0..blob.native_code.len], blob.native_code);
        try std.process.protectMemory(
            @alignCast(exec_mem[0..size]),
            .{ .read = true, .execute = true },
        );

        var jt = [_]u64{0} ** 4096;
        for (blob.jump_table, 0..) |v, i| {
            jt[i] = v;
        }

        return .{
            .exec_mem = exec_mem,
            .blob = blob,
            .jump_table = jt,
            .captures_buf = [_]u64{Jit.null_cap} ** Jit.max_captures,
            .stack_buf = undefined,
        };
    }

    pub fn deinit(self: *Engine) void {
        std.posix.munmap(self.exec_mem);
    }

    pub fn execute(self: *Engine, input: []const u8) ?usize {
        @memset(&self.captures_buf, Jit.null_cap);

        const ctx = JitCtx{
            .input_ptr = @intFromPtr(input.ptr),
            .input_len = input.len,
            .charsets_ptr = @intFromPtr(self.blob.charsets.ptr),
            .string_data_ptr = @intFromPtr(self.blob.string_data.ptr),
            .captures_ptr = @intFromPtr(&self.captures_buf),
            .stack_ptr = @intFromPtr(&self.stack_buf),
            .jump_table_ptr = @intFromPtr(&self.jump_table),
            .code_base_ptr = @intFromPtr(self.exec_mem.ptr),
            .helper_string_match = @intFromPtr(&Jit.helperStringMatch),
            .helper_charset_match = @intFromPtr(&Jit.helperCharsetMatch),
        };

        const jit_fn: *const fn (*const JitCtx) callconv(.c) u64 =
            @ptrCast(self.exec_mem.ptr);
        const result = jit_fn(&ctx);

        if (result == Jit.null_cap) return null;
        return @intCast(result);
    }

    pub fn getCapture(self: *const Engine, input: []const u8, i: u16) ?Vm.Span {
        const slot: usize = @as(usize, i) * 2;
        if (slot + 1 >= Jit.max_captures) return null;
        const s = self.captures_buf[slot];
        if (s == Jit.null_cap) return null;
        const e = self.captures_buf[slot + 1];
        if (e == Jit.null_cap) return null;
        _ = input;
        return .{ .start = @intCast(s), .end = @intCast(e) };
    }
};

pub fn run(blob: Aot.Blob, input: []const u8) ?usize {
    var engine = Engine.init(blob) catch return null;
    defer engine.deinit();
    return engine.execute(input);
}
