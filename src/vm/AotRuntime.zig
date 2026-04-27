/// AOT runtime: loads a .zpar blob and executes the compiled parser.
///
/// Use `Engine` for repeated execution (mmaps once). Use `run` for
/// one-shot execution (mmaps and munmaps each call). Both are aliases
/// for `EngineWith(.{})` / the equivalent one-shot wrapper -- mirror
/// the comptime configuration story of `Jit.JitWith` so blobs compiled
/// with `capture_events` or `memoize` get the matching runtime state
/// without dynamic dispatch.
const std = @import("std");
const I = @import("Instruction.zig");
const Jit = @import("Jit.zig");
const Aot = @import("Aot.zig");
const Vm = @import("Vm.zig").Vm;
const CaptureTree = @import("CaptureTree.zig");
const events_mod = @import("events.zig");
const memo_mod = @import("memo.zig");
const runtime_state = @import("RuntimeState.zig");

const page_size = Jit.page_size;
const JitCtx = Jit.JitCtx;

/// Default engine: matches blobs compiled with `compileToBlob` (no
/// capture events, no memoize). For event-recording blobs use
/// `EngineWith(.{ .capture_events = true })`; for memoized grammars
/// use `EngineWith(.{ .memoize = true })` (with or without events).
pub const Engine = EngineWith(.{});

pub fn EngineWith(comptime config: Jit.Config) type {
    return struct {
        const Self = @This();

        /// Expose the config so callers can branch on it at comptime
        /// when they receive a `*Self` via `anytype`.
        pub const aot_config = config;

        exec_mem: []align(page_size) u8,
        blob: Aot.Blob,
        jump_table: [4096]u64,
        captures_buf: [Jit.max_captures]u64,
        stack_buf: [Jit.max_stack]Jit.StackEntry,
        /// Optional event log + memo state shared with the JIT. All
        /// `config`-gated optional fields live here.
        state: runtime_state.RuntimeState(config) = .{},

        /// Plain constructor. Unavailable when `config.capture_events`
        /// or `config.memoize` is true -- use `initEvents` /
        /// `initPackrat`, which supply the allocator(s) needed.
        pub const init = if (config.capture_events or config.memoize) {} else struct {
            fn f(blob: Aot.Blob) !Self {
                if (blob.header.flags & Aot.Flag.capture_events != 0)
                    return error.ConfigMismatch;
                if (blob.header.flags & Aot.Flag.memoize != 0)
                    return error.ConfigMismatch;
                return try initInternal(blob, .empty());
            }
        }.f;

        /// Constructor for capture-event blobs without memoize. The
        /// allocator backs the event log shared between the runtime
        /// and the helpers.
        pub const initEvents = if (config.capture_events and !config.memoize)
            struct {
                fn f(allocator: std.mem.Allocator, blob: Aot.Blob) !Self {
                    if (blob.header.flags & Aot.Flag.capture_events == 0)
                        return error.ConfigMismatch;
                    if (blob.header.flags & Aot.Flag.memoize != 0)
                        return error.ConfigMismatch;
                    return try initInternal(blob, runtime_state.RuntimeState(config).initEvents(allocator));
                }
            }.f
        else {};

        /// Packrat constructor. Backs the memo table, side, heads, and
        /// (when `capture_events` is also on) the live event log + the
        /// cached-events replay buffer with the given allocator. The
        /// memo table itself is sized lazily on first `execute()`
        /// since input length isn't known yet. Threads the blob's
        /// per-memo-rule `examined_max` slice through to the runtime
        /// state for `applyEdit` precision.
        pub const initPackrat = if (config.memoize)
            struct {
                fn f(allocator: std.mem.Allocator, blob: Aot.Blob) !Self {
                    if ((blob.header.flags & Aot.Flag.memoize) == 0)
                        return error.ConfigMismatch;
                    if (config.capture_events and (blob.header.flags & Aot.Flag.capture_events) == 0)
                        return error.ConfigMismatch;
                    if (!config.capture_events and (blob.header.flags & Aot.Flag.capture_events) != 0)
                        return error.ConfigMismatch;
                    const state = try runtime_state.RuntimeState(config).initPackratLazy(
                        allocator,
                        @intCast(blob.header.memo_rule_count),
                        blob.examined_max,
                    );
                    return try initInternal(blob, state);
                }
            }.f
        else {};

        fn initInternal(blob: Aot.Blob, state: runtime_state.RuntimeState(config)) !Self {
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
                .state = state,
            };
        }

        pub fn deinit(self: *Self) void {
            std.posix.munmap(self.exec_mem);
            self.state.deinit();
        }

        pub fn execute(self: *Self, input: []const u8) ?usize {
            @memset(&self.captures_buf, Jit.null_cap);
            self.state.beginExecute(input.len) catch return null;
            if (config.memoize) {
                self.state.populateMemoCtx(
                    @intFromPtr(&self.stack_buf),
                    @intFromPtr(&self.jump_table),
                    @intFromPtr(self.exec_mem.ptr),
                );
            }

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
                .events_state_ptr = self.state.eventsStatePtr(),
                .helper_append_save = if (config.capture_events) @intFromPtr(&events_mod.helperAppendSave) else 0,
                .helper_truncate_events = if (config.capture_events) @intFromPtr(&events_mod.helperTruncate) else 0,
                .helper_append_token = if (config.capture_events) @intFromPtr(&events_mod.helperAppendToken) else 0,
                .helper_append_field = if (config.capture_events) @intFromPtr(&events_mod.helperAppendField) else 0,
                .helper_append_error_open = if (config.capture_events) @intFromPtr(&events_mod.helperAppendErrorOpen) else 0,
                .helper_append_error_close = if (config.capture_events) @intFromPtr(&events_mod.helperAppendErrorClose) else 0,
                .helper_append_missing = if (config.capture_events) @intFromPtr(&events_mod.helperAppendMissing) else 0,
                .helper_throw = if (config.capture_events) @intFromPtr(&events_mod.helperThrow) else 0,
                .helper_events_len = if (config.capture_events) @intFromPtr(&events_mod.helperEventsLen) else 0,
                .memo_ctx_ptr = self.state.memoCtxPtr(),
            };

            const jit_fn: *const fn (*const JitCtx) callconv(.c) u64 =
                @ptrCast(self.exec_mem.ptr);
            const result = jit_fn(&ctx);

            if (result == Jit.null_cap) return null;
            return @intCast(result);
        }

        pub fn getCapture(self: *const Self, input: []const u8, i: u16) ?Vm.Span {
            const slot: usize = @as(usize, i) * 2;
            if (slot + 1 >= Jit.max_captures) return null;
            const s = self.captures_buf[slot];
            if (s == Jit.null_cap) return null;
            const e = self.captures_buf[slot + 1];
            if (e == Jit.null_cap) return null;
            _ = input;
            return .{ .start = @intCast(s), .end = @intCast(e) };
        }

        /// Build a capture tree from the events recorded on the last
        /// `execute()`. Only available when `config.capture_events`
        /// is true. Mirrors `Jit.buildCaptureTree`.
        pub const buildCaptureTree = if (config.capture_events)
            struct {
                fn f(self: *const Self, tree_allocator: std.mem.Allocator) CaptureTree.BuildError!CaptureTree.Tree {
                    return self.state.buildCaptureTree(tree_allocator);
                }
            }.f
        else {};

        /// Raw view of recorded capture events. Valid until the next
        /// `execute()` or `deinit()`. Only available when
        /// `config.capture_events` is true.
        pub const getCaptureEvents = if (config.capture_events)
            struct {
                fn f(self: *const Self) []const CaptureTree.Event {
                    return self.state.getCaptureEvents();
                }
            }.f
        else {};
    };
}

pub fn run(blob: Aot.Blob, input: []const u8) ?usize {
    var engine = Engine.init(blob) catch return null;
    defer engine.deinit();
    return engine.execute(input);
}
