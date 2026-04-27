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
        /// Only present when `config.capture_events` is true. The
        /// emitted code writes into this through the C-ABI helpers
        /// in `events.zig`, exactly like the JIT.
        events: if (config.capture_events) events_mod.State else void =
            if (config.capture_events) undefined else {},

        /// Memo state. Sized lazily per-execute since the AOT engine
        /// doesn't bind to a single input the way the JIT does -- the
        /// table grows when a longer input arrives, otherwise reuses
        /// the existing capacity. Allocated by `initPackrat`, freed
        /// by `deinit`.
        memo_allocator: if (config.memoize) std.mem.Allocator else void =
            if (config.memoize) undefined else {},
        memo_table: if (config.memoize) []memo_mod.Entry else void =
            if (config.memoize) &.{} else {},
        memo_stride: if (config.memoize) usize else void =
            if (config.memoize) 0 else {},
        memo_rule_count: if (config.memoize) u16 else void =
            if (config.memoize) 0 else {},
        memo_heads: if (config.memoize) memo_mod.Heads else void =
            if (config.memoize) undefined else {},
        memo_side: if (config.memoize) memo_mod.Side else void =
            if (config.memoize) undefined else {},
        memo_events_buf: if (config.memoize and config.capture_events) memo_mod.EventsBuf else void =
            if (config.memoize and config.capture_events) undefined else {},
        memo_ctx: if (config.memoize) Jit.MemoCtx else void =
            if (config.memoize) undefined else {},

        /// Plain constructor. Unavailable when `config.capture_events`
        /// or `config.memoize` is true -- use `initEvents` /
        /// `initPackrat`, which supply the allocator(s) needed.
        pub const init = if (config.capture_events or config.memoize) {} else struct {
            fn f(blob: Aot.Blob) !Self {
                if (blob.header.flags & Aot.Flag.capture_events != 0)
                    return error.ConfigMismatch;
                if (blob.header.flags & Aot.Flag.memoize != 0)
                    return error.ConfigMismatch;
                return try initInternal(blob);
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
                    var self = try initInternal(blob);
                    self.events = events_mod.State.init(allocator);
                    return self;
                }
            }.f
        else {};

        /// Packrat constructor. Backs the memo table, side, heads, and
        /// (when `capture_events` is also on) the live event log + the
        /// cached-events replay buffer with the given allocator. The
        /// memo table itself is sized lazily on first `execute()`
        /// since input length isn't known yet.
        pub const initPackrat = if (config.memoize)
            struct {
                fn f(allocator: std.mem.Allocator, blob: Aot.Blob) !Self {
                    if ((blob.header.flags & Aot.Flag.memoize) == 0)
                        return error.ConfigMismatch;
                    if (config.capture_events and (blob.header.flags & Aot.Flag.capture_events) == 0)
                        return error.ConfigMismatch;
                    if (!config.capture_events and (blob.header.flags & Aot.Flag.capture_events) != 0)
                        return error.ConfigMismatch;

                    var self = try initInternal(blob);
                    self.memo_allocator = allocator;
                    self.memo_rule_count = @intCast(blob.header.memo_rule_count);
                    self.memo_table = &.{};
                    self.memo_stride = 0;
                    self.memo_side = memo_mod.Side.init(allocator);
                    // Heads needs an arr sized for input.len + 1.
                    // Start at zero; ensureMemoCapacity grows it.
                    self.memo_heads = try memo_mod.Heads.init(allocator, 0);
                    if (config.capture_events) {
                        self.events = events_mod.State.init(allocator);
                        self.memo_events_buf = memo_mod.EventsBuf.init(allocator);
                    }
                    return self;
                }
            }.f
        else {};

        fn initInternal(blob: Aot.Blob) !Self {
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

        pub fn deinit(self: *Self) void {
            std.posix.munmap(self.exec_mem);
            if (config.capture_events) {
                self.events.deinit();
            }
            if (config.memoize) {
                if (config.capture_events) self.memo_events_buf.deinit();
                self.memo_heads.deinit();
                self.memo_side.deinit();
                if (self.memo_table.len > 0) self.memo_allocator.free(self.memo_table);
            }
        }

        /// Resize memo state to fit `input.len`. Called per-execute
        /// before the parser runs. Reuses existing allocations when
        /// the new stride fits; reallocates otherwise.
        fn ensureMemoCapacity(self: *Self, input_len: usize) !void {
            const stride = input_len + 1;

            // memo_table: memo_rule_count * stride entries.
            const needed_table = @as(usize, self.memo_rule_count) * stride;
            if (needed_table > self.memo_table.len) {
                if (self.memo_table.len > 0) self.memo_allocator.free(self.memo_table);
                self.memo_table = try self.memo_allocator.alloc(memo_mod.Entry, needed_table);
            }
            self.memo_stride = stride;

            // Heads.arr: stride u32s.
            if (stride > self.memo_heads.arr.len) {
                self.memo_allocator.free(self.memo_heads.arr);
                self.memo_heads.arr = try self.memo_allocator.alloc(u32, stride);
            }
        }

        pub fn execute(self: *Self, input: []const u8) ?usize {
            @memset(&self.captures_buf, Jit.null_cap);
            if (config.capture_events) {
                self.events.clear();
            }
            if (config.memoize) {
                self.ensureMemoCapacity(input.len) catch return null;
                if (self.memo_rule_count > 0) {
                    const table_used = @as(usize, self.memo_rule_count) * self.memo_stride;
                    @memset(self.memo_table[0..table_used], .{ .state = .empty, .next_pos_or_frame = 0 });
                }
                @memset(self.memo_heads.arr[0..self.memo_stride], memo_mod.no_head);
                for (self.memo_heads.pool.items) |*h| {
                    h.involved.deinit(self.memo_heads.allocator);
                    h.eval.deinit(self.memo_heads.allocator);
                }
                self.memo_heads.pool.clearRetainingCapacity();
                self.memo_side.clear();
                if (config.capture_events) {
                    self.memo_events_buf.list.clearRetainingCapacity();
                }
                self.memo_ctx = .{
                    .table_ptr = @intFromPtr(self.memo_table.ptr),
                    .stride = self.memo_stride,
                    .side_ptr = @intFromPtr(&self.memo_side),
                    .events_buf_ptr = if (config.capture_events) @intFromPtr(&self.memo_events_buf) else 0,
                    .events_state_ptr = if (config.capture_events) @intFromPtr(&self.events) else 0,
                    .stack_ptr = @intFromPtr(&self.stack_buf),
                    .jump_table_ptr = @intFromPtr(&self.jump_table),
                    .code_base = @intFromPtr(self.exec_mem.ptr),
                    .heads_ptr = @intFromPtr(&self.memo_heads),
                    .memo_rule_count = self.memo_rule_count,
                    .helper_call_begin = @intFromPtr(&memo_mod.helperMemoCallBegin),
                    .helper_cached_slice = @intFromPtr(&memo_mod.helperMemoCachedSlice),
                    .helper_replay_events = if (config.capture_events) @intFromPtr(&memo_mod.helperMemoReplayEvents) else 0,
                    .helper_ret = @intFromPtr(&memo_mod.helperMemoRet),
                    .helper_backtrack = @intFromPtr(&memo_mod.helperMemoBacktrack),
                };
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
                .events_state_ptr = if (config.capture_events) @intFromPtr(&self.events) else 0,
                .helper_append_save = if (config.capture_events) @intFromPtr(&events_mod.helperAppendSave) else 0,
                .helper_truncate_events = if (config.capture_events) @intFromPtr(&events_mod.helperTruncate) else 0,
                .helper_append_token = if (config.capture_events) @intFromPtr(&events_mod.helperAppendToken) else 0,
                .helper_append_field = if (config.capture_events) @intFromPtr(&events_mod.helperAppendField) else 0,
                .helper_append_error_open = if (config.capture_events) @intFromPtr(&events_mod.helperAppendErrorOpen) else 0,
                .helper_append_error_close = if (config.capture_events) @intFromPtr(&events_mod.helperAppendErrorClose) else 0,
                .helper_append_missing = if (config.capture_events) @intFromPtr(&events_mod.helperAppendMissing) else 0,
                .helper_throw = if (config.capture_events) @intFromPtr(&events_mod.helperThrow) else 0,
                .helper_events_len = if (config.capture_events) @intFromPtr(&events_mod.helperEventsLen) else 0,
                .memo_ctx_ptr = if (config.memoize) @intFromPtr(&self.memo_ctx) else 0,
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
                    return CaptureTree.buildFromEvents(tree_allocator, self.events.items());
                }
            }.f
        else {};

        /// Raw view of recorded capture events. Valid until the next
        /// `execute()` or `deinit()`. Only available when
        /// `config.capture_events` is true.
        pub const getCaptureEvents = if (config.capture_events)
            struct {
                fn f(self: *const Self) []const CaptureTree.Event {
                    return self.events.items();
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
