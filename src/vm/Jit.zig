/// JIT compiler for the grammar parsing VM.
///
/// Translates bytecode produced by the Compiler into native machine code,
/// eliminating the interpreter dispatch overhead. The architecture-specific
/// backend is selected at comptime.
const std = @import("std");
const I = @import("Instruction.zig");
const Vm = @import("Vm.zig").Vm;
const CaptureTree = @import("CaptureTree.zig");
const events_mod = @import("events.zig");
const memo_mod = @import("memo.zig");
const jit_abi = @import("jit_abi.zig");
const runtime_state = @import("RuntimeState.zig");

/// Re-export the shared layout types so existing callers that reach
/// for `Jit.StackEntry` / `Jit.MemoCtx` / `Jit.max_stack` keep working
/// while the canonical definitions live in `jit_abi.zig` (which the
/// helper modules can also import without forming a cycle).
pub const StackEntry = jit_abi.StackEntry;
pub const MemoCtx = jit_abi.MemoCtx;
pub const max_stack = jit_abi.max_stack;

/// Layout of the per-call stack region the backends spill `JitCtx`
/// helper pointers and scratch values into. Both backends mirror this
/// layout 1:1 via `@offsetOf` so adding/reordering a slot updates
/// every emit site automatically; x86 prefixes it with 4 extra slots
/// (callee-saved-style state) since it doesn't dedicate registers to
/// `jump_table_ptr` / `code_base_ptr` the way aarch64 does.
///
/// Slots from `events_state_ptr` through `helper_events_len` are only
/// loaded when `Config.capture_events` is true. Slots from `memo_ctx`
/// through `memo_scratch2` are only loaded when `Config.memoize` is
/// true. `call_scratch` is reused by `throw` (capture_events) and the
/// memo helper-call path (memoize) to spill an out-pointer arg or
/// stash a caller-saved register.
pub const StackSlots = extern struct {
    helper_string_match: u64,
    helper_charset_match: u64,
    events_state_ptr: u64,
    helper_append_save: u64,
    helper_truncate_events: u64,
    helper_append_token: u64,
    helper_append_field: u64,
    helper_append_error_open: u64,
    helper_append_error_close: u64,
    helper_append_missing: u64,
    helper_throw: u64,
    helper_events_len: u64,
    /// Reused by `throw` (capture_events) and the memo helper-call
    /// path (memoize) to spill an out-pointer arg or stash a caller-
    /// saved register across a BLR/CALL. Two slots are reserved
    /// because some sites (notably the memo `ret` lowering) need to
    /// stash both `bsp` and the matched-frame index `t1` across the
    /// helper call.
    call_scratch: u64,
    call_scratch2: u64,
    memo_ctx: u64,
    memo_scratch1: u64,
    memo_scratch2: u64,
};

pub const FixupTarget = enum(u32) {
    backtrack = 0xFFFF_FFFE,
    fail = 0xFFFF_FFFD,
    success = 0xFFFF_FFFC,
    _, // bytecode PC

    pub fn bytecodePC(pc: u32) FixupTarget {
        return @enumFromInt(pc);
    }
};

pub const max_captures = 64;
pub const null_cap = std.math.maxInt(u64);
pub const page_size = std.heap.page_size_min;

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
    /// `*events_mod.State`. Zero when capture_events is off; backends
    /// only emit loads of this field when the comptime flag is set.
    events_state_ptr: u64 = 0,
    helper_append_save: u64 = 0,
    helper_truncate_events: u64 = 0,
    helper_append_token: u64 = 0,
    helper_append_field: u64 = 0,
    helper_append_error_open: u64 = 0,
    helper_append_error_close: u64 = 0,
    helper_append_missing: u64 = 0,
    helper_throw: u64 = 0,
    helper_events_len: u64 = 0,
    /// `*MemoCtx`. Zero when memoize is off; backends only emit loads
    /// of this field when the comptime flag is set. Bundled into a
    /// single pointer so JitCtx itself stays small even as the memo
    /// machinery grows new helpers.
    memo_ctx_ptr: u64 = 0,
};

/// Compile-time configuration for the JIT. Mirrors `Vm.Config` so the
/// same feature flags drive both backends. Re-exported from `jit_abi`
/// so the `RuntimeState` mixin can refer to it without importing
/// `Jit.zig`.
pub const Config = jit_abi.Config;

/// Default JIT type. Equivalent to `JitWith(.{})`.
pub const Jit = JitWith(.{});

const backend = switch (@import("builtin").cpu.arch) {
    .aarch64 => @import("JitAarch64.zig"),
    .x86_64 => @import("JitX86.zig"),
    else => @compileError("JIT not supported for this architecture"),
};

pub fn JitWith(comptime config: Config) type {
    return struct {
        const Self = @This();

        /// Expose the config so backends can branch on it at comptime when
        /// they receive a `*Self` via `anytype`.
        pub const jit_config = config;

        code: []const I.Inst,
        charsets: []const I.Charset,
        string_data: []const u8,
        input: []const u8,
        native_code: []align(page_size) u8,
        native_len: usize,
        jump_table: [4096]u64,
        captures_buf: [max_captures]u64,
        stack_buf: [max_stack]StackEntry,
        /// Optional event log + memo state shared with the AOT runtime.
        /// All `config`-gated optional fields live here.
        state: runtime_state.RuntimeState(config) = .{},

        /// Default constructor. Unavailable when `config.capture_events`
        /// or `config.memoize` is true - use `initEvents` / `initPackrat`,
        /// which supply the allocator(s) needed.
        pub const init = if (config.capture_events or config.memoize) {} else struct {
            fn f(
                code: []const I.Inst,
                charsets: []const I.Charset,
                string_data: []const u8,
                input: []const u8,
            ) !Self {
                var self = Self{
                    .code = code,
                    .charsets = charsets,
                    .string_data = string_data,
                    .input = input,
                    .native_code = undefined,
                    .native_len = 0,
                    .jump_table = [_]u64{0} ** 4096,
                    .captures_buf = [_]u64{null_cap} ** max_captures,
                    .stack_buf = undefined,
                    .state = .empty(),
                };
                try backend.compile(&self);
                return self;
            }
        }.f;

        /// Constructor for event-recording runs. Only available when
        /// `config.capture_events` is true and `config.memoize` is
        /// false (the memoize path uses `initPackrat`, which also
        /// initializes the events log when `capture_events` is on).
        pub const initEvents = if (config.capture_events and !config.memoize)
            struct {
                fn f(
                    allocator: std.mem.Allocator,
                    code: []const I.Inst,
                    charsets: []const I.Charset,
                    string_data: []const u8,
                    input: []const u8,
                ) !Self {
                    var self = Self{
                        .code = code,
                        .charsets = charsets,
                        .string_data = string_data,
                        .input = input,
                        .native_code = undefined,
                        .native_len = 0,
                        .jump_table = [_]u64{0} ** 4096,
                        .captures_buf = [_]u64{null_cap} ** max_captures,
                        .stack_buf = undefined,
                        .state = runtime_state.RuntimeState(config).initEvents(allocator),
                    };
                    try backend.compile(&self);
                    return self;
                }
            }.f
        else {};

        /// Packrat constructor. Allocates a memo table sized for
        /// `memo_rule_count` rules and `input.len + 1` positions. When
        /// `config.capture_events` is also true the same allocator
        /// backs the live event log and the cached-events buffer used
        /// to replay captures on a memo hit.
        pub const initPackrat = if (config.memoize)
            struct {
                fn f(
                    allocator: std.mem.Allocator,
                    code: []const I.Inst,
                    charsets: []const I.Charset,
                    string_data: []const u8,
                    memo_rule_count: u16,
                    input: []const u8,
                ) !Self {
                    return initPackratWithExamined(allocator, code, charsets, string_data, memo_rule_count, input, &.{});
                }
            }.f
        else {};

        /// Variant of `initPackrat` that threads the `LookaheadAnalysis`
        /// result through to `RuntimeState`. `examined_max` must be
        /// either empty (opt out of `applyEdit` precision) or exactly
        /// `memo_rule_count` long. Required path for hosts that intend
        /// to call `state.applyEdit` between executes.
        pub const initPackratWithExamined = if (config.memoize)
            struct {
                fn f(
                    allocator: std.mem.Allocator,
                    code: []const I.Inst,
                    charsets: []const I.Charset,
                    string_data: []const u8,
                    memo_rule_count: u16,
                    input: []const u8,
                    examined_max: []const u32,
                ) !Self {
                    var self = Self{
                        .code = code,
                        .charsets = charsets,
                        .string_data = string_data,
                        .input = input,
                        .native_code = undefined,
                        .native_len = 0,
                        .jump_table = [_]u64{0} ** 4096,
                        .captures_buf = [_]u64{null_cap} ** max_captures,
                        .stack_buf = undefined,
                        .state = try runtime_state.RuntimeState(config).initPackratEager(
                            allocator,
                            memo_rule_count,
                            input.len,
                            examined_max,
                        ),
                    };
                    try backend.compile(&self);
                    return self;
                }
            }.f
        else {};

        pub fn deinit(self: *Self) void {
            std.posix.munmap(self.native_code);
            self.state.deinit();
        }

        pub fn execute(self: *Self) ?usize {
            @memset(&self.captures_buf, null_cap);
            self.state.beginExecute(self.input.len) catch return null;
            if (config.memoize) {
                self.state.populateMemoCtx(
                    @intFromPtr(&self.stack_buf),
                    @intFromPtr(&self.jump_table),
                    @intFromPtr(self.native_code.ptr),
                );
            }

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
                @ptrCast(self.native_code.ptr);
            const result = jit_fn(&ctx);

            if (result == null_cap) return null;
            return @intCast(result);
        }

        pub fn getCapture(self: *const Self, i: u16) ?Vm.Span {
            const slot: usize = @as(usize, i) * 2;
            if (slot + 1 >= max_captures) return null;
            const s = self.captures_buf[slot];
            if (s == null_cap) return null;
            const e = self.captures_buf[slot + 1];
            if (e == null_cap) return null;
            return .{ .start = @intCast(s), .end = @intCast(e) };
        }

        pub fn getCaptureSlice(self: *const Self, i: u16) ?[]const u8 {
            const span = self.getCapture(i) orelse return null;
            return self.input[span.start..span.end];
        }

        /// Build a capture tree from the events recorded on the last
        /// `execute()`. Only available when `config.capture_events` is
        /// true. The returned `Tree` owns its nodes via `tree_allocator`.
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
const Compiler = @import("Compiler.zig").Compiler;
const EreScanner = @import("../ere/Scanner.zig").Scanner;
const EreParser = @import("../ere/Parser.zig").Parser;
const PegScanner = @import("../peg/Scanner.zig").Scanner;
const PegParser = @import("../peg/Parser.zig").Parser;

fn compileEre(source: []const u8) !Compiler {
    var scanner = EreScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, source);
    const rules = try parser.parse();
    return Compiler.compile(rules);
}

fn compilePeg(source: []const u8) !Compiler {
    var scanner = PegScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, source);
    const rules = try parser.parse();
    return Compiler.compile(rules);
}

fn expectMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = try compileEre(source);
    var jit = try Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), input);
    defer jit.deinit();
    const result = jit.execute();
    try testing.expectEqual(expected, result);
}

fn expectPegMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = try compilePeg(source);
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
    var compiler = try compileEre("a(bc)d");
    var jit = try Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "abcd");
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 4), jit.execute());
    try testing.expectEqualStrings("bc", jit.getCaptureSlice(0).?);
}

test "jit: capture multiple groups" {
    var compiler = try compileEre("(a+)(b+)");
    var jit = try Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "aaabb");
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 5), jit.execute());
    try testing.expectEqualStrings("aaa", jit.getCaptureSlice(0).?);
    try testing.expectEqualStrings("bb", jit.getCaptureSlice(1).?);
}

const EventJit = JitWith(.{ .capture_events = true });

test "jit capture_events: flat single group emits open/close pair" {
    var compiler = try compileEre("a(bc)d");
    var jit = try EventJit.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "abcd",
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 4), jit.execute());

    try testing.expectEqualStrings("bc", jit.getCaptureSlice(0).?);

    const events = jit.getCaptureEvents();
    try testing.expectEqual(@as(usize, 2), events.len);
    try testing.expectEqual(CaptureTree.Event{ .open = .{ .group_id = 0, .pos = 1 } }, events[0]);
    try testing.expectEqual(CaptureTree.Event{ .close = .{ .group_id = 0, .pos = 3 } }, events[1]);
}

test "jit capture_events: nested groups build a tree" {
    var compiler = try compileEre("((a)(b))");
    var jit = try EventJit.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "ab",
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 2), jit.execute());

    var tree = try jit.buildCaptureTree(testing.allocator);
    defer tree.deinit();

    try testing.expectEqual(@as(usize, 1), tree.roots.len);
    const outer = tree.roots[0];
    try testing.expectEqual(@as(u16, 0), outer.group_id);
    try testing.expectEqual(CaptureTree.Span{ .start = 0, .end = 2 }, outer.span);
    try testing.expectEqual(@as(usize, 2), outer.children.len);
    try testing.expectEqual(@as(u16, 1), outer.children[0].group_id);
    try testing.expectEqual(CaptureTree.Span{ .start = 0, .end = 1 }, outer.children[0].span);
    try testing.expectEqual(@as(u16, 2), outer.children[1].group_id);
    try testing.expectEqual(CaptureTree.Span{ .start = 1, .end = 2 }, outer.children[1].span);
}

test "jit capture_events: repetition yields sibling nodes" {
    var compiler = try compileEre("(a)(a)(a)");
    var jit = try EventJit.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "aaa",
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 3), jit.execute());

    const events = jit.getCaptureEvents();
    try testing.expectEqual(@as(usize, 6), events.len);
}

test "jit capture_events: backtracking truncates abandoned events" {
    // The first alternative (a)(b)x matches (a)(b) then fails on x;
    // the second alternative (c)(d)y succeeds. Only the surviving
    // captures from the second alternative should appear.
    var compiler = try compileEre("(a)(b)x|(c)(d)y");
    var jit = try EventJit.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "cdy",
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 3), jit.execute());

    const events = jit.getCaptureEvents();
    try testing.expectEqual(@as(usize, 4), events.len);
    try testing.expectEqual(CaptureTree.Event{ .open = .{ .group_id = 2, .pos = 0 } }, events[0]);
    try testing.expectEqual(CaptureTree.Event{ .close = .{ .group_id = 2, .pos = 1 } }, events[1]);
    try testing.expectEqual(CaptureTree.Event{ .open = .{ .group_id = 3, .pos = 1 } }, events[2]);
    try testing.expectEqual(CaptureTree.Event{ .close = .{ .group_id = 3, .pos = 2 } }, events[3]);
}

test "jit capture_events: failed match produces empty log" {
    var compiler = try compileEre("(a)b");
    var jit = try EventJit.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "ac",
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, null), jit.execute());
    try testing.expectEqual(@as(usize, 0), jit.getCaptureEvents().len);
}

test "jit capture_events: cleared between runs" {
    var compiler = try compileEre("(a)");
    var jit = try EventJit.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "a",
    );
    defer jit.deinit();
    _ = jit.execute();
    try testing.expectEqual(@as(usize, 2), jit.getCaptureEvents().len);
    _ = jit.execute();
    try testing.expectEqual(@as(usize, 2), jit.getCaptureEvents().len);
}

test "jit rules_as_captures: PEG grammar produces a tree" {
    const src =
        \\Expr <- Term ("+" Term)*
        \\Term <- [0-9]+
    ;
    var scanner = PegScanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, src);
    const rules = try parser.parse();
    var c = try Compiler.compileOpts(rules, .{ .rules_as_captures = true });

    var jit = try EventJit.initEvents(
        testing.allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        "1+2+3",
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 5), jit.execute());

    var tree = try jit.buildCaptureTree(testing.allocator);
    defer tree.deinit();

    try testing.expectEqual(@as(usize, 1), tree.roots.len);
    const expr = tree.roots[0];
    try testing.expectEqualStrings("Expr", c.getRuleName(expr.group_id));
    try testing.expectEqual(@as(usize, 3), expr.children.len);
    for (expr.children) |term| {
        try testing.expectEqualStrings("Term", c.getRuleName(term.group_id));
    }
}

test "jit: rejects recovery grammars when capture_events is off" {
    // Recovery opcodes need an event-log state. The default Jit has
    // capture_events = false, so attempting to compile a grammar that
    // uses lcatch/throw must surface a clean error rather than a
    // misbehaving codegen path.
    const throw_node: Ast.Node = .{ .throw_label = "L" };
    const missing_node: Ast.Node = .{ .missing_label = "L" };
    const lcatch_node: Ast.Node = .{ .lcatch = .{
        .label = "L",
        .body = &throw_node,
        .handler = &missing_node,
    } };
    const rules = [_]Ast.Rule{
        .{ .name = "stmt", .node = lcatch_node, .incremental = false },
    };

    var compiler = try Compiler.compileOpts(&rules, .{ .rules_as_captures = true });
    try testing.expectError(
        error.JitDoesNotSupportOp,
        Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), ""),
    );
}

const Ast = @import("../Ast.zig");

fn runJitRecovery(rules: []const Ast.Rule, input: []const u8) !struct {
    end: ?usize,
    tree: CaptureTree.Tree,
    jit: *EventJit,
} {
    var compiler = try Compiler.compileOpts(rules, .{ .rules_as_captures = true });
    const jit = try testing.allocator.create(EventJit);
    jit.* = try EventJit.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        input,
    );
    errdefer {
        jit.deinit();
        testing.allocator.destroy(jit);
    }
    const end = jit.execute();
    const tree = try jit.buildCaptureTree(testing.allocator);
    return .{ .end = end, .tree = tree, .jit = jit };
}

test "jit recovery: throw caught at top emits MISSING via recover_missing" {
    // Mirrors `recovery_test.zig` -- single-rule lcatch with missing
    // handler. Verifies the JIT's throw helper transfers control to
    // the handler PC and the missing event surfaces in the tree.
    const throw_node: Ast.Node = .{ .throw_label = "L" };
    const missing_node: Ast.Node = .{ .missing_label = "L" };
    const lcatch_node: Ast.Node = .{ .lcatch = .{
        .label = "L",
        .body = &throw_node,
        .handler = &missing_node,
    } };
    const rules = [_]Ast.Rule{
        .{ .name = "stmt", .node = lcatch_node, .incremental = false },
    };

    var r = try runJitRecovery(&rules, "");
    defer {
        r.tree.deinit();
        r.jit.deinit();
        testing.allocator.destroy(r.jit);
    }

    try testing.expectEqual(@as(?usize, 0), r.end);
    try testing.expectEqual(@as(usize, 1), r.tree.roots.len);
    const root = r.tree.roots[0];
    try testing.expectEqual(CaptureTree.NodeKind.rule, root.kind);
    try testing.expectEqual(@as(usize, 1), root.children.len);
    const m = root.children[0];
    try testing.expectEqual(CaptureTree.NodeKind.missing_node, m.kind);
}

test "jit recovery: throw across rule boundary leaves inner partial" {
    // Two-rule grammar; throw fires inside `inner` after consuming "a";
    // the unwinder must synthesize partial_close(inner) before running
    // the outer handler. Exercises both helperThrow's stack walk and
    // the partial-close synthesis.
    const a_lit: Ast.Node = .{ .char_val = .{ .value = "a", .case_sensitive = true } };
    const throw_node: Ast.Node = .{ .throw_label = "L" };
    const missing_node: Ast.Node = .{ .missing_label = "L" };
    const inner_concat_elems = [_]Ast.Node{ a_lit, throw_node };
    const inner_body: Ast.Node = .{ .concatenation = &inner_concat_elems };
    const inner_ref: Ast.Node = .{ .rulename = "inner" };
    const lcatch_node: Ast.Node = .{ .lcatch = .{
        .label = "L",
        .body = &inner_ref,
        .handler = &missing_node,
    } };
    const rules = [_]Ast.Rule{
        .{ .name = "outer", .node = lcatch_node, .incremental = false },
        .{ .name = "inner", .node = inner_body, .incremental = false },
    };

    var r = try runJitRecovery(&rules, "a");
    defer {
        r.tree.deinit();
        r.jit.deinit();
        testing.allocator.destroy(r.jit);
    }

    try testing.expectEqual(@as(?usize, 1), r.end);
    const outer = r.tree.roots[0];
    try testing.expectEqual(@as(usize, 2), outer.children.len);
    const inner = outer.children[0];
    try testing.expectEqual(CaptureTree.NodeKind.rule_partial, inner.kind);
    try testing.expectEqual(CaptureTree.Span{ .start = 0, .end = 1 }, inner.span);
    const m = outer.children[1];
    try testing.expectEqual(CaptureTree.NodeKind.missing_node, m.kind);
    try testing.expectEqual(CaptureTree.Span{ .start = 1, .end = 1 }, m.span);
}

test "jit recovery: rule handler wraps recovery in ERROR node" {
    // The handler is a real rule reference, so the compiler emits
    // event_error_open / event_error_close around it. Verifies all
    // three new event_* opcodes plus the throw/lcatch path.
    const throw_node: Ast.Node = .{ .throw_label = "L" };
    const x_lit: Ast.Node = .{ .char_val = .{ .value = "x", .case_sensitive = true } };
    const handler_ref: Ast.Node = .{ .rulename = "error_handler" };
    const lcatch_node: Ast.Node = .{ .lcatch = .{
        .label = "L",
        .body = &throw_node,
        .handler = &handler_ref,
    } };
    const rules = [_]Ast.Rule{
        .{ .name = "outer", .node = lcatch_node, .incremental = false },
        .{ .name = "error_handler", .node = x_lit, .incremental = false },
    };

    var r = try runJitRecovery(&rules, "x");
    defer {
        r.tree.deinit();
        r.jit.deinit();
        testing.allocator.destroy(r.jit);
    }

    try testing.expectEqual(@as(?usize, 1), r.end);
    const outer = r.tree.roots[0];
    try testing.expectEqual(@as(usize, 1), outer.children.len);
    const err_node = outer.children[0];
    try testing.expectEqual(CaptureTree.NodeKind.error_node, err_node.kind);
    try testing.expectEqual(CaptureTree.Span{ .start = 0, .end = 1 }, err_node.span);
    try testing.expectEqual(@as(usize, 1), err_node.children.len);
    const handler_node = err_node.children[0];
    try testing.expectEqual(CaptureTree.NodeKind.rule, handler_node.kind);
}

test "jit recovery: uncaught throw fails the whole match" {
    // A bare throw with no enclosing lcatch must drive helperThrow to
    // its miss path and the JIT to its fail handler. The events log is
    // left with whatever was emitted before the throw (here: an open
    // without a matching close), so buildCaptureTree on a failed match
    // is undefined behavior - we only assert the failure itself.
    const throw_node: Ast.Node = .{ .throw_label = "L" };
    const rules = [_]Ast.Rule{
        .{ .name = "stmt", .node = throw_node, .incremental = false },
    };

    var compiler = try Compiler.compileOpts(&rules, .{ .rules_as_captures = true });
    var jit = try EventJit.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "",
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, null), jit.execute());
}

test "jit recovery: lcatch with no throw runs body and pops cleanly" {
    // The lcatch frame is pushed but never matched. A regular `fail`
    // (here triggered by the body matching successfully and falling
    // through to the literal "z" mismatch) must walk *past* the lcatch
    // frame -- this exercises the widened tag-check in the backtrack
    // handler.
    const a_lit: Ast.Node = .{ .char_val = .{ .value = "a", .case_sensitive = true } };
    const z_lit: Ast.Node = .{ .char_val = .{ .value = "z", .case_sensitive = true } };
    const missing_node: Ast.Node = .{ .missing_label = "L" };
    const protected: Ast.Node = .{ .lcatch = .{
        .label = "L",
        .body = &a_lit,
        .handler = &missing_node,
    } };
    // alt: (lcatch L a missing) "z" / "ab"
    const alt1_elems = [_]Ast.Node{ protected, z_lit };
    const alt1: Ast.Node = .{ .concatenation = &alt1_elems };
    const ab_lit: Ast.Node = .{ .char_val = .{ .value = "ab", .case_sensitive = true } };
    const alts = [_]Ast.Node{ alt1, ab_lit };
    const top: Ast.Node = .{ .alternation = &alts };
    const rules = [_]Ast.Rule{
        .{ .name = "stmt", .node = top, .incremental = false },
    };

    var r = try runJitRecovery(&rules, "ab");
    defer {
        r.tree.deinit();
        r.jit.deinit();
        testing.allocator.destroy(r.jit);
    }

    // First alt matches "a" then fails on "z"; backtrack must rewind
    // past the lcatch frame and the second alt then matches "ab".
    try testing.expectEqual(@as(?usize, 2), r.end);
}

test "jit token_events=.all: literal matches emit token events" {
    const src =
        \\Expr <- Term ("+" Term)*
        \\Term <- [0-9]+
    ;
    var scanner = PegScanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, src);
    const rules = try parser.parse();
    var c = try Compiler.compileOpts(rules, .{
        .rules_as_captures = true,
        .token_events = .all,
    });

    var jit = try EventJit.initEvents(
        testing.allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        "1+2",
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 3), jit.execute());

    var saw_plus_token = false;
    for (jit.getCaptureEvents()) |ev| switch (ev) {
        .token => |t| {
            if (t.start == 1 and t.end == 2) saw_plus_token = true;
        },
        else => {},
    };
    try testing.expect(saw_plus_token);
}

test "jit token_events=.all: backtracking truncates abandoned token events" {
    // First alternative matches the literal "ab" then fails on "z";
    // the second alternative succeeds. The token event from the first
    // try must be truncated when the choice unwinds.
    const src =
        \\Main <- "ab" "z" / "ab" "c"
    ;
    var scanner = PegScanner.init(src);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, src);
    const rules = try parser.parse();
    var c = try Compiler.compileOpts(rules, .{
        .rules_as_captures = true,
        .token_events = .all,
    });

    var jit = try EventJit.initEvents(
        testing.allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        "abc",
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 3), jit.execute());

    var token_count: usize = 0;
    for (jit.getCaptureEvents()) |ev| switch (ev) {
        .token => token_count += 1,
        else => {},
    };
    try testing.expectEqual(@as(usize, 2), token_count);
}

test "jit field_events: field marker is emitted before tagged subexpression" {
    const inner: Ast.Node = .{ .char_val = .{ .value = "x", .case_sensitive = true } };
    const field_node: Ast.Node = .{ .field = .{ .name = "lhs", .body = &inner } };
    const rules = [_]Ast.Rule{
        .{ .name = "Main", .node = field_node, .incremental = false },
    };

    var c = try Compiler.compileOpts(&rules, .{
        .rules_as_captures = true,
        .field_events = true,
    });

    var jit = try EventJit.initEvents(
        testing.allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        "x",
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 1), jit.execute());

    var saw_field_marker = false;
    for (jit.getCaptureEvents()) |ev| switch (ev) {
        .field_marker => |fm| {
            try testing.expectEqual(@as(u16, 0), fm.field_id);
            try testing.expectEqual(@as(u32, 0), fm.pos);
            saw_field_marker = true;
        },
        else => {},
    };
    try testing.expect(saw_field_marker);
}

const PackJit = JitWith(.{ .memoize = true });

test "jit packrat: smoke -- minimal grammar through packrat path" {
    // Smallest grammar that produces a memo_call opcode: two rules,
    // both capture-free. Validates the basic memo_call -> miss ->
    // ret -> writeback -> resume flow without involving alternation
    // or character classes.
    const src =
        \\Main <- A
        \\A    <- "a"
    ;
    var memo = try Compiler.compileOpts(blk: {
        var sc = PegScanner.init(src);
        const tokens = sc.scanTokens();
        var p = PegParser.init(tokens, src);
        break :blk try p.parse();
    }, .{ .memoize = true });
    var jit = try PackJit.initPackrat(
        testing.allocator,
        memo.getCode(),
        memo.getCharsets(),
        memo.getStringData(),
        memo.getMemoRuleCount(),
        "a",
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 1), jit.execute());
}

test "jit packrat: matches simple grammar with same result as plain JIT" {
    const src =
        \\Main <- Greet " " Name
        \\Greet <- "hi" / "hello"
        \\Name  <- [a-z]+
    ;
    const input = "hello world";

    var plain = try compilePeg(src);
    var jit_plain = try Jit.init(plain.getCode(), plain.getCharsets(), plain.getStringData(), input);
    defer jit_plain.deinit();
    const r_plain = jit_plain.execute();

    var memo = try Compiler.compileOpts(blk: {
        var sc = PegScanner.init(src);
        const tokens = sc.scanTokens();
        var p = PegParser.init(tokens, src);
        break :blk try p.parse();
    }, .{ .memoize = true });
    var jit_memo = try PackJit.initPackrat(
        testing.allocator,
        memo.getCode(),
        memo.getCharsets(),
        memo.getStringData(),
        memo.getMemoRuleCount(),
        input,
    );
    defer jit_memo.deinit();
    const r_memo = jit_memo.execute();

    try testing.expectEqual(r_plain, r_memo);
    try testing.expectEqual(@as(?usize, 11), r_memo);
}

test "jit packrat: redundant rule re-entry uses cache (correctness check)" {
    // S backtracks after the first alternative fails on `!`; the
    // second alternative re-enters E at pos 0. Without packrat, E
    // would be re-evaluated; with packrat the second call hits the
    // cached success entry written by the first. Test asserts the
    // result, not the work saved.
    const src =
        \\S <- E "!" / E "?"
        \\E <- "a" "b" "c" "d"
    ;
    const input = "abcd?";

    var memo = try Compiler.compileOpts(blk: {
        var sc = PegScanner.init(src);
        const tokens = sc.scanTokens();
        var p = PegParser.init(tokens, src);
        break :blk try p.parse();
    }, .{ .memoize = true });
    var jit = try PackJit.initPackrat(
        testing.allocator,
        memo.getCode(),
        memo.getCharsets(),
        memo.getStringData(),
        memo.getMemoRuleCount(),
        input,
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 5), jit.execute());
}

test "jit packrat: failure memoization on re-entry" {
    // First alternative fails (E doesn't match "abcz"), so the table
    // entry for (E, 0) becomes .fail. Second alternative re-enters E
    // and hits the cached fail short-circuit.
    const src =
        \\S <- E "!" / "abcz"
        \\E <- "a" "b" "c" "d"
    ;
    const input = "abcz";

    var memo = try Compiler.compileOpts(blk: {
        var sc = PegScanner.init(src);
        const tokens = sc.scanTokens();
        var p = PegParser.init(tokens, src);
        break :blk try p.parse();
    }, .{ .memoize = true });
    var jit = try PackJit.initPackrat(
        testing.allocator,
        memo.getCode(),
        memo.getCharsets(),
        memo.getStringData(),
        memo.getMemoRuleCount(),
        input,
    );
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 4), jit.execute());
}

test "jit packrat: rejects bytecode without memoize config" {
    // A grammar compiled with memoize=true will contain memo_call.
    // The default Jit has memoize=false, so attempting to compile
    // such bytecode through the plain JIT must surface a clean
    // error.
    const src =
        \\Main <- A
        \\A    <- "a"
    ;
    var memo = try Compiler.compileOpts(blk: {
        var sc = PegScanner.init(src);
        const tokens = sc.scanTokens();
        var p = PegParser.init(tokens, src);
        break :blk try p.parse();
    }, .{ .memoize = true });
    try testing.expectError(
        error.JitDoesNotSupportOp,
        Jit.init(memo.getCode(), memo.getCharsets(), memo.getStringData(), "a"),
    );
}

fn runJitPackrat(src: []const u8, input: []const u8) !?usize {
    var memo = try Compiler.compileOpts(blk: {
        var sc = PegScanner.init(src);
        const tokens = sc.scanTokens();
        var p = PegParser.init(tokens, src);
        break :blk try p.parse();
    }, .{ .memoize = true });
    var jit = try PackJit.initPackrat(
        testing.allocator,
        memo.getCode(),
        memo.getCharsets(),
        memo.getStringData(),
        memo.getMemoRuleCount(),
        input,
    );
    defer jit.deinit();
    return jit.execute();
}

test "jit warth: direct left recursion (single digit seed)" {
    const src =
        \\Expr <- Expr "+" Num / Num
        \\Num  <- [0-9]+
    ;
    try testing.expectEqual(@as(?usize, 1), try runJitPackrat(src, "1"));
}

test "jit warth: direct left recursion grows across input" {
    const src =
        \\Expr <- Expr "+" Num / Num
        \\Num  <- [0-9]+
    ;
    try testing.expectEqual(@as(?usize, 5), try runJitPackrat(src, "1+2+3"));
}

test "jit warth: direct left recursion stops at non-matching suffix" {
    const src =
        \\Expr <- Expr "+" Num / Num
        \\Num  <- [0-9]+
    ;
    try testing.expectEqual(@as(?usize, 3), try runJitPackrat(src, "1+2+x"));
}

test "jit warth: left recursion with two operators" {
    const src =
        \\Expr <- Expr "+" Num / Expr "-" Num / Num
        \\Num  <- [0-9]+
    ;
    try testing.expectEqual(@as(?usize, 7), try runJitPackrat(src, "1+2-3+4"));
}

test "jit warth: left recursion that never matches returns null" {
    const src =
        \\Expr <- Expr "+" Num / Num
        \\Num  <- [0-9]+
    ;
    try testing.expectEqual(@as(?usize, null), try runJitPackrat(src, "x"));
}

test "jit warth: indirect left recursion through two rules" {
    const src =
        \\A <- B
        \\B <- A "x" / "y"
    ;
    try testing.expectEqual(@as(?usize, 3), try runJitPackrat(src, "yxx"));
}
