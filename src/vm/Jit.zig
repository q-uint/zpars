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
    tag: u64, // 0=choice, 1=ret, 2=save, 3=event
    val1: u64,
    val2: u64,
    /// Snapshot of `events.len` captured at a successful `save` or
    /// `event_open`/`event_close`, used by the backtrack handler to
    /// truncate the log in lockstep with the capture-slot undo (for
    /// save) or alone (for event). Unused (zero) when capture_events
    /// is off.
    event_len: u64 = 0,
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
    /// `*events_mod.State`. Zero when capture_events is off; backends
    /// only emit loads of this field when the comptime flag is set.
    events_state_ptr: u64 = 0,
    helper_append_save: u64 = 0,
    helper_truncate_events: u64 = 0,
};

/// Compile-time configuration for the JIT. Mirrors `Vm.Config` so the
/// same feature flags drive both backends.
pub const Config = struct {
    /// Record open/close events for each capture save so a tree can be
    /// built in a post-pass. When true, use `initEvents` (the plain
    /// `init` is gated off because no allocator is available to back
    /// the event log).
    capture_events: bool = false,
};

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
        /// Only present when `config.capture_events` is true. The JIT
        /// writes into this via the C-ABI helpers in `events.zig`.
        events: if (config.capture_events) events_mod.State else void =
            if (config.capture_events) undefined else {},

        /// Default constructor. Unavailable when `config.capture_events`
        /// is true - use `initEvents`, which supplies the allocator that
        /// backs the event log.
        pub const init = if (config.capture_events) {} else struct {
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
                };
                try backend.compile(&self);
                return self;
            }
        }.f;

        /// Constructor for event-recording runs. Only available when
        /// `config.capture_events` is true.
        pub const initEvents = if (config.capture_events)
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
                        .events = events_mod.State.init(allocator),
                    };
                    try backend.compile(&self);
                    return self;
                }
            }.f
        else {};

        pub fn deinit(self: *Self) void {
            std.posix.munmap(self.native_code);
            if (config.capture_events) {
                self.events.deinit();
            }
        }

        pub fn execute(self: *Self) ?usize {
            @memset(&self.captures_buf, null_cap);
            if (config.capture_events) {
                self.events.clear();
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
                .events_state_ptr = if (config.capture_events) @intFromPtr(&self.events) else 0,
                .helper_append_save = if (config.capture_events) @intFromPtr(&events_mod.helperAppendSave) else 0,
                .helper_truncate_events = if (config.capture_events) @intFromPtr(&events_mod.helperTruncate) else 0,
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

test "jit: rejects recovery grammars with explicit error" {
    const Ast = @import("../Ast.zig");
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
