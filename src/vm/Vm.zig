/// Grammar parsing VM.
///
/// Executes bytecode produced by the Compiler. Uses a backtracking
/// stack for ordered choice and a call stack for rule invocations.
const std = @import("std");
const I = @import("Instruction.zig");

const Vm = @This();

const max_stack = 1024;
const max_captures = 64;

const Entry = union(enum) {
    /// Backtrack point: saved position and instruction to jump to on failure.
    choice: struct {
        pos: usize,
        pc: u32,
    },
    /// Return address for rule calls.
    ret: u32,
    /// Undo log for a save instruction: restore old value on backtrack.
    save: struct {
        slot: u16,
        old: ?usize,
    },
};

pub const Span = struct {
    start: usize,
    end: usize,
};

code: []const I.Inst,
charsets: []const I.Charset,
string_data: []const u8,
input: []const u8,
trace: ?Trace = null,
captures: [max_captures]?usize = .{null} ** max_captures,

pub const Writer = @TypeOf(@as(std.fs.File.Writer, undefined).interface);

pub const Trace = struct {
    writer: *Writer,
};

pub fn init(code: []const I.Inst, charsets: []const I.Charset, string_data: []const u8, input: []const u8) Vm {
    return .{ .code = code, .charsets = charsets, .string_data = string_data, .input = input };
}

/// Run the VM. Returns the position after the match, or null on failure.
pub fn execute(self: *Vm) ?usize {
    var pc: u32 = 0;
    var pos: usize = 0;
    var stack: [max_stack]Entry = undefined;
    var sp: usize = 0;

    while (pc < self.code.len) {
        const inst = self.code[pc];
        self.traceStep(pc, pos, sp, inst);
        switch (inst.op) {
            .char => {
                if (pos < self.input.len and self.input[pos] == inst.data.byte) {
                    pos += 1;
                    pc += 1;
                } else {
                    if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                    return null;
                }
            },
            .string => {
                const ref = inst.data.string;
                const str = self.string_data[ref.offset..][0..ref.len];
                if (pos + ref.len <= self.input.len and
                    std.mem.eql(u8, self.input[pos..][0..ref.len], str))
                {
                    pos += ref.len;
                    pc += 1;
                } else {
                    if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                    return null;
                }
            },
            .any => {
                if (pos < self.input.len) {
                    pos += 1;
                    pc += 1;
                } else {
                    if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                    return null;
                }
            },
            .set => {
                const cs = self.charsets[inst.data.charset];
                if (pos < self.input.len and I.charsetContains(cs, self.input[pos])) {
                    pos += 1;
                    pc += 1;
                } else {
                    if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                    return null;
                }
            },
            .neg_set => {
                const cs = self.charsets[inst.data.charset];
                if (pos < self.input.len and !I.charsetContains(cs, self.input[pos])) {
                    pos += 1;
                    pc += 1;
                } else {
                    if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                    return null;
                }
            },
            .optional_char => {
                if (pos < self.input.len and self.input[pos] == inst.data.byte) {
                    pos += 1;
                }
                pc += 1;
            },
            .choice => {
                stack[sp] = .{ .choice = .{ .pos = pos, .pc = inst.data.offset } };
                sp += 1;
                pc += 1;
            },
            .commit => {
                // Pop the backtrack entry (discard it) and jump.
                sp -= 1;
                pc = inst.data.offset;
            },
            .fail => {
                if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                return null;
            },
            .fail_twice => {
                // Pop one entry then fail.
                sp -= 1;
                if (self.backtrack(&stack, &sp, &pc, &pos)) continue;
                return null;
            },
            .jump => {
                pc = inst.data.offset;
            },
            .call => {
                stack[sp] = .{ .ret = pc + 1 };
                sp += 1;
                pc = inst.data.offset;
            },
            .ret => {
                sp -= 1;
                pc = stack[sp].ret;
            },
            .save => {
                const slot = inst.data.slot;
                stack[sp] = .{ .save = .{ .slot = slot, .old = self.captures[slot] } };
                sp += 1;
                self.captures[slot] = pos;
                pc += 1;
            },
            .match => {
                return pos;
            },
        }
    }
    return null;
}

fn backtrack(self: *Vm, stack: *[max_stack]Entry, sp: *usize, pc: *u32, pos: *usize) bool {
    while (sp.* > 0) {
        sp.* -= 1;
        switch (stack[sp.*]) {
            .choice => |c| {
                if (self.trace) |t| {
                    t.writer.print("      backtrack -> pc={d} pos={d}\n", .{ c.pc, c.pos }) catch {};
                }
                pc.* = c.pc;
                pos.* = c.pos;
                return true;
            },
            .ret => {},
            .save => |s| {
                self.captures[s.slot] = s.old;
            },
        }
    }
    return false;
}

/// Return the span for capture group `i`, or null if not captured.
pub fn getCapture(self: *const Vm, i: u16) ?Span {
    const start = self.captures[i * 2] orelse return null;
    const end = self.captures[i * 2 + 1] orelse return null;
    return .{ .start = start, .end = end };
}

/// Return the matched slice for capture group `i`, or null if not captured.
pub fn getCaptureSlice(self: *const Vm, i: u16) ?[]const u8 {
    const span = self.getCapture(i) orelse return null;
    return self.input[span.start..span.end];
}

fn traceStep(self: *Vm, pc: u32, pos: usize, sp: usize, inst: I.Inst) void {
    const t = self.trace orelse return;
    const w = t.writer;
    // pc, stack depth, position, remaining input preview
    w.print("{d:>4}: sp={d:<3} pos={d:<3} ", .{ pc, sp, pos }) catch return;
    // input context: show up to 16 bytes from current position
    w.writeByte('"') catch return;
    const remaining = self.input[pos..];
    const preview = remaining[0..@min(remaining.len, 16)];
    for (preview) |b| {
        if (b >= 0x20 and b < 0x7F)
            w.writeByte(b) catch return
        else
            w.print("\\x{x:0>2}", .{b}) catch return;
    }
    if (remaining.len > 16) w.writeAll("...") catch {};
    w.writeAll("\" ") catch return;
    // opcode
    switch (inst.op) {
        .char => {
            const b = inst.data.byte;
            if (b >= 0x20 and b < 0x7F)
                w.print("char '{c}'", .{b}) catch {}
            else
                w.print("char 0x{x:0>2}", .{b}) catch {};
        },
        .string => {
            const ref = inst.data.string;
            const str = self.string_data[ref.offset..][0..ref.len];
            w.print("string \"{s}\"", .{str}) catch {};
        },
        .any => w.writeAll("any") catch {},
        .set => w.print("set [#{d}]", .{inst.data.charset}) catch {},
        .neg_set => w.print("neg_set [#{d}]", .{inst.data.charset}) catch {},
        .optional_char => {
            const b = inst.data.byte;
            if (b >= 0x20 and b < 0x7F)
                w.print("opt_char '{c}'", .{b}) catch {}
            else
                w.print("opt_char 0x{x:0>2}", .{b}) catch {};
        },
        .choice => w.print("choice -> {d}", .{inst.data.offset}) catch {},
        .commit => w.print("commit -> {d}", .{inst.data.offset}) catch {},
        .fail => w.writeAll("fail") catch {},
        .fail_twice => w.writeAll("fail_twice") catch {},
        .jump => w.print("jump -> {d}", .{inst.data.offset}) catch {},
        .call => w.print("call -> {d}", .{inst.data.offset}) catch {},
        .ret => w.writeAll("ret") catch {},
        .save => w.print("save {d}", .{inst.data.slot}) catch {},
        .match => w.writeAll("match") catch {},
    }
    w.writeByte('\n') catch {};
}

const testing = std.testing;
const Compiler = @import("Compiler.zig");
const EreScanner = @import("../ere/Scanner.zig");
const EreParser = @import("../ere/Parser.zig");
const PegScanner = @import("../peg/Scanner.zig");
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
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), input);
    const result = vm.execute();
    try testing.expectEqual(expected, result);
}

fn expectPegMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = compilePeg(source);
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), input);
    const result = vm.execute();
    try testing.expectEqual(expected, result);
}

test "literal match" {
    try expectMatch("abc", "abc", 3);
    try expectMatch("abc", "abx", null);
    try expectMatch("abc", "ab", null);
}

test "alternation" {
    try expectMatch("a|b", "a", 1);
    try expectMatch("a|b", "b", 1);
    try expectMatch("a|b", "c", null);
}

test "star repetition" {
    try expectMatch("a*", "", 0);
    try expectMatch("a*", "aaa", 3);
    try expectMatch("a*b", "aaab", 4);
    try expectMatch("a*b", "b", 1);
}

test "plus repetition" {
    try expectMatch("a+", "", null);
    try expectMatch("a+", "aaa", 3);
}

test "optional" {
    try expectMatch("a?b", "ab", 2);
    try expectMatch("a?b", "b", 1);
}

test "character class" {
    try expectMatch("[a-z]+", "hello", 5);
    try expectMatch("[a-z]+", "HELLO", null);
    try expectMatch("[0-9]+", "42", 2);
}

test "negated character class" {
    try expectMatch("[^0-9]+", "abc", 3);
    try expectMatch("[^0-9]+", "123", null);
}

test "dot wildcard" {
    try expectMatch("a.c", "abc", 3);
    try expectMatch("a.c", "aXc", 3);
    try expectMatch("a.c", "ac", null);
}

test "grouped alternation" {
    try expectMatch("(ab|cd)e", "abe", 3);
    try expectMatch("(ab|cd)e", "cde", 3);
    try expectMatch("(ab|cd)e", "ace", null);
}

test "interval repetition" {
    try expectMatch("a{2,4}", "a", null);
    try expectMatch("a{2,4}", "aa", 2);
    try expectMatch("a{2,4}", "aaa", 3);
    try expectMatch("a{2,4}", "aaaa", 4);
    try expectMatch("a{2,4}", "aaaaa", 4);
}

test "alternation with common prefix" {
    try expectMatch("https|http", "https", 5);
    try expectMatch("https|http", "http", 4);
    try expectMatch("https|http", "httq", null);
    try expectMatch("httpAB|httpCD", "httpAB", 6);
    try expectMatch("httpAB|httpCD", "httpCD", 6);
    try expectMatch("httpAB|httpCD", "httpXX", null);
    try expectMatch("httpAB|httpCD", "http", null);
    try expectMatch("ab|a", "ab", 2);
    try expectMatch("ab|a", "a", 1);
    try expectMatch("ab|a", "x", null);
}

test "peg: single rule" {
    try expectPegMatch("Main <- \"hello\"", "hello", 5);
    try expectPegMatch("Main <- \"hello\"", "world", null);
}

test "peg: rule references" {
    try expectPegMatch(
        \\Main  <- Greeting " " Name
        \\Greeting <- "hi" / "hello"
        \\Name <- [a-z]+
    , "hi world", 8);
    try expectPegMatch(
        \\Main  <- Greeting " " Name
        \\Greeting <- "hi" / "hello"
        \\Name <- [a-z]+
    , "hello world", 11);
}

test "peg: recursive rules" {
    try expectPegMatch(
        \\Expr   <- Term ("+" Term)*
        \\Term   <- Factor ("*" Factor)*
        \\Factor <- "(" Expr ")" / [0-9]+
    , "1+2*3", 5);
    try expectPegMatch(
        \\Expr   <- Term ("+" Term)*
        \\Term   <- Factor ("*" Factor)*
        \\Factor <- "(" Expr ")" / [0-9]+
    , "(1+2)*3", 7);
}

test "peg: not predicate" {
    try expectPegMatch(
        \\Line <- (!"\n" .)*
    , "hello world", 11);
    try expectPegMatch(
        \\Line <- (!"\n" .)* "\n"
    , "hello\n", 6);
}

test "capture: single group" {
    var compiler = compileEre("a(bc)d");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "abcd");
    try testing.expectEqual(@as(?usize, 4), vm.execute());
    try testing.expectEqualStrings("bc", vm.getCaptureSlice(0).?);
}

test "capture: multiple groups" {
    var compiler = compileEre("(a+)(b+)");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "aaabb");
    try testing.expectEqual(@as(?usize, 5), vm.execute());
    try testing.expectEqualStrings("aaa", vm.getCaptureSlice(0).?);
    try testing.expectEqualStrings("bb", vm.getCaptureSlice(1).?);
}

test "capture: alternation picks correct branch" {
    var compiler = compileEre("(ab)|(cd)");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "cd");
    try testing.expectEqual(@as(?usize, 2), vm.execute());
    // First group did not match.
    try testing.expectEqual(@as(?Span, null), vm.getCapture(0));
    // Second group matched.
    try testing.expectEqualStrings("cd", vm.getCaptureSlice(1).?);
}

test "capture: nested groups" {
    var compiler = compileEre("((a)(b))");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "ab");
    try testing.expectEqual(@as(?usize, 2), vm.execute());
    try testing.expectEqualStrings("ab", vm.getCaptureSlice(0).?);
    try testing.expectEqualStrings("a", vm.getCaptureSlice(1).?);
    try testing.expectEqualStrings("b", vm.getCaptureSlice(2).?);
}

test "capture: group with repetition" {
    var compiler = compileEre("(a+)b");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "aaab");
    try testing.expectEqual(@as(?usize, 4), vm.execute());
    try testing.expectEqualStrings("aaa", vm.getCaptureSlice(0).?);
}

test "capture: no match clears captures" {
    var compiler = compileEre("(a)b");
    var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "ac");
    try testing.expectEqual(@as(?usize, null), vm.execute());
    // Capture should be null after failed match (undone by backtrack).
    try testing.expectEqual(@as(?Span, null), vm.getCapture(0));
}
