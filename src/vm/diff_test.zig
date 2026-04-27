//! Differential testing across the three execution backends:
//! VM (`Vm.zig`), JIT (`Jit.zig`), and AOT (`Aot.zig` + `AotRuntime.zig`).
//!
//! Same grammar, same input, three engines. Each `diffCheck` call runs
//! the input through two configurations:
//!
//!   `diffCheckPlain` — plain config: VM, JIT, and AOT must return the
//!   same `?usize` end position.
//!
//!   `diffCheckEvents` — `capture_events = true`: same end-position
//!   agreement, plus all three engines must build identical
//!   `CaptureTree.Tree`s (compared via `writeSExp`).
//!
//! Smoke cases live alongside the harness so a fuzz failure means a
//! real backend bug, not a harness bug. The fuzz target uses a fixed
//! arithmetic grammar and `std.testing.fuzz` to drive random byte
//! input through `diffCheck`. Run with `zig build test --fuzz` for
//! coverage-guided exploration; plain `zig build test` exercises the
//! seed corpus only.

const std = @import("std");
const testing = std.testing;
const Compiler = @import("Compiler.zig").Compiler;
const Vm = @import("Vm.zig");
const Jit = @import("Jit.zig");
const Aot = @import("Aot.zig");
const AotRuntime = @import("AotRuntime.zig");
const PegScanner = @import("../peg/Scanner.zig").Scanner;
const PegParser = @import("../peg/Parser.zig").Parser;

const EventVm = Vm.VmWith(.{ .capture_events = true });
const EventJit = Jit.JitWith(.{ .capture_events = true });
const EventAot = AotRuntime.EngineWith(.{ .capture_events = true });

// Memoize is a runtime-only knob in the VM (no comptime flag — selected
// by `initPackrat` plus a non-zero memo_rule_count). The JIT does carry
// a comptime flag because the helper-pointer wiring differs.
const MemoJit = Jit.JitWith(.{ .memoize = true });
const MemoEventJit = Jit.JitWith(.{ .capture_events = true, .memoize = true });

const MemoAot = AotRuntime.EngineWith(.{ .memoize = true });
const MemoEventAot = AotRuntime.EngineWith(.{ .capture_events = true, .memoize = true });

/// Cap on serialized-tree size in fuzz iterations. Any input that
/// produces a larger tree blows the buffer and we'd rather see a
/// `WriteFailed` than silently truncate the comparison.
const tree_buf_size = 65536;

/// Run `(grammar, input)` through all three backends with the
/// agreement contract above, asserting on first mismatch.
fn diffCheck(
    allocator: std.mem.Allocator,
    source: []const u8,
    input: []const u8,
) !void {
    var scanner = PegScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, source);
    const rules = try parser.parse();

    try diffCheckPlain(allocator, rules, input);
    try diffCheckEvents(allocator, rules, input);
    try diffCheckMemoize(allocator, rules, input);
    try diffCheckMemoizeEvents(allocator, rules, input);
}

fn diffCheckMemoize(
    allocator: std.mem.Allocator,
    rules: []const @import("../Ast.zig").Rule,
    input: []const u8,
) !void {
    var c = try Compiler.compileOpts(rules, .{ .memoize = true });

    var vm = try Vm.Vm.initPackrat(
        allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        c.getMemoRuleCount(),
        input,
    );
    defer vm.deinit();
    const vm_end = try vm.execute();

    var jit = try MemoJit.initPackrat(
        allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        c.getMemoRuleCount(),
        input,
    );
    defer jit.deinit();
    const jit_end = jit.execute();
    try testing.expectEqual(vm_end, jit_end);

    var blob = try Aot.compileToBlobWith(
        .{ .memoize = true },
        allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        c.getCaptureCount(),
        c.getMemoRuleCount(),
    );
    defer Aot.freeBlob(allocator, &blob);
    const data = try Aot.serializeBlob(allocator, blob);
    defer allocator.free(data);
    var blob2 = try Aot.deserializeBlob(allocator, data);
    defer Aot.freeBlob(allocator, &blob2);
    var aot = try MemoAot.initPackrat(allocator, blob2);
    defer aot.deinit();
    const aot_end = aot.execute(input);
    try testing.expectEqual(jit_end, aot_end);
}

fn diffCheckMemoizeEvents(
    allocator: std.mem.Allocator,
    rules: []const @import("../Ast.zig").Rule,
    input: []const u8,
) !void {
    var c = try Compiler.compileOpts(rules, .{
        .memoize = true,
        .memoize_captures = true,
        .rules_as_captures = true,
    });

    var vm = try EventVm.initPackrat(
        allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        c.getMemoRuleCount(),
        input,
    );
    defer vm.deinit();
    const vm_end = try vm.execute();

    var jit = try MemoEventJit.initPackrat(
        allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        c.getMemoRuleCount(),
        input,
    );
    defer jit.deinit();
    const jit_end = jit.execute();
    try testing.expectEqual(vm_end, jit_end);

    var blob = try Aot.compileToBlobWith(
        .{ .capture_events = true, .memoize = true },
        allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        c.getCaptureCount(),
        c.getMemoRuleCount(),
    );
    defer Aot.freeBlob(allocator, &blob);
    const data = try Aot.serializeBlob(allocator, blob);
    defer allocator.free(data);
    var blob2 = try Aot.deserializeBlob(allocator, data);
    defer Aot.freeBlob(allocator, &blob2);
    var aot = try MemoEventAot.initPackrat(allocator, blob2);
    defer aot.deinit();
    const aot_end = aot.execute(input);
    try testing.expectEqual(jit_end, aot_end);

    if (vm_end == null) return;

    var vm_tree = try vm.buildCaptureTree(allocator);
    defer vm_tree.deinit();
    var jit_tree = try jit.buildCaptureTree(allocator);
    defer jit_tree.deinit();
    var aot_tree = try aot.buildCaptureTree(allocator);
    defer aot_tree.deinit();

    var vm_buf: [tree_buf_size]u8 = undefined;
    var vm_stream: std.Io.Writer = .fixed(&vm_buf);
    try vm_tree.writeSExp(&vm_stream, .{});

    var jit_buf: [tree_buf_size]u8 = undefined;
    var jit_stream: std.Io.Writer = .fixed(&jit_buf);
    try jit_tree.writeSExp(&jit_stream, .{});

    var aot_buf: [tree_buf_size]u8 = undefined;
    var aot_stream: std.Io.Writer = .fixed(&aot_buf);
    try aot_tree.writeSExp(&aot_stream, .{});

    try testing.expectEqualStrings(vm_stream.buffered(), jit_stream.buffered());
    try testing.expectEqualStrings(jit_stream.buffered(), aot_stream.buffered());
}

fn diffCheckPlain(
    allocator: std.mem.Allocator,
    rules: []const @import("../Ast.zig").Rule,
    input: []const u8,
) !void {
    var c = try Compiler.compile(rules);

    var vm = Vm.Vm.init(c.getCode(), c.getCharsets(), c.getStringData(), input);
    const vm_end = try vm.execute();

    var jit = try Jit.Jit.init(c.getCode(), c.getCharsets(), c.getStringData(), input);
    defer jit.deinit();
    const jit_end = jit.execute();
    try testing.expectEqual(vm_end, jit_end);

    var blob = try Aot.compileToBlob(
        allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        c.getCaptureCount(),
    );
    defer Aot.freeBlob(allocator, &blob);
    // Round-trip through serialization to mirror real deployment paths.
    const data = try Aot.serializeBlob(allocator, blob);
    defer allocator.free(data);
    var blob2 = try Aot.deserializeBlob(allocator, data);
    defer Aot.freeBlob(allocator, &blob2);
    var engine = try AotRuntime.Engine.init(blob2);
    defer engine.deinit();
    const aot_end = engine.execute(input);
    try testing.expectEqual(jit_end, aot_end);
}

fn diffCheckEvents(
    allocator: std.mem.Allocator,
    rules: []const @import("../Ast.zig").Rule,
    input: []const u8,
) !void {
    var c = try Compiler.compileOpts(rules, .{ .rules_as_captures = true });

    var vm = EventVm.initEvents(allocator, c.getCode(), c.getCharsets(), c.getStringData(), input);
    defer vm.deinit();
    const vm_end = try vm.execute();

    var jit = try EventJit.initEvents(allocator, c.getCode(), c.getCharsets(), c.getStringData(), input);
    defer jit.deinit();
    const jit_end = jit.execute();
    try testing.expectEqual(vm_end, jit_end);

    var blob = try Aot.compileToBlobWith(
        .{ .capture_events = true },
        allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        c.getCaptureCount(),
        0,
    );
    defer Aot.freeBlob(allocator, &blob);
    const data = try Aot.serializeBlob(allocator, blob);
    defer allocator.free(data);
    var blob2 = try Aot.deserializeBlob(allocator, data);
    defer Aot.freeBlob(allocator, &blob2);
    var aot = try EventAot.initEvents(allocator, blob2);
    defer aot.deinit();
    const aot_end = aot.execute(input);
    try testing.expectEqual(jit_end, aot_end);

    // Tree comparison only meaningful when all succeeded. On
    // mismatch the trees are undefined; the position assertions
    // above already caught it.
    if (vm_end == null) return;

    var vm_tree = try vm.buildCaptureTree(allocator);
    defer vm_tree.deinit();
    var jit_tree = try jit.buildCaptureTree(allocator);
    defer jit_tree.deinit();
    var aot_tree = try aot.buildCaptureTree(allocator);
    defer aot_tree.deinit();

    var vm_buf: [tree_buf_size]u8 = undefined;
    var vm_stream: std.Io.Writer = .fixed(&vm_buf);
    try vm_tree.writeSExp(&vm_stream, .{});

    var jit_buf: [tree_buf_size]u8 = undefined;
    var jit_stream: std.Io.Writer = .fixed(&jit_buf);
    try jit_tree.writeSExp(&jit_stream, .{});

    var aot_buf: [tree_buf_size]u8 = undefined;
    var aot_stream: std.Io.Writer = .fixed(&aot_buf);
    try aot_tree.writeSExp(&aot_stream, .{});

    try testing.expectEqualStrings(vm_stream.buffered(), jit_stream.buffered());
    try testing.expectEqualStrings(jit_stream.buffered(), aot_stream.buffered());
}

test "diff: literal grammar matches" {
    try diffCheck(testing.allocator,
        \\Main <- "abc"
    , "abc");
}

test "diff: literal grammar rejects" {
    try diffCheck(testing.allocator,
        \\Main <- "abc"
    , "abx");
}

test "diff: arithmetic grammar matches" {
    try diffCheck(testing.allocator,
        \\Expr   <- Term ("+" Term)*
        \\Term   <- Factor ("*" Factor)*
        \\Factor <- "(" Expr ")" / [0-9]+
    , "1+2*3");
}

test "diff: arithmetic divergence candidates" {
    const candidates = [_][]const u8{
        "(1)",
        "((1))",
        "(1)+2",
        "(1)*2",
        "1+(2)",
        "1*(2)",
        "1+(2*3)",
        "1*(2+3)",
        "(1+2)*3",
        "((1+2))",
        "(1+2)+3",
        "(1)+2*3",
        "((1))+2",
        "((1+2)*3)",
        "(1+(2*3))",
        "(((1)))",
        "1+",
        "1*",
        "(",
        "()",
        "(1",
        "(1+",
        "(1+2",
        "1+(",
        "1+()",
        "1+(2",
    };
    for (candidates) |c| {
        diffCheck(testing.allocator,
            \\Expr   <- Term ("+" Term)*
            \\Term   <- Factor ("*" Factor)*
            \\Factor <- "(" Expr ")" / [0-9]+
        , c) catch |err| {
            std.debug.print("FAIL on \"{s}\": {}\n", .{ c, err });
            return err;
        };
    }
}

const arith_grammar =
    \\Expr   <- Term ("+" Term)*
    \\Term   <- Factor ("*" Factor)*
    \\Factor <- "(" Expr ")" / [0-9]+
;

const FuzzCtx = struct { allocator: std.mem.Allocator };

/// Bytes the arithmetic grammar can actually consume. Uniform random
/// bytes from `Smith.slice` get rejected on the first byte ~99% of the
/// time, which leaves the parser's interesting paths almost entirely
/// unexplored. Folding each random byte through this alphabet keeps
/// the input coverage-shaped (mutations still produce structured
/// changes) while pushing the parser deep into Term/Factor branches.
const arith_alphabet = "0123456789+*() ";

fn biasAlphabet(buf: []u8, alphabet: []const u8) void {
    for (buf) |*b| b.* = alphabet[b.* % alphabet.len];
}

fn fuzzArith(ctx: *const FuzzCtx, smith: *std.testing.Smith) anyerror!void {
    var buf: [256]u8 = undefined;
    const len = smith.slice(&buf);
    const input = buf[0..len];
    biasAlphabet(input, arith_alphabet);
    try diffCheck(ctx.allocator, arith_grammar, input);
}

test "fuzz: arithmetic grammar - VM/JIT/AOT agree on random input" {
    const ctx = FuzzCtx{ .allocator = testing.allocator };
    try std.testing.fuzz(&ctx, fuzzArith, .{
        .corpus = &.{
            "",
            "1",
            "1+2",
            "1*2",
            "(1)",
            "1+2*3",
            "((1+2)*3)",
            "1+",
            "+1",
            "1+2+3+4+5",
        },
    });
}

const json_grammar =
    \\Value   <- Spacing (Object / Array / String / Number / True / False / Null) Spacing
    \\Object  <- "{" Spacing (Pair ("," Spacing Pair)*)? "}" Spacing
    \\Pair    <- String Spacing ":" Spacing Value
    \\Array   <- "[" Spacing (Value ("," Spacing Value)*)? "]" Spacing
    \\String  <- '"' (!'"' !'\\' . / '\\' ["\\/bfnrt])* '"'
    \\Number  <- "-"? ("0" / [1-9] [0-9]*) ("." [0-9]+)? ([eE] [+\-]? [0-9]+)?
    \\True    <- "true"
    \\False   <- "false"
    \\Null    <- "null"
    \\Spacing <- [ \t\n\r]*
;

/// Bytes the JSON grammar can actually consume:
///   - structural punctuation: `{}[]:,`
///   - string framing + escape backslash: `"\\`
///   - escape-sequence trailers: `/bfnrt` (full set per the grammar's
///     `["\\/bfnrt]` class; `f`/`n`/`r`/`t` come for free via the
///     `true`/`false`/`null` keyword letters)
///   - keyword letters: `truefalsn`
///   - digits + sign + decimal + exponent: `0-9 - . e E +`
///   - whitespace: ` \t\n\r`
/// Inside strings the `.` opcode matches any of these bytes too, so
/// the alphabet doubles as string-content coverage.
const json_alphabet = "{}[]:,\"\\/btruefalsn0123456789-.eE+ \t\n\r";

fn fuzzJson(ctx: *const FuzzCtx, smith: *std.testing.Smith) anyerror!void {
    var buf: [256]u8 = undefined;
    const len = smith.slice(&buf);
    const input = buf[0..len];
    biasAlphabet(input, json_alphabet);
    try diffCheck(ctx.allocator, json_grammar, input);
}

test "fuzz: json grammar - VM/JIT/AOT agree on random input" {
    const ctx = FuzzCtx{ .allocator = testing.allocator };
    try std.testing.fuzz(&ctx, fuzzJson, .{
        .corpus = &.{
            "",
            "null",
            "true",
            "false",
            "0",
            "-1",
            "1.5",
            "1e10",
            "\"hi\"",
            "\"a\\nb\"",
            "[]",
            "[1,2,3]",
            "{}",
            "{\"a\":1}",
            "{\"a\":[1,2],\"b\":null}",
            "[[[]]]",
            "{",
            "[",
            "[1,",
            "{\"a\":",
        },
    });
}

test "diff: json divergence candidates" {
    const candidates = [_][]const u8{
        "[1,2,3,4,5,6,7,8",
        "[1,2,3,4,5,6,7]",
        "[1,2,3,4]",
        "{\"a\":1,\"b\":2,",
        "{\"a\":1234567890",
        "{\"a\":[1,2,3,4]}",
        "[\"abc\",\"def\"]",
        "1.5e+10",
        "1.5e+10,",
        "1234567890123456",
        "[[[[1,2,3,4]]]]",
        "[[[[1,2,3,4]]]",
        "{\"a\":{\"b\":{\"c\":1}}}",
        "{\"a\":{\"b\":{\"c\":1}}",
        "[1.5e10,2.5e20]",
        "[1.5e10,2.5e20",
        "\"\\n\\t\\r\\b\\f\\\\\"",
        "\"\\n\\t\\r\\b\\f\\\\",
    };
    for (candidates) |c| {
        diffCheck(testing.allocator, json_grammar, c) catch |err| {
            std.debug.print("FAIL on \"{s}\" ({d} bytes): {}\n", .{ c, c.len, err });
            return err;
        };
    }
}

