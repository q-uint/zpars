const std = @import("std");
const zpars = @import("zpars");
const Abnf = zpars.abnf.Compiler;
const Matcher = zpars.Matcher;
const Scanner = zpars.abnf.Scanner;
const Parser = zpars.abnf.Parser;
const Validator = zpars.Validator;
const Ast = zpars.Ast;
const VmCompiler = zpars.vm.Compiler;
const Vm = zpars.vm.Vm;
const Jit = zpars.vm.Jit;
const EreScanner = zpars.ere.Scanner;
const EreParser = zpars.ere.Parser;

const iterations = 1_000_000;


const Case = struct {
    name: []const u8,
    grammar: []const u8,
    rule: []const u8,
    input: []const u8,
};

const cases = [_]Case{
    .{
        .name = "literal",
        .grammar = "greeting = \"hello\"",
        .rule = "greeting",
        .input = "Hello world",
    },
    .{
        .name = "alternation",
        .grammar = "bit = \"0\" / \"1\"",
        .rule = "bit",
        .input = "1",
    },
    .{
        .name = "repetition",
        .grammar = "digits = 1*DIGIT",
        .rule = "digits",
        .input = "1234567890abcdef",
    },
    .{
        .name = "multi-rule",
        .grammar =
        \\number = 1*DIGIT
        \\pair   = number "," number
        ,
        .rule = "pair",
        .input = "42,7!",
    },
    .{
        .name = "HTTP version",
        .grammar =
        \\version = "HTTP/" 1*DIGIT "." 1*DIGIT
        ,
        .rule = "version",
        .input = "HTTP/1.1 OK",
    },
};


fn ComptimeParser(comptime idx: usize) type {
    return Abnf.Compile(cases[idx].grammar, cases[idx].rule);
}


fn benchComptime(comptime idx: usize) u64 {
    const P = ComptimeParser(idx);
    var input: []const u8 = cases[idx].input;
    std.mem.doNotOptimizeAway(&input);

    var timer = std.time.Timer.start() catch unreachable;

    for (0..iterations) |_| {
        const r = P.parse(input);
        std.mem.doNotOptimizeAway(&r);
    }

    return timer.read();
}

fn benchRuntime(comptime idx: usize, matcher: *Matcher) u64 {
    var input: []const u8 = cases[idx].input;
    std.mem.doNotOptimizeAway(&input);

    var timer = std.time.Timer.start() catch unreachable;

    for (0..iterations) |_| {
        const r = matcher.match(cases[idx].rule, input);
        std.mem.doNotOptimizeAway(&r);
    }

    return timer.read();
}


// VM benchmark cases using ERE patterns.
// These exercise all three optimization passes:
//   - string fusion (consecutive chars)
//   - optional char fusion (e?)
//   - charset-to-char (single-char class)
const VmCase = struct {
    name: []const u8,
    pattern: []const u8,
    input: []const u8,
};

const vm_cases = [_]VmCase{
    .{
        // string fusion: "hello" -> 5 chars fused into 1 string instruction
        .name = "string fusion",
        .pattern = "hello",
        .input = "hello",
    },
    .{
        // optional char: a?b -> optional_char + char
        .name = "optional char",
        .pattern = "a?b",
        .input = "ab",
    },
    .{
        // charset to char: [a] treated as char
        .name = "charset->char",
        .pattern = "[a]+",
        .input = "aaaaaaaaaa",
    },
    .{
        // combined: literal prefix + optional + class
        .name = "combined opts",
        .pattern = "HTTP/[0-9].[0-9]",
        .input = "HTTP/1.1",
    },
    .{
        // alternation with string fusion on both branches
        .name = "alt + strings",
        .pattern = "hello|world",
        .input = "world",
    },
    .{
        // longer string match to amplify fusion benefit
        .name = "long literal",
        .pattern = "abcdefghijklmnop",
        .input = "abcdefghijklmnop",
    },
    .{
        // common prefix factoring: "https|http" shares "http" prefix
        .name = "prefix factor",
        .pattern = "https|http",
        .input = "https",
    },
    .{
        // common prefix with both suffixes non-empty
        .name = "prefix both",
        .pattern = "httpAB|httpCD",
        .input = "httpCD",
    },
    .{
        // repetition over charset (no fusion, baseline)
        .name = "charset repeat",
        .pattern = "[a-z]+",
        .input = "thequickbrownfox",
    },
};

fn compileEre(source: []const u8, optimize: bool) VmCompiler {
    var scanner = EreScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, source);
    const rules = parser.parse() catch return VmCompiler{};
    return VmCompiler.compileOpts(rules, .{ .optimize = optimize });
}

fn benchVm(compiler: *const VmCompiler, input: []const u8) u64 {
    var inp: []const u8 = input;
    std.mem.doNotOptimizeAway(&inp);

    var timer = std.time.Timer.start() catch unreachable;

    for (0..iterations) |_| {
        var vm = Vm.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), inp);
        const r = vm.execute();
        std.mem.doNotOptimizeAway(&r);
    }

    return timer.read();
}

fn benchJit(compiler: *const VmCompiler, input: []const u8) u64 {
    var inp: []const u8 = input;
    std.mem.doNotOptimizeAway(&inp);

    var jit = Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), inp) catch unreachable;
    defer jit.deinit();

    var timer = std.time.Timer.start() catch unreachable;

    for (0..iterations) |_| {
        const r = jit.execute();
        std.mem.doNotOptimizeAway(&r);
    }

    return timer.read();
}


pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // -- Comptime vs Runtime (tree-walker) benchmarks --
    try stdout.print("\n  Comptime vs Runtime (tree-walker)\n", .{});
    try stdout.print("  {s:<16} {s:>14} {s:>14} {s:>10}\n", .{
        "case", "comptime", "runtime", "ratio",
    });
    try stdout.print("  {s:-<16} {s:->14} {s:->14} {s:->10}\n", .{
        "", "", "", "",
    });

    inline for (0..cases.len) |idx| {
        // Build the runtime matcher once (not timed).
        var arena = std.heap.ArenaAllocator.init(gpa.allocator());
        defer arena.deinit();
        var matcher = try buildMatcher(arena.allocator(), idx);

        const ct_ns = benchComptime(idx);
        const rt_ns = benchRuntime(idx, &matcher);

        const ct_per_op = ct_ns / iterations;
        const rt_per_op = rt_ns / iterations;
        const ratio: f64 = if (ct_per_op > 0)
            @as(f64, @floatFromInt(rt_per_op)) / @as(f64, @floatFromInt(ct_per_op))
        else
            0;

        try stdout.print("  {s:<16} {d:>11} ns {d:>11} ns {d:>9.1}x\n", .{
            cases[idx].name,
            ct_per_op,
            rt_per_op,
            ratio,
        });
    }

    try stdout.print("\n  ({d} iterations per case)\n", .{iterations});

    // -- VM optimized vs unoptimized benchmarks --
    try stdout.print("\n  VM optimized vs unoptimized\n", .{});
    try stdout.print("  {s:<16} {s:>14} {s:>14} {s:>10} {s:>8} {s:>8}\n", .{
        "case", "optimized", "unoptimized", "speedup", "opt #", "unopt #",
    });
    try stdout.print("  {s:-<16} {s:->14} {s:->14} {s:->10} {s:->8} {s:->8}\n", .{
        "", "", "", "", "", "",
    });

    for (vm_cases) |case| {
        const opt = compileEre(case.pattern, true);
        const unopt = compileEre(case.pattern, false);

        const opt_ns = benchVm(&opt, case.input);
        const unopt_ns = benchVm(&unopt, case.input);

        const opt_per_op = opt_ns / iterations;
        const unopt_per_op = unopt_ns / iterations;
        const speedup: f64 = if (opt_per_op > 0)
            @as(f64, @floatFromInt(unopt_per_op)) / @as(f64, @floatFromInt(opt_per_op))
        else
            0;

        try stdout.print("  {s:<16} {d:>11} ns {d:>11} ns {d:>9.2}x {d:>8} {d:>8}\n", .{
            case.name,
            opt_per_op,
            unopt_per_op,
            speedup,
            opt.code_len,
            unopt.code_len,
        });
    }

    // -- VM interpreter vs JIT benchmarks --
    try stdout.print("\n  VM interpreter vs JIT\n", .{});
    try stdout.print("  {s:<16} {s:>14} {s:>14} {s:>10}\n", .{
        "case", "interpreter", "jit", "speedup",
    });
    try stdout.print("  {s:-<16} {s:->14} {s:->14} {s:->10}\n", .{
        "", "", "", "",
    });

    for (vm_cases) |case| {
        const comp = compileEre(case.pattern, true);

        const vm_ns = benchVm(&comp, case.input);
        const jit_ns = benchJit(&comp, case.input);

        const vm_per_op = vm_ns / iterations;
        const jit_per_op = jit_ns / iterations;
        const speedup: f64 = if (jit_per_op > 0)
            @as(f64, @floatFromInt(vm_per_op)) / @as(f64, @floatFromInt(jit_per_op))
        else
            0;

        try stdout.print("  {s:<16} {d:>11} ns {d:>11} ns {d:>9.2}x\n", .{
            case.name,
            vm_per_op,
            jit_per_op,
            speedup,
        });
    }

    try stdout.print("\n  ({d} iterations per case)\n\n", .{iterations});
    try stdout.flush();
}

fn buildMatcher(allocator: std.mem.Allocator, comptime idx: usize) !Matcher {
    var scanner = Scanner.init(cases[idx].grammar);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, cases[idx].grammar);
    const rules = try parser.parse();
    var validator = Validator.init(allocator, rules);
    const merged = try validator.validate();
    return Matcher.init(allocator, merged);
}
