const std = @import("std");
const zpars = @import("zpars");
const Abnf = zpars.Abnf;
const Matcher = zpars.Matcher;
const Scanner = zpars.Scanner;
const Parser = zpars.Parser;
const Validator = zpars.Validator;
const Ast = zpars.Ast;

const iterations = 1_000_000;

// --- Benchmark cases ---------------------------------------------------------

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

// --- Comptime parsers (one per case, generated at comptime) ------------------

fn ComptimeParser(comptime idx: usize) type {
    return Abnf.Compile(cases[idx].grammar, cases[idx].rule);
}

// --- Timing helpers ----------------------------------------------------------

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

fn benchRuntime(comptime idx: usize, matcher: *const Matcher) u64 {
    var input: []const u8 = cases[idx].input;
    std.mem.doNotOptimizeAway(&input);

    var timer = std.time.Timer.start() catch unreachable;

    for (0..iterations) |_| {
        const r = matcher.match(cases[idx].rule, input);
        std.mem.doNotOptimizeAway(&r);
    }

    return timer.read();
}

// --- Main --------------------------------------------------------------------

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("\n  {s:<16} {s:>14} {s:>14} {s:>10}\n", .{
        "case", "comptime", "runtime", "ratio",
    });
    try stdout.print("  {s:-<16} {s:->14} {s:->14} {s:->10}\n", .{
        "", "", "", "",
    });

    inline for (0..cases.len) |idx| {
        // Build the runtime matcher once (not timed).
        var arena = std.heap.ArenaAllocator.init(gpa.allocator());
        defer arena.deinit();
        const matcher = try buildMatcher(arena.allocator(), idx);

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
    return Matcher.init(merged);
}
