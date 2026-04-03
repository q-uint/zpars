const std = @import("std");
const zpars = @import("zpars");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        std.process.exit(1);
    }

    const cmd = args[1];
    if (std.mem.eql(u8, cmd, "check")) {
        try runCheck(allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "compile")) {
        try runCompile(allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "fmt")) {
        try runFmt(allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "match")) {
        try runMatch(allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "run")) {
        try runAot(allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "vm")) {
        try runVm(allocator, args[2..]);
    } else {
        printUsage();
        std.process.exit(1);
    }
}

fn printUsage() void {
    std.debug.print(
        \\usage: zpars <command> [options]
        \\
        \\commands:
        \\  check   <file>                     Validate a grammar
        \\  compile <file> -o <output>         Compile grammar to native .zpar blob
        \\  fmt     <file>                     Format a grammar
        \\  match   -r <rule> <file> <input>   Match input against a rule
        \\  run     <blob> <input>             Run a compiled .zpar blob
        \\  vm      [-t] <file> [<input>]      Disassemble (and optionally run) via VM
        \\
        \\Format is auto-detected from file extension (.abnf, .peg, .ere).
        \\
    , .{});
}

const Format = enum { abnf, bnf, peg, ere };

fn detectFormat(filename: []const u8) Format {
    if (std.mem.endsWith(u8, filename, ".peg")) return .peg;
    if (std.mem.endsWith(u8, filename, ".ere")) return .ere;
    if (std.mem.endsWith(u8, filename, ".bnf")) return .bnf;
    return .abnf;
}

fn ParseResult(comptime Scanner: type) type {
    return struct {
        rules: []const zpars.Ast.Rule,
        tokens: @typeInfo(@TypeOf(Scanner.scanTokens)).@"fn".return_type.?,
    };
}

fn parseGrammar(
    comptime Scanner: type,
    comptime Parser: type,
    source: []const u8,
    filename: []const u8,
    stderr: anytype,
) !ParseResult(Scanner) {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    const diags = parser.getDiagnostics();
    if (diags.len > 0) {
        for (diags) |diag| diag.format(source, filename, stderr) catch {};
        stderr.flush() catch {};
        std.process.exit(1);
    }
    return .{ .rules = rules, .tokens = tokens };
}

fn runCheck(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        std.debug.print("usage: zpars check <file>\n", .{});
        std.process.exit(1);
    }

    const filename = args[0];
    const source = try readSource(allocator, filename);
    defer allocator.free(source);

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    const rules = switch (detectFormat(filename)) {
        .abnf => (try parseGrammar(zpars.abnf.Scanner, zpars.abnf.Parser, source, filename, stderr)).rules,
        .bnf => (try parseGrammar(zpars.bnf.Scanner, zpars.bnf.Parser, source, filename, stderr)).rules,
        .peg => (try parseGrammar(zpars.peg.Scanner, zpars.peg.Parser, source, filename, stderr)).rules,
        .ere => (try parseGrammar(zpars.ere.Scanner, zpars.ere.Parser, source, filename, stderr)).rules,
    };

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var validator = zpars.Validator.init(arena.allocator(), rules);
    _ = try validator.validate();

    if (reportValidation(validator.diagnostics.items, filename, stderr)) {
        stderr.flush() catch {};
        std.process.exit(1);
    }
    stderr.flush() catch {};
}

fn runFmt(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        std.debug.print("usage: zpars fmt <file>\n", .{});
        std.process.exit(1);
    }

    const filename = args[0];
    const source = try readSource(allocator, filename);
    defer allocator.free(source);

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    switch (detectFormat(filename)) {
        .abnf => {
            const r = try parseGrammar(zpars.abnf.Scanner, zpars.abnf.Parser, source, filename, stderr);
            zpars.abnf.Formatter.formatGrammar(r.rules, r.tokens, source, stdout) catch {
                std.process.exit(1);
            };
        },
        .bnf => {
            const r = try parseGrammar(zpars.bnf.Scanner, zpars.bnf.Parser, source, filename, stderr);
            zpars.bnf.Formatter.formatGrammar(r.rules, stdout) catch {
                std.process.exit(1);
            };
        },
        .peg => {
            const r = try parseGrammar(zpars.peg.Scanner, zpars.peg.Parser, source, filename, stderr);
            zpars.peg.Formatter.formatGrammar(r.rules, r.tokens, source, stdout) catch {
                std.process.exit(1);
            };
        },
        .ere => {
            const r = try parseGrammar(zpars.ere.Scanner, zpars.ere.Parser, source, filename, stderr);
            zpars.ere.Formatter.formatRule(r.rules[0], stdout) catch {
                std.process.exit(1);
            };
            stdout.writeByte('\n') catch {};
        },
    }
    try stdout.flush();
}

fn runMatch(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var rule_name: ?[]const u8 = null;
    var filename: ?[]const u8 = null;
    var input: ?[]const u8 = null;

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "-r")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("error: -r requires a rule name\n", .{});
                std.process.exit(1);
            }
            rule_name = args[i];
        } else if (filename == null) {
            filename = args[i];
        } else if (input == null) {
            input = args[i];
        }
    }

    const fmt = if (filename) |f| detectFormat(f) else Format.abnf;

    // ERE has a single unnamed rule — -r is optional.
    if (fmt == .ere) {
        if (rule_name == null) rule_name = "";
    }

    if (rule_name == null or filename == null or input == null) {
        std.debug.print("usage: zpars match -r <rule> <file> <input>\n", .{});
        std.process.exit(1);
    }

    const source = try readSource(allocator, filename.?);
    defer allocator.free(source);

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    const rules = switch (fmt) {
        .abnf => (try parseGrammar(zpars.abnf.Scanner, zpars.abnf.Parser, source, filename.?, stderr)).rules,
        .bnf => (try parseGrammar(zpars.bnf.Scanner, zpars.bnf.Parser, source, filename.?, stderr)).rules,
        .peg => (try parseGrammar(zpars.peg.Scanner, zpars.peg.Parser, source, filename.?, stderr)).rules,
        .ere => (try parseGrammar(zpars.ere.Scanner, zpars.ere.Parser, source, filename.?, stderr)).rules,
    };

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var validator = zpars.Validator.init(arena.allocator(), rules);
    const merged = try validator.validate();

    if (reportValidation(validator.diagnostics.items, filename.?, stderr)) {
        stderr.flush() catch {};
        std.process.exit(1);
    }
    stderr.flush() catch {};

    var matcher = zpars.Matcher.init(arena.allocator(), merged);
    const result = matcher.match(rule_name.?, input.?) orelse {
        std.debug.print("no match\n", .{});
        std.process.exit(1);
    };

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("{s}\n", .{result.value});
    try stdout.flush();
}

fn runVm(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var filename: ?[]const u8 = null;
    var input: ?[]const u8 = null;
    var trace_enabled = false;

    for (args) |arg| {
        if (std.mem.eql(u8, arg, "-t")) {
            trace_enabled = true;
        } else if (filename == null) {
            filename = arg;
        } else if (input == null) {
            input = arg;
        }
    }

    if (filename == null) {
        std.debug.print("usage: zpars vm [-t] <file> [<input>]\n", .{});
        std.process.exit(1);
    }

    const source = try readSource(allocator, filename.?);
    defer allocator.free(source);

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    const rules = switch (detectFormat(filename.?)) {
        .abnf => (try parseGrammar(zpars.abnf.Scanner, zpars.abnf.Parser, source, filename.?, stderr)).rules,
        .bnf => (try parseGrammar(zpars.bnf.Scanner, zpars.bnf.Parser, source, filename.?, stderr)).rules,
        .peg => (try parseGrammar(zpars.peg.Scanner, zpars.peg.Parser, source, filename.?, stderr)).rules,
        .ere => (try parseGrammar(zpars.ere.Scanner, zpars.ere.Parser, source, filename.?, stderr)).rules,
    };

    var compiler = zpars.vm.Compiler.compile(rules);
    const code = compiler.getCode();
    const charsets = compiler.getCharsets();
    const string_data = compiler.getStringData();

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const dis = zpars.vm.Disassembler.init(code, charsets, string_data);
    try dis.dump(stdout);

    if (input) |inp| {
        try stdout.print("\ninput: \"{s}\"\n", .{inp});
        if (trace_enabled) try stdout.print("--- trace ---\n", .{});
        try stdout.flush();

        var vm = zpars.vm.Vm.init(code, charsets, string_data, inp);
        if (trace_enabled) {
            vm.trace = .{ .writer = stdout };
        }
        if (vm.execute()) |pos| {
            if (trace_enabled) try stdout.print("--- end ---\n", .{});
            try stdout.print("match: {d} bytes \"{s}\"\n", .{ pos, inp[0..pos] });
            const cap_count = compiler.getCaptureCount();
            for (0..cap_count) |ci| {
                if (vm.getCaptureSlice(@intCast(ci))) |slice| {
                    try stdout.print("  group {d}: \"{s}\"\n", .{ ci, slice });
                } else {
                    try stdout.print("  group {d}: (none)\n", .{ci});
                }
            }
        } else {
            if (trace_enabled) try stdout.print("--- end ---\n", .{});
            try stdout.print("no match\n", .{});
        }
    }

    try stdout.flush();
}

fn runCompile(allocator: std.mem.Allocator, args: []const []const u8) !void {
    var filename: ?[]const u8 = null;
    var output: ?[]const u8 = null;

    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "-o")) {
            i += 1;
            if (i >= args.len) {
                std.debug.print("error: -o requires an output path\n", .{});
                std.process.exit(1);
            }
            output = args[i];
        } else if (filename == null) {
            filename = args[i];
        }
    }

    if (filename == null or output == null) {
        std.debug.print("usage: zpars compile <file> -o <output>\n", .{});
        std.process.exit(1);
    }

    const source = try readSource(allocator, filename.?);
    defer allocator.free(source);

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    const rules = switch (detectFormat(filename.?)) {
        .abnf => (try parseGrammar(zpars.abnf.Scanner, zpars.abnf.Parser, source, filename.?, stderr)).rules,
        .bnf => (try parseGrammar(zpars.bnf.Scanner, zpars.bnf.Parser, source, filename.?, stderr)).rules,
        .peg => (try parseGrammar(zpars.peg.Scanner, zpars.peg.Parser, source, filename.?, stderr)).rules,
        .ere => (try parseGrammar(zpars.ere.Scanner, zpars.ere.Parser, source, filename.?, stderr)).rules,
    };

    var compiler = zpars.vm.Compiler.compile(rules);

    var blob = try zpars.vm.Aot.compileToBlob(
        allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        compiler.getCaptureCount(),
    );
    defer zpars.vm.Aot.freeBlob(allocator, &blob);

    const file = try std.fs.cwd().createFile(output.?, .{});
    defer file.close();
    const data = try zpars.vm.Aot.serializeBlob(allocator, blob);
    defer allocator.free(data);
    try file.writeAll(data);
}

fn runAot(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 2) {
        std.debug.print("usage: zpars run <blob> <input>\n", .{});
        std.process.exit(1);
    }

    const blob_path = args[0];
    const input = args[1];

    const blob_data = try readSource(allocator, blob_path);
    defer allocator.free(blob_data);

    var blob = zpars.vm.Aot.deserializeBlob(allocator, blob_data) catch |err| {
        std.debug.print("error reading blob: {}\n", .{err});
        std.process.exit(1);
    };
    defer zpars.vm.Aot.freeBlob(allocator, &blob);

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    if (zpars.vm.AotRuntime.run(blob, input)) |pos| {
        try stdout.print("match: {d} bytes \"{s}\"\n", .{ pos, input[0..pos] });
    } else {
        try stdout.print("no match\n", .{});
    }
    try stdout.flush();
}

fn readSource(allocator: std.mem.Allocator, filename: []const u8) ![]const u8 {
    return std.fs.cwd().readFileAlloc(allocator, filename, 1024 * 1024);
}

/// Report validation diagnostics. Returns true if any errors were found.
fn reportValidation(items: []const zpars.Validator.Validation, filename: []const u8, stderr: anytype) bool {
    var has_errors = false;
    for (items) |v| {
        switch (v.kind) {
            .duplicate_rule => stderr.print(
                "{s}: warning: duplicate definition of '{s}'\n",
                .{ filename, v.rule_name },
            ) catch {},
            .undefined_rule => {
                stderr.print(
                    "{s}: error: rule '{s}' references undefined rule '{s}'\n",
                    .{ filename, v.rule_name, v.ref_name.? },
                ) catch {};
                has_errors = true;
            },
            .unused_rule => stderr.print(
                "{s}: warning: rule '{s}' is defined but never referenced\n",
                .{ filename, v.rule_name },
            ) catch {},
            .unproductive_rule => {
                stderr.print(
                    "{s}: error: rule '{s}' is unproductive (circular with no terminal escape)\n",
                    .{ filename, v.rule_name },
                ) catch {};
                has_errors = true;
            },
            .left_recursive_rule => {
                stderr.print(
                    "{s}: error: rule '{s}' is left-recursive (calls itself without consuming input)\n",
                    .{ filename, v.rule_name },
                ) catch {};
                has_errors = true;
            },
            .zero_width_loop => {
                stderr.print(
                    "{s}: error: rule '{s}' contains an unbounded repetition whose body can match empty\n",
                    .{ filename, v.rule_name },
                ) catch {};
                has_errors = true;
            },
        }
    }
    return has_errors;
}
