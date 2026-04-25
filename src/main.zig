const std = @import("std");
const zpars = @import("zpars");

const Io = std.Io;

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const allocator = init.gpa;
    const arena = init.arena.allocator();

    const args = try init.minimal.args.toSlice(arena);

    if (args.len < 2) {
        printUsage();
        std.process.exit(1);
    }

    const cmd = args[1];
    if (std.mem.eql(u8, cmd, "check")) {
        try runCheck(io, allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "compile")) {
        try runCompile(io, allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "fmt")) {
        try runFmt(io, allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "match")) {
        try runMatch(io, allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "run")) {
        try runAot(io, allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "tree")) {
        try runTree(io, allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "vm")) {
        try runVm(io, allocator, args[2..]);
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
        \\  tree    [-j] [-p|--jit] <file> <input>  Parse input and print parse tree (-j JSON, -p packrat, --jit native)
        \\  vm      [-t] [-p] <file> [<input>]  Disassemble (and optionally run) via VM (-t trace, -p packrat)
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

fn runCheck(io: Io, allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    if (args.len < 1) {
        std.debug.print("usage: zpars check <file>\n", .{});
        std.process.exit(1);
    }

    const filename = args[0];
    const source = try readSource(io, allocator, filename);
    defer allocator.free(source);

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = Io.File.stderr().writer(io, &stderr_buffer);
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

fn runFmt(io: Io, allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    if (args.len < 1) {
        std.debug.print("usage: zpars fmt <file>\n", .{});
        std.process.exit(1);
    }

    const filename = args[0];
    const source = try readSource(io, allocator, filename);
    defer allocator.free(source);

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = Io.File.stderr().writer(io, &stderr_buffer);
    const stderr = &stderr_writer.interface;

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = Io.File.stdout().writer(io, &stdout_buffer);
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

fn runMatch(io: Io, allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
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

    // ERE has a single unnamed rule - -r is optional.
    if (fmt == .ere) {
        if (rule_name == null) rule_name = "";
    }

    if (rule_name == null or filename == null or input == null) {
        std.debug.print("usage: zpars match -r <rule> <file> <input>\n", .{});
        std.process.exit(1);
    }

    const source = try readSource(io, allocator, filename.?);
    defer allocator.free(source);

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = Io.File.stderr().writer(io, &stderr_buffer);
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

    var matcher = try zpars.Matcher.init(arena.allocator(), merged);
    const result = matcher.match(rule_name.?, input.?) orelse {
        std.debug.print("no match\n", .{});
        std.process.exit(1);
    };

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("{s}\n", .{result.value});
    try stdout.flush();
}

fn runTree(io: Io, allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    var filename: ?[]const u8 = null;
    var input: ?[]const u8 = null;
    var json_output = false;
    var packrat_enabled = false;
    var jit_enabled = false;

    for (args) |arg| {
        if (std.mem.eql(u8, arg, "-j") or std.mem.eql(u8, arg, "--json")) {
            json_output = true;
        } else if (std.mem.eql(u8, arg, "-p")) {
            packrat_enabled = true;
        } else if (std.mem.eql(u8, arg, "--jit")) {
            jit_enabled = true;
        } else if (filename == null) {
            filename = arg;
        } else if (input == null) {
            input = arg;
        }
    }

    if (filename == null or input == null) {
        std.debug.print("usage: zpars tree [-j] [-p|--jit] <file> <input>\n", .{});
        std.process.exit(1);
    }
    if (jit_enabled and packrat_enabled) {
        std.debug.print("error: --jit and -p are mutually exclusive (the JIT does not implement packrat memoization)\n", .{});
        std.process.exit(1);
    }

    const source = try readSource(io, allocator, filename.?);
    defer allocator.free(source);

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = Io.File.stderr().writer(io, &stderr_buffer);
    const stderr = &stderr_writer.interface;

    // The tree subcommand enables recovery directives (#@) for the PEG
    // front-end so users can author grammars with labeled-failure
    // recovery and see ERROR / MISSING / partial nodes in the output.
    // Other formats (ABNF/BNF/ERE) do not have a recovery surface
    // syntax and use their default parsers.
    const RecoveryPegParser = zpars.peg.ParserWith(.{ .recovery = true });

    const rules = switch (detectFormat(filename.?)) {
        .abnf => (try parseGrammar(zpars.abnf.Scanner, zpars.abnf.Parser, source, filename.?, stderr)).rules,
        .bnf => (try parseGrammar(zpars.bnf.Scanner, zpars.bnf.Parser, source, filename.?, stderr)).rules,
        .peg => (try parseGrammar(zpars.peg.Scanner, RecoveryPegParser, source, filename.?, stderr)).rules,
        .ere => (try parseGrammar(zpars.ere.Scanner, zpars.ere.Parser, source, filename.?, stderr)).rules,
    };

    var compiler = try zpars.vm.Compiler.compileOpts(rules, .{
        .memoize = packrat_enabled,
        .memoize_captures = packrat_enabled,
        .rules_as_captures = true,
    });

    // Build name slices indexed by rule_id and label_id for the tree
    // printer. Labels are populated only when the grammar actually uses
    // recovery directives; for plain grammars the labels slice is empty.
    var names_buf: [256][]const u8 = undefined;
    const names = names_buf[0..compiler.rule_count];
    for (0..compiler.rule_count) |i| names[i] = compiler.getRuleName(@intCast(i));

    var labels_buf: [256][]const u8 = undefined;
    const labels = labels_buf[0..compiler.label_count];
    for (0..compiler.label_count) |i| labels[i] = compiler.getLabelName(@intCast(i));

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    var tree = if (jit_enabled) blk: {
        const EventJit = zpars.vm.Jit.JitWith(.{ .capture_events = true });
        var jit = try EventJit.initEvents(
            allocator,
            compiler.getCode(),
            compiler.getCharsets(),
            compiler.getStringData(),
            input.?,
        );
        defer jit.deinit();
        if (jit.execute() == null) {
            try stdout.print("no match\n", .{});
            try stdout.flush();
            std.process.exit(1);
        }
        break :blk try jit.buildCaptureTree(allocator);
    } else blk: {
        const EventVm = zpars.vm.VmWith(.{ .capture_events = true });
        var vm = if (packrat_enabled)
            try EventVm.initPackrat(
                allocator,
                compiler.getCode(),
                compiler.getCharsets(),
                compiler.getStringData(),
                compiler.getMemoRuleCount(),
                input.?,
            )
        else
            EventVm.initEvents(
                allocator,
                compiler.getCode(),
                compiler.getCharsets(),
                compiler.getStringData(),
                input.?,
            );
        defer vm.deinit();
        if ((try vm.execute()) == null) {
            try stdout.print("no match\n", .{});
            try stdout.flush();
            std.process.exit(1);
        }
        break :blk try vm.buildCaptureTree(allocator);
    };
    defer tree.deinit();

    const tree_names: zpars.vm.CaptureTree.Names = .{ .rules = names, .labels = labels };
    if (json_output) {
        try tree.writeJson(stdout, tree_names);
    } else {
        try tree.writeSExp(stdout, tree_names);
    }
    try stdout.writeByte('\n');
    try stdout.flush();
}

fn runVm(io: Io, allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    var filename: ?[]const u8 = null;
    var input: ?[]const u8 = null;
    var trace_enabled = false;
    var packrat_enabled = false;

    for (args) |arg| {
        if (std.mem.eql(u8, arg, "-t")) {
            trace_enabled = true;
        } else if (std.mem.eql(u8, arg, "-p")) {
            packrat_enabled = true;
        } else if (filename == null) {
            filename = arg;
        } else if (input == null) {
            input = arg;
        }
    }

    if (filename == null) {
        std.debug.print("usage: zpars vm [-t] [-p] <file> [<input>]\n", .{});
        std.process.exit(1);
    }

    const source = try readSource(io, allocator, filename.?);
    defer allocator.free(source);

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = Io.File.stderr().writer(io, &stderr_buffer);
    const stderr = &stderr_writer.interface;

    const rules = switch (detectFormat(filename.?)) {
        .abnf => (try parseGrammar(zpars.abnf.Scanner, zpars.abnf.Parser, source, filename.?, stderr)).rules,
        .bnf => (try parseGrammar(zpars.bnf.Scanner, zpars.bnf.Parser, source, filename.?, stderr)).rules,
        .peg => (try parseGrammar(zpars.peg.Scanner, zpars.peg.Parser, source, filename.?, stderr)).rules,
        .ere => (try parseGrammar(zpars.ere.Scanner, zpars.ere.Parser, source, filename.?, stderr)).rules,
    };

    var compiler = try zpars.vm.Compiler.compileOpts(rules, .{ .memoize = packrat_enabled });
    const code = compiler.getCode();
    const charsets = compiler.getCharsets();
    const string_data = compiler.getStringData();

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    const dis = zpars.vm.Disassembler.init(code, charsets, string_data);
    try dis.dump(stdout);

    if (input) |inp| {
        try stdout.print("\ninput: \"{s}\"\n", .{inp});
        if (trace_enabled) try stdout.print("--- trace ---\n", .{});
        try stdout.flush();

        var vm = if (packrat_enabled)
            try zpars.vm.Vm.initPackrat(
                allocator,
                code,
                charsets,
                string_data,
                compiler.getMemoRuleCount(),
                inp,
            )
        else
            zpars.vm.Vm.init(code, charsets, string_data, inp);
        defer vm.deinit();
        if (trace_enabled) {
            vm.trace = .{ .writer = stdout };
        }
        if (try vm.execute()) |pos| {
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

fn runCompile(io: Io, allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
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

    const source = try readSource(io, allocator, filename.?);
    defer allocator.free(source);

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = Io.File.stderr().writer(io, &stderr_buffer);
    const stderr = &stderr_writer.interface;

    const rules = switch (detectFormat(filename.?)) {
        .abnf => (try parseGrammar(zpars.abnf.Scanner, zpars.abnf.Parser, source, filename.?, stderr)).rules,
        .bnf => (try parseGrammar(zpars.bnf.Scanner, zpars.bnf.Parser, source, filename.?, stderr)).rules,
        .peg => (try parseGrammar(zpars.peg.Scanner, zpars.peg.Parser, source, filename.?, stderr)).rules,
        .ere => (try parseGrammar(zpars.ere.Scanner, zpars.ere.Parser, source, filename.?, stderr)).rules,
    };

    var compiler = try zpars.vm.Compiler.compile(rules);

    var blob = try zpars.vm.Aot.compileToBlob(
        allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        compiler.getCaptureCount(),
    );
    defer zpars.vm.Aot.freeBlob(allocator, &blob);

    const file = try Io.Dir.cwd().createFile(io, output.?, .{});
    defer file.close(io);
    const data = try zpars.vm.Aot.serializeBlob(allocator, blob);
    defer allocator.free(data);
    try file.writeStreamingAll(io, data);
}

fn runAot(io: Io, allocator: std.mem.Allocator, args: []const [:0]const u8) !void {
    if (args.len < 2) {
        std.debug.print("usage: zpars run <blob> <input>\n", .{});
        std.process.exit(1);
    }

    const blob_path = args[0];
    const input = args[1];

    const blob_data = try readSource(io, allocator, blob_path);
    defer allocator.free(blob_data);

    var blob = zpars.vm.Aot.deserializeBlob(allocator, blob_data) catch |err| {
        std.debug.print("error reading blob: {}\n", .{err});
        std.process.exit(1);
    };
    defer zpars.vm.Aot.freeBlob(allocator, &blob);

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_writer.interface;

    if (zpars.vm.AotRuntime.run(blob, input)) |pos| {
        try stdout.print("match: {d} bytes \"{s}\"\n", .{ pos, input[0..pos] });
    } else {
        try stdout.print("no match\n", .{});
    }
    try stdout.flush();
}

fn readSource(io: Io, allocator: std.mem.Allocator, filename: []const u8) ![]const u8 {
    return Io.Dir.cwd().readFileAlloc(io, filename, allocator, .limited(1024 * 1024));
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
