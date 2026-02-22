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
    } else if (std.mem.eql(u8, cmd, "fmt")) {
        try runFmt(allocator, args[2..]);
    } else if (std.mem.eql(u8, cmd, "match")) {
        try runMatch(allocator, args[2..]);
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
        \\  check <file>                       Validate an ABNF grammar
        \\  fmt   <file>                       Format an ABNF grammar
        \\  match -r <rule> <file> <input>     Match input against a rule
        \\
    , .{});
}

// --- check -------------------------------------------------------------------

fn runCheck(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        std.debug.print("usage: zpars check <file>\n", .{});
        std.process.exit(1);
    }

    const filename = args[0];
    const source = try readSource(allocator, filename);
    defer allocator.free(source);

    var scanner = zpars.abnf.Scanner.init(source);
    const tokens = scanner.scanTokens();

    var parser = zpars.abnf.Parser.init(tokens, source);
    const rules = try parser.parse();

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    const diags = parser.getDiagnostics();
    if (diags.len > 0) {
        for (diags) |diag| {
            diag.format(source, filename, stderr) catch {};
        }
        stderr.flush() catch {};
        std.process.exit(1);
    }

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

// --- fmt ---------------------------------------------------------------------

fn runFmt(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 1) {
        std.debug.print("usage: zpars fmt <file>\n", .{});
        std.process.exit(1);
    }

    const filename = args[0];
    const source = try readSource(allocator, filename);
    defer allocator.free(source);

    var scanner = zpars.abnf.Scanner.init(source);
    const tokens = scanner.scanTokens();

    var parser = zpars.abnf.Parser.init(tokens, source);
    const rules = try parser.parse();

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    const diags = parser.getDiagnostics();
    if (diags.len > 0) {
        for (diags) |diag| {
            diag.format(source, filename, stderr) catch {};
        }
        stderr.flush() catch {};
        std.process.exit(1);
    }

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    zpars.abnf.Formatter.formatGrammar(rules, stdout) catch {
        std.process.exit(1);
    };
    try stdout.flush();
}

// --- match -------------------------------------------------------------------

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

    if (rule_name == null or filename == null or input == null) {
        std.debug.print("usage: zpars match -r <rule> <file> <input>\n", .{});
        std.process.exit(1);
    }

    const source = try readSource(allocator, filename.?);
    defer allocator.free(source);

    var scanner = zpars.abnf.Scanner.init(source);
    const tokens = scanner.scanTokens();

    var parser = zpars.abnf.Parser.init(tokens, source);
    const rules = try parser.parse();

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    const diags = parser.getDiagnostics();
    if (diags.len > 0) {
        for (diags) |diag| {
            diag.format(source, filename.?, stderr) catch {};
        }
        stderr.flush() catch {};
        std.process.exit(1);
    }

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var validator = zpars.Validator.init(arena.allocator(), rules);
    const merged = try validator.validate();

    if (reportValidation(validator.diagnostics.items, filename.?, stderr)) {
        stderr.flush() catch {};
        std.process.exit(1);
    }
    stderr.flush() catch {};

    const matcher = zpars.Matcher.init(merged);
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

// --- helpers -----------------------------------------------------------------

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
        }
    }
    return has_errors;
}
