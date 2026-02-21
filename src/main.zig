const std = @import("std");
const zpars = @import("zpars");

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("usage: zpars <file.abnf>\n", .{});
        std.process.exit(1);
    }

    const filename = args[1];
    const source = try std.fs.cwd().readFileAlloc(allocator, filename, 1024 * 1024);
    defer allocator.free(source);

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const aa = arena.allocator();

    var scanner = zpars.Scanner.init(aa, source);
    const tokens = try scanner.scanTokens();

    var parser = zpars.Parser.init(aa, tokens, source);
    const rules = try parser.parse();

    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    if (parser.diagnostics.items.len > 0) {
        for (parser.diagnostics.items) |diag| {
            diag.format(source, filename, stderr) catch {};
        }
        stderr.flush() catch {};
        std.process.exit(1);
    }

    var validator = zpars.Validator.init(aa, rules);
    const merged = try validator.validate();

    var has_errors = false;
    for (validator.diagnostics.items) |v| {
        switch (v.kind) {
            .duplicate_rule => stderr.print("{s}: warning: duplicate definition of '{s}'\n", .{ filename, v.rule_name }) catch {},
            .undefined_rule => {
                stderr.print("{s}: error: rule '{s}' references undefined rule '{s}'\n", .{ filename, v.rule_name, v.ref_name.? }) catch {};
                has_errors = true;
            },
            .unused_rule => stderr.print("{s}: warning: rule '{s}' is defined but never referenced\n", .{ filename, v.rule_name }) catch {},
            .unproductive_rule => {
                stderr.print("{s}: error: rule '{s}' is unproductive (circular with no terminal escape)\n", .{ filename, v.rule_name }) catch {};
                has_errors = true;
            },
        }
    }
    stderr.flush() catch {};
    if (has_errors) std.process.exit(1);

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    for (merged) |rule| {
        try stdout.print("{s}\n", .{rule.name});
    }
    try stdout.flush();
}
