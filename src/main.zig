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
    const rules = parser.parse() catch |err| switch (err) {
        error.SyntaxError => {
            if (parser.diagnostic) |diag| {
                var stderr_buffer: [4096]u8 = undefined;
                var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
                const stderr = &stderr_writer.interface;
                diag.format(source, filename, stderr) catch {};
                stderr.flush() catch {};
            }
            std.process.exit(1);
        },
        else => return err,
    };

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    for (rules) |rule| {
        try stdout.print("{s}\n", .{rule.name});
    }
    try stdout.flush();
}
