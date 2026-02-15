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

    const source = try std.fs.cwd().readFileAlloc(allocator, args[1], 1024 * 1024);
    defer allocator.free(source);

    var scanner = zpars.Scanner.init(allocator, source);
    defer scanner.deinit();

    const tokens = try scanner.scanTokens();

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    for (tokens) |tok| {
        try stdout.print("[{d}:{d: >3}] {s: <16} \"{s}\"\n", .{
            tok.line,
            tok.start,
            @tagName(tok.tag),
            tok.lexeme(source),
        });
    }
    try stdout.flush();
}
