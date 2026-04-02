/// Pretty-prints bytecode produced by the Compiler.
const std = @import("std");
const I = @import("Instruction.zig");

const Disassembler = @This();

code: []const I.Inst,
charsets: []const I.Charset,
string_data: []const u8,

pub fn init(code: []const I.Inst, charsets: []const I.Charset, string_data: []const u8) Disassembler {
    return .{ .code = code, .charsets = charsets, .string_data = string_data };
}

pub fn dump(self: *const Disassembler, writer: anytype) !void {
    for (self.code, 0..) |inst, i| {
        try writer.print("{d:>4}: ", .{i});
        switch (inst.op) {
            .char => {
                const b = inst.data.byte;
                if (b >= 0x20 and b < 0x7F)
                    try writer.print("char    '{c}'\n", .{b})
                else
                    try writer.print("char    0x{x:0>2}\n", .{b});
            },
            .string => {
                const ref = inst.data.string;
                const str = self.string_data[ref.offset..][0..ref.len];
                try writer.print("string  \"{s}\"\n", .{str});
            },
            .any => try writer.writeAll("any\n"),
            .set => {
                try writer.print("set     ", .{});
                try self.printCharset(writer, inst.data.charset);
                try writer.writeByte('\n');
            },
            .neg_set => {
                try writer.print("neg_set ", .{});
                try self.printCharset(writer, inst.data.charset);
                try writer.writeByte('\n');
            },
            .optional_char => {
                const b = inst.data.byte;
                if (b >= 0x20 and b < 0x7F)
                    try writer.print("opt_char '{c}'\n", .{b})
                else
                    try writer.print("opt_char 0x{x:0>2}\n", .{b});
            },
            .choice => try writer.print("choice  -> {d}\n", .{inst.data.offset}),
            .commit => try writer.print("commit  -> {d}\n", .{inst.data.offset}),
            .fail => try writer.writeAll("fail\n"),
            .fail_twice => try writer.writeAll("fail_twice\n"),
            .jump => try writer.print("jump    -> {d}\n", .{inst.data.offset}),
            .call => try writer.print("call    -> {d}\n", .{inst.data.offset}),
            .ret => try writer.writeAll("ret\n"),
            .save => try writer.print("save    {d}\n", .{inst.data.slot}),
            .match => try writer.writeAll("match\n"),
        }
    }
}

fn printCharset(self: *const Disassembler, writer: anytype, idx: u16) !void {
    const cs = self.charsets[idx];
    try writer.writeByte('[');
    var in_range = false;
    var range_start: u8 = 0;
    for (0..256) |bi| {
        const b: u8 = @intCast(bi);
        const set = I.charsetContains(cs, b);
        if (set and !in_range) {
            range_start = b;
            in_range = true;
        } else if (!set and in_range) {
            try printRange(writer, range_start, b - 1);
            in_range = false;
        }
    }
    if (in_range) try printRange(writer, range_start, 255);
    try writer.writeByte(']');
}

fn printRange(writer: anytype, lo: u8, hi: u8) !void {
    try printByte(writer, lo);
    if (hi > lo) {
        try writer.writeByte('-');
        try printByte(writer, hi);
    }
}

fn printByte(writer: anytype, b: u8) !void {
    if (b >= 0x20 and b < 0x7F)
        try writer.writeByte(b)
    else
        try writer.print("\\x{x:0>2}", .{b});
}

const testing = std.testing;
const Compiler = @import("Compiler.zig");
const EreScanner = @import("../ere/Scanner.zig");
const EreParser = @import("../ere/Parser.zig");

test "disassemble literal" {
    var scanner = EreScanner.init("abc");
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, "abc");
    const rules = try parser.parse();
    var compiler = Compiler.compile(rules);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const dis = Disassembler.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData());
    try dis.dump(stream.writer());
    try testing.expectEqualStrings(
        \\   0: string  "abc"
        \\   1: match
        \\
    , stream.getWritten());
}

test "disassemble alternation" {
    var scanner = EreScanner.init("a|b");
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, "a|b");
    const rules = try parser.parse();
    var compiler = Compiler.compile(rules);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const dis = Disassembler.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData());
    try dis.dump(stream.writer());
    try testing.expectEqualStrings(
        \\   0: choice  -> 3
        \\   1: char    'a'
        \\   2: commit  -> 4
        \\   3: char    'b'
        \\   4: match
        \\
    , stream.getWritten());
}

test "disassemble star" {
    var scanner = EreScanner.init("a*");
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, "a*");
    const rules = try parser.parse();
    var compiler = Compiler.compile(rules);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const dis = Disassembler.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData());
    try dis.dump(stream.writer());
    try testing.expectEqualStrings(
        \\   0: choice  -> 3
        \\   1: char    'a'
        \\   2: commit  -> 0
        \\   3: match
        \\
    , stream.getWritten());
}

test "disassemble charset" {
    var scanner = EreScanner.init("[a-z]+");
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, "[a-z]+");
    const rules = try parser.parse();
    var compiler = Compiler.compile(rules);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const dis = Disassembler.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData());
    try dis.dump(stream.writer());
    try testing.expectEqualStrings(
        \\   0: set     [a-z]
        \\   1: choice  -> 4
        \\   2: set     [a-z]
        \\   3: commit  -> 1
        \\   4: match
        \\
    , stream.getWritten());
}

test "disassemble capture" {
    var scanner = EreScanner.init("a(b)c");
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, "a(b)c");
    const rules = try parser.parse();
    var compiler = Compiler.compile(rules);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const dis = Disassembler.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData());
    try dis.dump(stream.writer());
    try testing.expectEqualStrings(
        \\   0: char    'a'
        \\   1: save    0
        \\   2: char    'b'
        \\   3: save    1
        \\   4: char    'c'
        \\   5: match
        \\
    , stream.getWritten());
}
