/// Peephole optimizer for compiled VM bytecode.
///
/// Runs after compilation, before execution. Each pass is an in-place
/// rewrite over the instruction and charset arrays. New passes are
/// added as functions and called from `optimize`.
const I = @import("Instruction.zig");
const Compiler = @import("Compiler.zig");

pub fn optimize(c: *Compiler) void {
    singleCharSetToChar(c.code[0..c.code_len], c.charsets[0..c.charset_len]);
    fuseConsecutiveChars(c);
    fuseOptionalChar(c);
}

/// Replace `set` instructions whose charset has exactly one bit set
/// with a cheaper `char` instruction.
fn singleCharSetToChar(code: []I.Inst, charsets: []const I.Charset) void {
    for (code) |*inst| {
        if (inst.op != .set) continue;
        const cs = charsets[inst.data.charset];
        if (charsetPopcount(cs) == 1) {
            inst.* = .{ .op = .char, .data = .{ .byte = charsetSingleByte(cs) } };
        }
    }
}

/// Fuse runs of 2+ consecutive `char` instructions into single `string`
/// instructions. Stores the byte sequences in the compiler's string_data
/// buffer, then compacts the code array and remaps all offsets.
fn fuseConsecutiveChars(c: *Compiler) void {
    const code = c.code[0..c.code_len];

    // Mark which instructions to keep (false = removed by fusion).
    var keep = [_]bool{true} ** Compiler.max_code;
    var any_fused = false;

    var i: u32 = 0;
    while (i < c.code_len) {
        if (code[i].op != .char) {
            i += 1;
            continue;
        }

        // Find run length.
        var run_end = i + 1;
        while (run_end < c.code_len and code[run_end].op == .char) : (run_end += 1) {}
        const run_len = run_end - i;

        if (run_len >= 2) {
            // Copy bytes into string_data buffer.
            const str_offset = c.string_data_len;
            const len: u8 = @intCast(@min(run_len, 255));
            for (0..len) |j| {
                c.string_data[c.string_data_len] = code[i + @as(u32, @intCast(j))].data.byte;
                c.string_data_len += 1;
            }

            // Replace first char with string instruction.
            code[i] = .{
                .op = .string,
                .data = .{ .string = .{ .offset = str_offset, .len = len } },
            };

            // Mark remaining chars in the run for removal.
            for (i + 1..i + len) |k| {
                keep[k] = false;
            }
            any_fused = true;
            i += len;
        } else {
            i += 1;
        }
    }

    if (!any_fused) return;

    // Build remap table: old address -> new address.
    var remap = [_]u32{0} ** Compiler.max_code;
    var new_len: u32 = 0;
    for (0..c.code_len) |old| {
        remap[old] = new_len;
        if (keep[old]) new_len += 1;
    }
    // Addresses equal to code_len (targets just past the last instruction)
    // must also be remapped.
    remap[c.code_len] = new_len;

    // Compact: shift kept instructions down.
    var dst: u32 = 0;
    for (0..c.code_len) |old| {
        if (keep[old]) {
            c.code[dst] = code[old];
            dst += 1;
        }
    }
    c.code_len = new_len;

    // Repatch all offset-bearing instructions.
    for (c.code[0..c.code_len]) |*inst| {
        switch (inst.op) {
            .jump, .call, .choice, .commit => {
                inst.data = .{ .offset = remap[inst.data.offset] };
            },
            else => {},
        }
    }
}

/// Fuse `choice[+3], char, commit[+1]` into a single `optional_char`.
/// This pattern is emitted for `e?` when `e` is a single char.
fn fuseOptionalChar(c: *Compiler) void {
    const code = c.code[0..c.code_len];

    var keep = [_]bool{true} ** Compiler.max_code;
    var any_fused = false;

    var i: u32 = 0;
    while (i + 2 < c.code_len) {
        if (code[i].op == .choice and
            code[i + 1].op == .char and
            code[i + 2].op == .commit and
            code[i].data.offset == i + 3 and
            code[i + 2].data.offset == i + 3)
        {
            // Replace choice with optional_char, mark char and commit for removal.
            code[i] = .{ .op = .optional_char, .data = .{ .byte = code[i + 1].data.byte } };
            keep[i + 1] = false;
            keep[i + 2] = false;
            any_fused = true;
            i += 3;
        } else {
            i += 1;
        }
    }

    if (!any_fused) return;

    // Build remap table and compact, same as fuseConsecutiveChars.
    var remap = [_]u32{0} ** Compiler.max_code;
    var new_len: u32 = 0;
    for (0..c.code_len) |old| {
        remap[old] = new_len;
        if (keep[old]) new_len += 1;
    }
    remap[c.code_len] = new_len;

    var dst: u32 = 0;
    for (0..c.code_len) |old| {
        if (keep[old]) {
            c.code[dst] = code[old];
            dst += 1;
        }
    }
    c.code_len = new_len;

    for (c.code[0..c.code_len]) |*inst| {
        switch (inst.op) {
            .jump, .call, .choice, .commit => {
                inst.data = .{ .offset = remap[inst.data.offset] };
            },
            else => {},
        }
    }
}

fn charsetPopcount(cs: I.Charset) u32 {
    var count: u32 = 0;
    for (cs) |word| count += @popCount(word);
    return count;
}

fn charsetSingleByte(cs: I.Charset) u8 {
    for (cs, 0..) |word, wi| {
        if (word != 0) return @intCast(wi * 64 + @ctz(word));
    }
    unreachable;
}

const testing = @import("std").testing;
const EreScanner = @import("../ere/Scanner.zig");
const EreParser = @import("../ere/Parser.zig");

fn compileEre(source: []const u8) Compiler {
    var scanner = EreScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, source);
    const rules = parser.parse() catch return Compiler{};
    return Compiler.compile(rules);
}

fn compileEreUnopt(source: []const u8) Compiler {
    var scanner = EreScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, source);
    const rules = parser.parse() catch return Compiler{};
    return Compiler.compileOpts(rules, .{ .optimize = false });
}

test "charset deduplication" {
    // Two identical charsets [a-z] should share one slot.
    const compiler = compileEre("[a-z][a-z]");
    try testing.expectEqual(@as(u16, 1), compiler.charset_len);
}

test "single-char charset replaced with char" {
    // [a] should be optimized to a char instruction.
    const compiler = compileEre("[a]");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.char, code[0].op);
    try testing.expectEqual(@as(u8, 'a'), code[0].data.byte);
}

test "multi-char charset not replaced" {
    // [ab] has two bits set; must remain a set instruction.
    const compiler = compileEre("[ab]");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.set, code[0].op);
}

test "negated single-char charset not replaced" {
    // [^a] should stay as neg_set (no single-instruction equivalent).
    const compiler = compileEre("[^a]");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.neg_set, code[0].op);
}

test "consecutive chars fused into string" {
    // "abc" should produce a single string instruction instead of 3 chars.
    const compiler = compileEre("abc");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.string, code[0].op);
    try testing.expectEqual(@as(u8, 3), code[0].data.string.len);
    const str = compiler.getStringData();
    try testing.expectEqualStrings("abc", str[code[0].data.string.offset..][0..3]);
    // string + match = 2 instructions total
    try testing.expectEqual(@as(u32, 2), compiler.code_len);
}

test "single char not fused" {
    // A single char should remain as char, not become a 1-byte string.
    const compiler = compileEre("a");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.char, code[0].op);
}

test "string fusion preserves offsets" {
    // "ab|cd" -- alternation offsets must be correct after fusion.
    const compiler = compileEre("ab|cd");
    const code = compiler.getCode();
    // choice -> string "ab" -> commit -> string "cd" -> match
    try testing.expectEqual(I.Opcode.choice, code[0].op);
    try testing.expectEqual(I.Opcode.string, code[1].op);
    try testing.expectEqual(I.Opcode.commit, code[2].op);
    try testing.expectEqual(I.Opcode.string, code[3].op);
    try testing.expectEqual(I.Opcode.match, code[4].op);
    // choice should jump to the second alternative (string "cd")
    try testing.expectEqual(@as(u32, 3), code[0].data.offset);
    // commit should jump past the end
    try testing.expectEqual(@as(u32, 4), code[2].data.offset);
}

test "optional char fused" {
    // "a?" should produce optional_char instead of choice/char/commit.
    const compiler = compileEre("a?");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.optional_char, code[0].op);
    try testing.expectEqual(@as(u8, 'a'), code[0].data.byte);
    try testing.expectEqual(I.Opcode.match, code[1].op);
    try testing.expectEqual(@as(u32, 2), compiler.code_len);
}

test "optional char not fused for star" {
    // "a*" uses choice/char/commit but commit jumps back, not forward.
    const compiler = compileEre("a*");
    const code = compiler.getCode();
    // Should remain choice/char/commit, not fused.
    try testing.expectEqual(I.Opcode.choice, code[0].op);
}

test "optional char preserves surrounding offsets" {
    // "a?b" should become: optional_char 'a', char 'b', match
    const compiler = compileEre("a?b");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.optional_char, code[0].op);
    try testing.expectEqual(@as(u8, 'a'), code[0].data.byte);
    try testing.expectEqual(I.Opcode.char, code[1].op);
    try testing.expectEqual(@as(u8, 'b'), code[1].data.byte);
    try testing.expectEqual(I.Opcode.match, code[2].op);
}

test "optimizer disabled" {
    // With optimization off, chars remain unfused and charsets are not simplified.
    const compiler = compileEreUnopt("abc");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.char, code[0].op);
    try testing.expectEqual(I.Opcode.char, code[1].op);
    try testing.expectEqual(I.Opcode.char, code[2].op);
}
