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
    factorCommonPrefix(c);
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

/// Factor common byte prefixes out of two-branch alternations.
///
/// Before: choice -> L; string/char; commit -> Lend; L: string/char
/// After:  <prefix>; choice; <suffix_0>; commit; <suffix_1>
///
/// Avoids redundant re-matching of the shared prefix on backtrack.
fn factorCommonPrefix(c: *Compiler) void {
    while (factorOnePrefixPass(c)) {}
}

fn factorOnePrefixPass(c: *Compiler) bool {
    var i: u32 = 0;
    while (i + 3 < c.code_len) : (i += 1) {
        if (c.code[i].op != .choice) continue;

        const b1_start = c.code[i].data.offset;
        if (b1_start != i + 3) continue;
        if (c.code[i + 2].op != .commit) continue;

        const lend = c.code[i + 2].data.offset;
        if (lend != b1_start + 1) continue;

        const b0 = c.code[i + 1];
        const b1 = c.code[i + 3];
        if (!isLiteralOp(b0.op) or !isLiteralOp(b1.op)) continue;

        var buf0: [256]u8 = undefined;
        var buf1: [256]u8 = undefined;
        const sd = c.string_data[0..c.string_data_len];
        const b0_len = extractLitBytes(b0, sd, &buf0);
        const b1_len = extractLitBytes(b1, sd, &buf1);

        const max_p = @min(b0_len, b1_len);
        var prefix_len: u32 = 0;
        while (prefix_len < max_p and buf0[prefix_len] == buf1[prefix_len]) : (prefix_len += 1) {}
        if (prefix_len == 0) continue;

        const s0_len = b0_len - prefix_len;
        const s1_len = b1_len - prefix_len;

        if (s0_len == 0 and s1_len == 0) continue;
        // If branch 0 is a pure prefix of branch 1, branch 1 is dead
        // code in PEG ordered choice. Nothing to gain.
        if (s0_len == 0) continue;

        const prefix_inst = sliceLitInst(b0, sd, 0, prefix_len);

        if (s1_len == 0) {
            // Branch 1 was just the common prefix. Rewrite in place
            // (same instruction count).
            const suffix0 = sliceLitInst(b0, sd, prefix_len, s0_len);
            c.code[i] = prefix_inst;
            c.code[i + 1] = .{ .op = .choice, .data = .{ .offset = lend } };
            c.code[i + 2] = suffix0;
            c.code[i + 3] = .{ .op = .commit, .data = .{ .offset = lend } };
            return true;
        }

        // Both suffixes non-empty: need one extra slot.
        if (c.code_len + 1 > Compiler.max_code) continue;

        const suffix0 = sliceLitInst(b0, sd, prefix_len, s0_len);
        const suffix1 = sliceLitInst(b1, sd, prefix_len, s1_len);

        const old_end = i + 4;

        // Shift everything after the original alternation right by 1.
        var j: u32 = c.code_len;
        while (j > old_end) : (j -= 1) {
            c.code[j] = c.code[j - 1];
        }
        c.code_len += 1;

        // Remap all absolute offsets that pointed past the alternation.
        for (c.code[0..c.code_len]) |*inst| {
            switch (inst.op) {
                .jump, .call, .choice, .commit => {
                    if (inst.data.offset >= old_end) {
                        inst.data.offset += 1;
                    }
                },
                else => {},
            }
        }

        // Overwrite the alternation with the factored form.
        c.code[i] = prefix_inst;
        c.code[i + 1] = .{ .op = .choice, .data = .{ .offset = i + 4 } };
        c.code[i + 2] = suffix0;
        c.code[i + 3] = .{ .op = .commit, .data = .{ .offset = lend + 1 } };
        c.code[i + 4] = suffix1;
        return true;
    }
    return false;
}

fn isLiteralOp(op: I.Opcode) bool {
    return op == .char or op == .string;
}

fn extractLitBytes(inst: I.Inst, sd: []const u8, buf: *[256]u8) u32 {
    switch (inst.op) {
        .char => {
            buf[0] = inst.data.byte;
            return 1;
        },
        .string => {
            const s = inst.data.string;
            const len: u32 = s.len;
            const off: u32 = s.offset;
            @memcpy(buf[0..len], sd[off..][0..len]);
            return len;
        },
        else => return 0,
    }
}

/// Build a literal instruction covering bytes [start..start+len) of an
/// existing char/string instruction, reusing its string_data slot.
fn sliceLitInst(inst: I.Inst, sd: []const u8, start: u32, len: u32) I.Inst {
    if (len == 1) {
        const byte: u8 = switch (inst.op) {
            .char => inst.data.byte,
            .string => sd[inst.data.string.offset + @as(u16, @intCast(start))],
            else => unreachable,
        };
        return .{ .op = .char, .data = .{ .byte = byte } };
    }
    // len > 1 implies the source was a string instruction.
    return .{
        .op = .string,
        .data = .{ .string = .{
            .offset = inst.data.string.offset + @as(u16, @intCast(start)),
            .len = @intCast(len),
        } },
    };
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

test "common prefix factored (one suffix empty)" {
    // "https|http" -> string "http", optional_char 's', match
    const compiler = compileEre("https|http");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.string, code[0].op);
    const str = compiler.getStringData();
    try testing.expectEqualStrings("http", str[code[0].data.string.offset..][0..code[0].data.string.len]);
    try testing.expectEqual(I.Opcode.optional_char, code[1].op);
    try testing.expectEqual(@as(u8, 's'), code[1].data.byte);
    try testing.expectEqual(I.Opcode.match, code[2].op);
    try testing.expectEqual(@as(u32, 3), compiler.code_len);
}

test "common prefix factored (both suffixes non-empty)" {
    // "httpAB|httpCD" -> string "http", choice, string "AB", commit, string "CD", match
    const compiler = compileEre("httpAB|httpCD");
    const code = compiler.getCode();
    const str = compiler.getStringData();
    try testing.expectEqual(I.Opcode.string, code[0].op);
    try testing.expectEqualStrings("http", str[code[0].data.string.offset..][0..code[0].data.string.len]);
    try testing.expectEqual(I.Opcode.choice, code[1].op);
    try testing.expectEqual(I.Opcode.string, code[2].op);
    try testing.expectEqualStrings("AB", str[code[2].data.string.offset..][0..code[2].data.string.len]);
    try testing.expectEqual(I.Opcode.commit, code[3].op);
    try testing.expectEqual(I.Opcode.string, code[4].op);
    try testing.expectEqualStrings("CD", str[code[4].data.string.offset..][0..code[4].data.string.len]);
    try testing.expectEqual(I.Opcode.match, code[5].op);
    try testing.expectEqual(@as(u32, 6), compiler.code_len);
}

test "common prefix not factored without shared prefix" {
    // "abc|xyz" has no common prefix; alternation stays intact.
    const compiler = compileEre("abc|xyz");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.choice, code[0].op);
}

test "common prefix not factored when branch 0 is prefix" {
    // "http|https": branch 0 always wins in PEG, no benefit.
    const compiler = compileEre("http|https");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.choice, code[0].op);
}

test "common prefix single char branches" {
    // "ab|a" -> char 'a', optional_char 'b', match
    const compiler = compileEre("ab|a");
    const code = compiler.getCode();
    try testing.expectEqual(I.Opcode.char, code[0].op);
    try testing.expectEqual(@as(u8, 'a'), code[0].data.byte);
    try testing.expectEqual(I.Opcode.optional_char, code[1].op);
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
