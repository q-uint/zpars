/// Bytecode instructions for the grammar parsing VM.
///
/// Based on the LPeg parsing machine design (Ierusalimschy, 2009).
/// Supports ordered choice with backtracking, rule calls, and
/// character set matching via 256-bit bitmaps.
pub const Opcode = enum(u8) {
    /// Match a single literal byte or fail.
    char,
    /// Match a sequence of literal bytes or fail.
    string,
    /// Match any single byte or fail.
    any,
    /// Match a byte within a 256-bit charset or fail.
    set,
    /// Match a byte NOT within a 256-bit charset or fail.
    neg_set,
    /// Push backtrack entry (current pos, L) onto the stack.
    /// On failure, restore pos and jump to L.
    choice,
    /// Pop backtrack entry, jump to L. Used after a choice succeeds.
    commit,
    /// Trigger backtracking: pop entry, restore pos, jump to saved L.
    fail,
    /// Pop backtrack entry then fail again. Implements `!e`.
    fail_twice,
    /// Unconditional jump to L.
    jump,
    /// Push return address, jump to L. Implements rule calls.
    call,
    /// Pop return address and jump to it.
    ret,
    /// Record the current input position in capture slot N.
    /// Even slots mark the start of a capture, odd slots mark the end.
    save,
    /// Match a single literal byte if present, without backtracking.
    /// Equivalent to choice/char/commit but without stack operations.
    optional_char,
    /// Memoized rule call. Looks up (rule_id, pos) in the memo table.
    /// On hit-success, skips the call and advances pos. On hit-fail,
    /// triggers backtracking. On miss, pushes a memo frame and jumps
    /// to the rule entry exactly like a regular `call`. The final
    /// `ret` writes the result back to the table.
    memo_call,
    /// Accept the match and halt.
    match,
};

/// A 256-bit bitmap for character sets.
/// Bit i is set if byte i is in the set.
pub const Charset = [4]u64;

pub fn charsetContains(cs: Charset, byte: u8) bool {
    const word = byte >> 6;
    const bit: u6 = @truncate(byte);
    return cs[word] & (@as(u64, 1) << bit) != 0;
}

pub fn charsetFromRanges(ranges: []const [2]u8) Charset {
    var cs = Charset{ 0, 0, 0, 0 };
    for (ranges) |r| {
        var b: u16 = r[0];
        while (b <= r[1]) : (b += 1) {
            const word = @as(u8, @intCast(b)) >> 6;
            const bit: u6 = @truncate(@as(u8, @intCast(b)));
            cs[word] |= @as(u64, 1) << bit;
        }
    }
    return cs;
}

pub const Inst = struct {
    op: Opcode,
    data: Data = .{ .none = {} },

    pub const Data = union {
        byte: u8,
        offset: u32,
        charset: u16,
        slot: u16,
        string: StringRef,
        memo: MemoCall,
        none: void,
    };

    pub const StringRef = packed struct {
        offset: u16,
        len: u8,
    };

    pub const MemoCall = struct {
        rule_id: u16,
        offset: u32,
    };
};
