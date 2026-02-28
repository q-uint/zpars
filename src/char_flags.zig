/// Shared character classification table for all grammar scanners.
///
/// A single comptime-built 256-entry lookup table replaces the per-scanner
/// predicate functions (`isAlpha`, `isDigit`, `isHexDigit`, `isBit`,
/// `isIdentStart`, `isIdentCont`).
///
/// `ident_cont` covers the common subset `[a-zA-Z0-9_]`. Scanners with
/// extended identifier rules (e.g. CFG allows `-`) add their own check.
pub const CharFlags = packed struct(u8) {
    alpha: bool = false,
    digit: bool = false,
    hex: bool = false,
    bit: bool = false,
    ident_start: bool = false,
    ident_cont: bool = false,
    _pad: u2 = 0,
};

pub const table: [256]CharFlags = blk: {
    var t: [256]CharFlags = @splat(.{});

    for ('A'..('Z' + 1)) |c| {
        t[c] = .{
            .alpha = true,
            .hex = c <= 'F',
            .ident_start = true,
            .ident_cont = true,
        };
    }
    for ('a'..('z' + 1)) |c| {
        t[c] = .{
            .alpha = true,
            .hex = c <= 'f',
            .ident_start = true,
            .ident_cont = true,
        };
    }
    for ('0'..('9' + 1)) |c| {
        t[c] = .{
            .digit = true,
            .hex = true,
            .bit = c <= '1',
            .ident_cont = true,
        };
    }
    t['_'] = .{ .ident_start = true, .ident_cont = true };

    break :blk t;
};

pub fn isAlpha(c: u8) bool {
    return table[c].alpha;
}

pub fn isDigit(c: u8) bool {
    return table[c].digit;
}

pub fn isHexDigit(c: u8) bool {
    return table[c].hex;
}

pub fn isBit(c: u8) bool {
    return table[c].bit;
}

pub fn isIdentStart(c: u8) bool {
    return table[c].ident_start;
}

pub fn isIdentCont(c: u8) bool {
    return table[c].ident_cont;
}
