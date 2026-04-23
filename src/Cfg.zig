/// Context-Free Grammar representation.
///
/// A CFG is the 4-tuple (V, Σ, R, S) where V is the set of nonterminals,
/// Σ is the set of terminals, R is the set of productions, and S is the
/// start symbol.
///
/// Terminals use a richer representation than pure bytes: string literals
/// and case-sensitivity are preserved as terminal variants rather than
/// being desugared into byte-level productions. This keeps the grammar
/// compact and readable while preserving the original intent. A lowering
/// pass can derive a pure byte-level CFG if needed.
const std = @import("std");

const Cfg = @This();

/// A terminal symbol in the grammar.
pub const Terminal = union(enum) {
    /// Single byte, e.g. `%x41`.
    byte: u8,
    /// Inclusive byte range, e.g. `%x41-5A`.
    range: struct { lo: u8, hi: u8 },
    /// Case-sensitive string literal, e.g. `%s"GET"`.
    string: []const u8,
    /// Case-insensitive string literal, e.g. `"hello"` / `%i"hello"`.
    string_ci: []const u8,

    pub fn eql(a: Terminal, b: Terminal) bool {
        const Tag = std.meta.Tag(Terminal);
        if (@as(Tag, a) != @as(Tag, b)) return false;
        return switch (a) {
            .byte => a.byte == b.byte,
            .range => a.range.lo == b.range.lo and a.range.hi == b.range.hi,
            .string => std.mem.eql(u8, a.string, b.string),
            .string_ci => std.mem.eql(u8, a.string_ci, b.string_ci),
        };
    }
};

/// A grammar symbol — either a terminal or a reference to a nonterminal.
pub const Symbol = union(enum) {
    terminal: Terminal,
    /// Index into the `nonterminals` table.
    nonterminal: u32,

    pub fn eql(a: Symbol, b: Symbol) bool {
        const Tag = std.meta.Tag(Symbol);
        if (@as(Tag, a) != @as(Tag, b)) return false;
        return switch (a) {
            .terminal => a.terminal.eql(b.terminal),
            .nonterminal => a.nonterminal == b.nonterminal,
        };
    }
};

/// A production rule: `lhs → rhs`.
///
/// An empty `rhs` represents an ε-production.
pub const Production = struct {
    /// Nonterminal index (left-hand side).
    lhs: u32,
    /// Sequence of symbols (right-hand side).
    rhs: []const Symbol,
};

/// Nonterminal names, indexed by nonterminal id.
nonterminals: []const []const u8,
/// All productions in the grammar.
productions: []const Production,
/// Index of the start nonterminal.
start: u32,

/// Return the name of a nonterminal by its index.
pub fn nonterminalName(self: Cfg, id: u32) []const u8 {
    return self.nonterminals[id];
}

/// Return all productions whose left-hand side is `nonterminal`.
pub fn productionsFor(self: Cfg, nonterminal: u32) []const Production {
    // Productions are grouped by LHS. Find the contiguous range.
    var lo: usize = 0;
    while (lo < self.productions.len and self.productions[lo].lhs != nonterminal) : (lo += 1) {}
    if (lo == self.productions.len) return &.{};

    var hi: usize = lo;
    while (hi < self.productions.len and self.productions[hi].lhs == nonterminal) : (hi += 1) {}
    return self.productions[lo..hi];
}

/// Format a single production as `A → X Y Z`.
fn formatProduction(self: Cfg, prod: Production, writer: anytype) !void {
    try writer.writeAll(self.nonterminalName(prod.lhs));
    try writer.writeAll(" →");

    if (prod.rhs.len == 0) {
        try writer.writeAll(" ε");
        return;
    }

    for (prod.rhs) |sym| {
        try writer.writeByte(' ');
        switch (sym) {
            .terminal => |t| switch (t) {
                .byte => |b| try writer.print("%x{X:0>2}", .{b}),
                .range => |r| try writer.print("%x{X:0>2}-{X:0>2}", .{ r.lo, r.hi }),
                .string => |s| try writer.print("\"{s}\"", .{s}),
                .string_ci => |s| try writer.print("%i\"{s}\"", .{s}),
            },
            .nonterminal => |id| try writer.writeAll(self.nonterminalName(id)),
        }
    }
}

/// Format the entire grammar for debug display.
pub fn format(self: Cfg, writer: anytype) !void {
    for (self.productions, 0..) |prod, i| {
        if (i > 0) try writer.writeByte('\n');
        try self.formatProduction(prod, writer);
    }
}

const Scanner = @import("cfg/Scanner.zig").Scanner;
const parser_mod = @import("cfg/Parser.zig");
const Parser = parser_mod.Parser;

/// Parse a CFG from a textual representation at compile time.
///
/// Syntax:
///   - Each line defines one or more productions: `A -> X Y | Z`
///   - `->` separates the LHS nonterminal from the RHS symbols
///   - `|` separates alternative right-hand sides
///   - Quoted strings are case-sensitive terminals: `"text"`
///   - `%s"text"` — case-sensitive string terminal
///   - `%i"text"` — case-insensitive string terminal
///   - `%x41` — single byte terminal, `%x41-5A` — byte range
///   - Bare identifiers are nonterminal references
///   - Empty RHS is an ε-production
///   - The first rule's LHS becomes the start symbol
///   - Lines starting with `//` are comments; blank lines are ignored
///
/// Example:
///
///     const cfg = comptime Cfg.parse(
///         \\S -> A "x"
///         \\A -> "hello" |
///     );
pub fn parse(comptime source: []const u8) Cfg {
    comptime {
        @setEvalBranchQuota(100_000);
        var scanner = Scanner.init(source);
        const tokens = scanner.scanTokens();
        var parser = Parser.init(tokens, source);
        return parser.parse() catch @compileError("CFG grammar has syntax errors");
    }
}

const CnfBuilder = @import("cfg/CnfBuilder.zig");

/// Convert this grammar to Chomsky Normal Form at compile time.
///
/// CNF restricts every production to one of:
///   - `A → B C`   (exactly two nonterminals)
///   - `A → a`     (exactly one terminal)
///   - `S0 → ε`    (only the start symbol, if ε ∈ L)
///
/// The conversion applies the standard textbook steps in order:
///   1. **START** — guarantee the start symbol never appears on any RHS
///   2. **DEL**   — eliminate ε-productions
///   3. **UNIT**  — eliminate unit productions
///   4. **TERM**  — isolate terminals in long RHS
///   5. **BIN**   — break long RHS into binary chains
pub fn toCnf(comptime self: Cfg) Cfg {
    comptime {
        @setEvalBranchQuota(1_000_000);
        var b = CnfBuilder.init(self);
        return b.build();
    }
}

test "basic construction and name lookup" {
    const cfg = Cfg{
        .nonterminals = &.{ "S", "A" },
        .productions = &.{
            // S → A "x"
            .{ .lhs = 0, .rhs = &.{
                .{ .nonterminal = 1 },
                .{ .terminal = .{ .byte = 'x' } },
            } },
            // A → "hello"
            .{ .lhs = 1, .rhs = &.{
                .{ .terminal = .{ .string = "hello" } },
            } },
            // A → ε
            .{ .lhs = 1, .rhs = &.{} },
        },
        .start = 0,
    };

    try std.testing.expectEqualStrings("S", cfg.nonterminalName(0));
    try std.testing.expectEqualStrings("A", cfg.nonterminalName(1));
}

test "productionsFor returns matching slice" {
    const cfg = Cfg{
        .nonterminals = &.{ "S", "A" },
        .productions = &.{
            .{ .lhs = 0, .rhs = &.{.{ .nonterminal = 1 }} },
            .{ .lhs = 1, .rhs = &.{.{ .terminal = .{ .byte = 'a' } }} },
            .{ .lhs = 1, .rhs = &.{} },
        },
        .start = 0,
    };

    const s_prods = cfg.productionsFor(0);
    try std.testing.expectEqual(1, s_prods.len);
    try std.testing.expectEqual(@as(u32, 0), s_prods[0].lhs);

    const a_prods = cfg.productionsFor(1);
    try std.testing.expectEqual(2, a_prods.len);

    const none = cfg.productionsFor(99);
    try std.testing.expectEqual(0, none.len);
}

test "symbol equality" {
    const a: Symbol = .{ .terminal = .{ .byte = 'a' } };
    const b: Symbol = .{ .terminal = .{ .byte = 'b' } };
    const a2: Symbol = .{ .terminal = .{ .byte = 'a' } };
    const nt: Symbol = .{ .nonterminal = 0 };

    try std.testing.expect(a.eql(a2));
    try std.testing.expect(!a.eql(b));
    try std.testing.expect(!a.eql(nt));
}

test "terminal equality across variants" {
    const byte_a: Terminal = .{ .byte = 'A' };
    const range_a: Terminal = .{ .range = .{ .lo = 'A', .hi = 'Z' } };
    const str: Terminal = .{ .string = "GET" };
    const str_ci: Terminal = .{ .string_ci = "get" };

    try std.testing.expect(byte_a.eql(.{ .byte = 'A' }));
    try std.testing.expect(!byte_a.eql(range_a));
    try std.testing.expect(!str.eql(str_ci));
    try std.testing.expect(range_a.eql(.{ .range = .{ .lo = 'A', .hi = 'Z' } }));
    try std.testing.expect(str.eql(.{ .string = "GET" }));
    try std.testing.expect(str_ci.eql(.{ .string_ci = "get" }));
}

test "format produces readable output" {
    const cfg = Cfg{
        .nonterminals = &.{ "S", "A" },
        .productions = &.{
            .{ .lhs = 0, .rhs = &.{
                .{ .nonterminal = 1 },
                .{ .terminal = .{ .byte = 'x' } },
            } },
            .{ .lhs = 1, .rhs = &.{
                .{ .terminal = .{ .string = "hello" } },
            } },
            .{ .lhs = 1, .rhs = &.{} },
        },
        .start = 0,
    };

    var buf: [256]u8 = undefined;
    var fbs: std.Io.Writer = .fixed(&buf);
    try cfg.formatProduction(cfg.productions[0], &fbs);
    try std.testing.expectEqualStrings("S → A %x78", fbs.buffered());

    fbs.end = 0;
    try cfg.formatProduction(cfg.productions[2], &fbs);
    try std.testing.expectEqualStrings("A → ε", fbs.buffered());
}

test "parse: basic nonterminal and string terminal" {
    const cfg = comptime Cfg.parse(
        \\S -> A "x"
        \\A -> "hello"
    );

    try std.testing.expectEqual(2, cfg.nonterminals.len);
    comptime try std.testing.expectEqualStrings("S", cfg.nonterminalName(0));
    comptime try std.testing.expectEqualStrings("A", cfg.nonterminalName(1));
    try std.testing.expectEqual(@as(u32, 0), cfg.start);

    try std.testing.expectEqual(2, cfg.productions.len);

    // S -> A "x"
    const p0 = cfg.productions[0];
    try std.testing.expectEqual(@as(u32, 0), p0.lhs);
    try std.testing.expectEqual(2, p0.rhs.len);
    try std.testing.expect(p0.rhs[0].eql(.{ .nonterminal = 1 }));
    try std.testing.expect(p0.rhs[1].eql(.{ .terminal = .{ .string = "x" } }));

    // A -> "hello"
    const p1 = cfg.productions[1];
    try std.testing.expectEqual(@as(u32, 1), p1.lhs);
    try std.testing.expect(p1.rhs[0].eql(.{ .terminal = .{ .string = "hello" } }));
}

test "parse: alternation with pipe" {
    const cfg = comptime Cfg.parse(
        \\S -> "a" | "b" | "c"
    );

    try std.testing.expectEqual(3, cfg.productions.len);
    try std.testing.expect(cfg.productions[0].rhs[0].eql(.{ .terminal = .{ .string = "a" } }));
    try std.testing.expect(cfg.productions[1].rhs[0].eql(.{ .terminal = .{ .string = "b" } }));
    try std.testing.expect(cfg.productions[2].rhs[0].eql(.{ .terminal = .{ .string = "c" } }));
}

test "parse: epsilon production" {
    const cfg = comptime Cfg.parse(
        \\S -> "x" |
    );

    try std.testing.expectEqual(2, cfg.productions.len);
    try std.testing.expectEqual(1, cfg.productions[0].rhs.len);
    try std.testing.expectEqual(0, cfg.productions[1].rhs.len); // ε
}

test "parse: hex byte and range" {
    const cfg = comptime Cfg.parse(
        \\S -> %x41 %x61-7A
    );

    try std.testing.expectEqual(1, cfg.productions.len);
    try std.testing.expect(cfg.productions[0].rhs[0].eql(.{ .terminal = .{ .byte = 0x41 } }));
    try std.testing.expect(cfg.productions[0].rhs[1].eql(.{ .terminal = .{ .range = .{ .lo = 0x61, .hi = 0x7A } } }));
}

test "parse: case-sensitive and case-insensitive strings" {
    const cfg = comptime Cfg.parse(
        \\S -> %s"GET" %i"hello"
    );

    try std.testing.expectEqual(1, cfg.productions.len);
    try std.testing.expect(cfg.productions[0].rhs[0].eql(.{ .terminal = .{ .string = "GET" } }));
    try std.testing.expect(cfg.productions[0].rhs[1].eql(.{ .terminal = .{ .string_ci = "hello" } }));
}

test "parse: comments and blank lines" {
    const cfg = comptime Cfg.parse(
        \\// This is a comment
        \\
        \\S -> "x"
        \\// Another comment
    );

    try std.testing.expectEqual(1, cfg.productions.len);
}

test "parse: productions grouped by LHS" {
    const cfg = comptime Cfg.parse(
        \\S -> A
        \\A -> "a"
        \\S -> "b"
    );

    // S productions should be grouped together despite interleaving.
    // `productionsFor` takes Cfg by value, which would require comptime
    // slice pointers to be materialized at runtime; run at comptime.
    comptime {
        const s_prods = cfg.productionsFor(0);
        try std.testing.expectEqual(2, s_prods.len);
        try std.testing.expectEqual(@as(u32, 0), s_prods[0].lhs);
        try std.testing.expectEqual(@as(u32, 0), s_prods[1].lhs);

        const a_prods = cfg.productionsFor(1);
        try std.testing.expectEqual(1, a_prods.len);
    }
}

test "parse: format round-trip" {
    const cfg = comptime Cfg.parse(
        \\S -> A %x78
        \\A -> %s"hello"
        \\A ->
    );

    const actual = comptime std.fmt.comptimePrint("{f}", .{cfg});
    try std.testing.expectEqualStrings(
        \\S → A %x78
        \\A → "hello"
        \\A → ε
    , actual);
}
