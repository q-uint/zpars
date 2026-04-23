/// CNF (Chomsky Normal Form) conversion builder.
///
/// Transforms a Cfg into an equivalent grammar where every production is:
///   - `A -> B C`   (exactly two nonterminals)
///   - `A -> a`     (exactly one terminal)
///   - `S0 -> ε`    (only the start symbol, if ε ∈ L)
///
/// Uses the same comptime bounded-array pattern as `cfg/Parser.zig`.
const std = @import("std");
const Cfg = @import("../Cfg.zig");

const Symbol = Cfg.Symbol;
const Production = Cfg.Production;

const CnfBuilder = @This();

/// Bounded limits (larger than parser limits since CNF conversion
/// can expand the grammar).
pub const max_nts = 512;
pub const max_prods = 2048;
pub const max_symbols = 8192;

nts: [max_nts][]const u8 = undefined,
nt_count: usize = 0,

sym_pool: [max_symbols]Symbol = undefined,
sym_total: usize = 0,

prods: [max_prods]Production = undefined,
prod_count: usize = 0,

start: u32,

/// Seed the builder from an existing Cfg.
pub fn init(cfg: Cfg) CnfBuilder {
    var b: CnfBuilder = .{ .start = cfg.start };
    for (cfg.nonterminals) |name| {
        b.nts[b.nt_count] = name;
        b.nt_count += 1;
    }
    for (cfg.productions) |prod| {
        b.addProd(prod.lhs, prod.rhs);
    }
    return b;
}

/// Run all CNF transformation steps and return the result.
pub fn build(b: *CnfBuilder) Cfg {
    b.startStep();
    b.delStep();
    b.unitStep();
    b.termStep();
    b.binStep();
    return b.result();
}

/// Append a production, copying its RHS symbols into the pool.
fn addProd(b: *CnfBuilder, lhs: u32, rhs: []const Symbol) void {
    if (b.sym_total + rhs.len > max_symbols)
        @panic("CnfBuilder symbol pool exhausted");
    if (b.prod_count >= max_prods)
        @panic("CnfBuilder production pool exhausted");
    const sym_start = b.sym_total;
    for (rhs) |sym| {
        b.sym_pool[b.sym_total] = sym;
        b.sym_total += 1;
    }
    b.prods[b.prod_count] = .{
        .lhs = lhs,
        .rhs = b.sym_pool[sym_start..b.sym_total],
    };
    b.prod_count += 1;
}

/// Register a fresh nonterminal and return its id.
fn addNt(b: *CnfBuilder, name: []const u8) u32 {
    if (b.nt_count >= max_nts)
        @panic("CnfBuilder nonterminal pool exhausted");
    const id: u32 = @intCast(b.nt_count);
    b.nts[b.nt_count] = name;
    b.nt_count += 1;
    return id;
}

/// Check whether a production `lhs -> rhs` already exists.
fn hasProd(b: *CnfBuilder, lhs: u32, rhs: []const Symbol) bool {
    for (b.prods[0..b.prod_count]) |prod| {
        if (prod.lhs != lhs) continue;
        if (prod.rhs.len != rhs.len) continue;
        var match = true;
        for (prod.rhs, rhs) |a, c| {
            if (!a.eql(c)) {
                match = false;
                break;
            }
        }
        if (match) return true;
    }
    return false;
}

/// If the start symbol appears on any RHS, introduce a fresh S0 -> S.
fn startStep(b: *CnfBuilder) void {
    for (b.prods[0..b.prod_count]) |prod| {
        for (prod.rhs) |sym| {
            switch (sym) {
                .nonterminal => |id| {
                    if (id == b.start) {
                        const old_start = b.start;
                        const s0 = b.addNt("S0");
                        const ss = b.sym_total;
                        b.sym_pool[b.sym_total] = .{ .nonterminal = old_start };
                        b.sym_total += 1;
                        b.prods[b.prod_count] = .{
                            .lhs = s0,
                            .rhs = b.sym_pool[ss..b.sym_total],
                        };
                        b.prod_count += 1;
                        b.start = s0;
                        return;
                    }
                },
                .terminal => {},
            }
        }
    }
}

/// Remove ε-productions and propagate nullable combinations.
///
/// 1. Compute the set of nullable nonterminals (fixed point).
/// 2. For every production whose RHS contains nullable symbols, generate
///    all combinations with those symbols present/absent.
/// 3. Drop all ε-productions.
/// 4. If the start symbol is nullable, add back `S -> ε`.
fn delStep(b: *CnfBuilder) void {
    // 1. Compute nullable set.
    var nullable = [1]bool{false} ** max_nts;

    // Seed: direct ε-productions.
    for (b.prods[0..b.prod_count]) |prod| {
        if (prod.rhs.len == 0) nullable[prod.lhs] = true;
    }

    // Fixed point.
    var changed = true;
    while (changed) {
        changed = false;
        for (b.prods[0..b.prod_count]) |prod| {
            if (nullable[prod.lhs] or prod.rhs.len == 0) continue;
            var all = true;
            for (prod.rhs) |sym| {
                switch (sym) {
                    .nonterminal => |id| {
                        if (!nullable[id]) {
                            all = false;
                            break;
                        }
                    },
                    .terminal => {
                        all = false;
                        break;
                    },
                }
            }
            if (all) {
                nullable[prod.lhs] = true;
                changed = true;
            }
        }
    }

    // Early exit when nothing is nullable.
    var any_nullable = false;
    for (nullable[0..b.nt_count]) |n| {
        if (n) {
            any_nullable = true;
            break;
        }
    }
    if (!any_nullable) return;

    // 2. Snapshot current productions.
    const old_count = b.prod_count;
    var old_prods: [max_prods]Production = undefined;
    for (b.prods[0..old_count], 0..) |prod, i| old_prods[i] = prod;
    b.prod_count = 0; // sym_pool keeps growing - old slices stay valid.

    // 3. Rebuild with nullable combinations.
    for (old_prods[0..old_count]) |prod| {
        if (prod.rhs.len == 0) continue; // Drop ε-productions.

        // Bitmask of nullable positions.
        var nmask: usize = 0;
        for (prod.rhs, 0..) |sym, i| {
            switch (sym) {
                .nonterminal => |id| {
                    if (nullable[id]) nmask |= @as(usize, 1) << @intCast(i);
                },
                .terminal => {},
            }
        }

        // Enumerate all subsets of nmask (ascending: 0 first = original).
        var remove: usize = 0;
        while (true) {
            // Build RHS with `remove` positions dropped.
            const ss = b.sym_total;
            for (prod.rhs, 0..) |sym, i| {
                if (remove & (@as(usize, 1) << @intCast(i)) == 0) {
                    b.sym_pool[b.sym_total] = sym;
                    b.sym_total += 1;
                }
            }
            const new_rhs = b.sym_pool[ss..b.sym_total];

            if (new_rhs.len > 0 and !b.hasProd(prod.lhs, new_rhs)) {
                b.prods[b.prod_count] = .{ .lhs = prod.lhs, .rhs = new_rhs };
                b.prod_count += 1;
            } else {
                b.sym_total = ss; // Revert unused symbols.
            }

            if (remove == nmask) break;
            // Next subset: advance within the bits of nmask.
            // Increment remove, but only across bits in nmask.
            // Carry into the next nmask-bit by adding the complement.
            remove = (remove | ~nmask) +% 1;
            remove &= nmask;
        }
    }

    // 4. Re-add start -> ε if the start symbol is nullable.
    if (nullable[b.start]) {
        b.prods[b.prod_count] = .{
            .lhs = b.start,
            .rhs = b.sym_pool[b.sym_total..b.sym_total],
        };
        b.prod_count += 1;
    }
}

/// Remove unit productions `A -> B` by copying B's non-unit productions to A.
///
/// A unit production is one whose RHS is a single nonterminal.
/// For each unit pair (A, B) reachable through unit chains, every
/// non-unit production of B is added to A. Then all unit productions
/// are removed.
fn unitStep(b: *CnfBuilder) void {
    // 1. Compute unit closure for every nonterminal.
    //    unit[A][B] == true means A can reach B through unit productions.
    //    Seed with identity (A reaches A), then propagate.
    var unit = [1][max_nts]bool{[1]bool{false} ** max_nts} ** max_nts;
    for (0..b.nt_count) |i| unit[i][i] = true;

    // Seed direct unit productions.
    for (b.prods[0..b.prod_count]) |prod| {
        if (prod.rhs.len == 1) {
            switch (prod.rhs[0]) {
                .nonterminal => |id| unit[prod.lhs][id] = true,
                .terminal => {},
            }
        }
    }

    // Floyd-Warshall-style transitive closure.
    for (0..b.nt_count) |k| {
        for (0..b.nt_count) |i| {
            if (!unit[i][k]) continue;
            for (0..b.nt_count) |j| {
                if (unit[k][j]) unit[i][j] = true;
            }
        }
    }

    // Check if there are any non-trivial unit pairs (early exit).
    var any_unit = false;
    for (b.prods[0..b.prod_count]) |prod| {
        if (prod.rhs.len == 1) {
            switch (prod.rhs[0]) {
                .nonterminal => {
                    any_unit = true;
                    break;
                },
                .terminal => {},
            }
        }
    }
    if (!any_unit) return;

    // 2. Snapshot current productions.
    const old_count = b.prod_count;
    var old_prods: [max_prods]Production = undefined;
    for (b.prods[0..old_count], 0..) |prod, i| old_prods[i] = prod;
    b.prod_count = 0;

    // 3. For each nonterminal A, for each B in unit[A], copy B's
    //    non-unit productions as A's.
    for (0..b.nt_count) |a| {
        for (0..b.nt_count) |bb| {
            if (!unit[a][bb]) continue;
            for (old_prods[0..old_count]) |prod| {
                if (prod.lhs != @as(u32, @intCast(bb))) continue;
                // Skip unit productions.
                if (prod.rhs.len == 1) {
                    switch (prod.rhs[0]) {
                        .nonterminal => continue,
                        .terminal => {},
                    }
                }
                const lhs: u32 = @intCast(a);
                if (!b.hasProd(lhs, prod.rhs)) {
                    b.addProd(lhs, prod.rhs);
                }
            }
        }
    }
}

/// In productions with |rhs| >= 2, replace each terminal `a` with a fresh
/// nonterminal `T_a -> a` so that long productions contain only nonterminals.
fn termStep(b: *CnfBuilder) void {
    // Snapshot.
    const old_count = b.prod_count;
    var old_prods: [max_prods]Production = undefined;
    for (b.prods[0..old_count], 0..) |prod, i| old_prods[i] = prod;
    b.prod_count = 0;

    // Map: terminal -> proxy nonterminal id.  Index by order of discovery.
    var proxy_terms: [max_nts]Cfg.Terminal = undefined;
    var proxy_nts: [max_nts]u32 = undefined;
    var proxy_count: usize = 0;

    for (old_prods[0..old_count]) |prod| {
        if (prod.rhs.len < 2) {
            // Already CNF-legal (single terminal or ε) - keep as-is.
            b.addProd(prod.lhs, prod.rhs);
            continue;
        }

        // Rewrite: replace terminals with proxy nonterminals. Stage
        // into a local buffer first because `findOrAddProxy` itself
        // writes the proxy's own production into `sym_pool`, which
        // would otherwise interleave with (and grow) the rhs we're
        // accumulating for this production.
        var new_rhs: [max_symbols]Symbol = undefined;
        var new_len: usize = 0;
        for (prod.rhs) |sym| {
            switch (sym) {
                .terminal => |t| {
                    const pid = findOrAddProxy(
                        &proxy_terms,
                        &proxy_nts,
                        &proxy_count,
                        t,
                        b,
                    );
                    new_rhs[new_len] = .{ .nonterminal = pid };
                },
                .nonterminal => {
                    new_rhs[new_len] = sym;
                },
            }
            new_len += 1;
        }
        const ss = b.sym_total;
        for (new_rhs[0..new_len]) |sym| {
            b.sym_pool[b.sym_total] = sym;
            b.sym_total += 1;
        }
        b.prods[b.prod_count] = .{
            .lhs = prod.lhs,
            .rhs = b.sym_pool[ss..b.sym_total],
        };
        b.prod_count += 1;
    }
}

/// Look up or create a proxy nonterminal for a terminal.
fn findOrAddProxy(
    proxy_terms: *[max_nts]Cfg.Terminal,
    proxy_nts: *[max_nts]u32,
    proxy_count: *usize,
    t: Cfg.Terminal,
    b: *CnfBuilder,
) u32 {
    // Check existing proxies.
    for (proxy_terms.*[0..proxy_count.*], proxy_nts.*[0..proxy_count.*]) |pt, pn| {
        if (pt.eql(t)) return pn;
    }
    // Create fresh nonterminal T_<name> -> t.
    const name = proxyName(t);
    const id = b.addNt(name);
    const ss = b.sym_total;
    b.sym_pool[b.sym_total] = .{ .terminal = t };
    b.sym_total += 1;
    b.prods[b.prod_count] = .{ .lhs = id, .rhs = b.sym_pool[ss..b.sym_total] };
    b.prod_count += 1;

    proxy_terms.*[proxy_count.*] = t;
    proxy_nts.*[proxy_count.*] = id;
    proxy_count.* += 1;
    return id;
}

fn proxyName(t: Cfg.Terminal) []const u8 {
    return switch (t) {
        .byte => |v| std.fmt.comptimePrint("T_{X:0>2}", .{v}),
        .range => |r| std.fmt.comptimePrint("T_{X:0>2}-{X:0>2}", .{ r.lo, r.hi }),
        .string => |s| std.fmt.comptimePrint("T_{s}", .{s}),
        .string_ci => |s| std.fmt.comptimePrint("T_i_{s}", .{s}),
    };
}

/// Break productions with |rhs| > 2 into chains of binary productions.
///
/// `A -> B C D` becomes `A -> B X0`, `X0 -> C D`.
/// `A -> B C D E` becomes `A -> B X0`, `X0 -> C X1`, `X1 -> D E`.
///
/// Fresh nonterminals are named `B_0`, `B_1`, ... .
fn binStep(b: *CnfBuilder) void {
    // Snapshot.
    const old_count = b.prod_count;
    var old_prods: [max_prods]Production = undefined;
    for (b.prods[0..old_count], 0..) |prod, i| old_prods[i] = prod;
    b.prod_count = 0;

    var bin_id: usize = 0;

    for (old_prods[0..old_count]) |prod| {
        if (prod.rhs.len <= 2) {
            // Already binary (or unit/ε) - keep as-is.
            b.addProd(prod.lhs, prod.rhs);
            continue;
        }

        // Chain: A -> s0 s1 s2 ... sN
        // becomes: A -> s0 X, X -> s1 X', ..., X'' -> s(N-1) sN
        var lhs = prod.lhs;
        var remaining = prod.rhs;

        while (remaining.len > 2) {
            const name = std.fmt.comptimePrint("B_{d}", .{bin_id});
            bin_id += 1;
            const fresh = b.addNt(name);

            // lhs -> remaining[0] fresh
            const ss = b.sym_total;
            b.sym_pool[b.sym_total] = remaining[0];
            b.sym_total += 1;
            b.sym_pool[b.sym_total] = .{ .nonterminal = fresh };
            b.sym_total += 1;
            b.prods[b.prod_count] = .{
                .lhs = lhs,
                .rhs = b.sym_pool[ss..b.sym_total],
            };
            b.prod_count += 1;

            lhs = fresh;
            remaining = remaining[1..];
        }

        // Final binary pair: lhs -> remaining[0] remaining[1]
        b.addProd(lhs, remaining);
    }
}

/// Produce the final Cfg with productions grouped by LHS.
/// The start symbol's productions come first.
fn result(b: *CnfBuilder) Cfg {
    var sorted: [max_prods]Production = undefined;
    var sorted_count: usize = 0;

    // Start symbol first.
    for (b.prods[0..b.prod_count]) |prod| {
        if (prod.lhs == b.start) {
            sorted[sorted_count] = prod;
            sorted_count += 1;
        }
    }
    // Remaining nonterminals in id order.
    for (0..b.nt_count) |nt_id| {
        if (nt_id == b.start) continue;
        for (b.prods[0..b.prod_count]) |prod| {
            if (prod.lhs == @as(u32, @intCast(nt_id))) {
                sorted[sorted_count] = prod;
                sorted_count += 1;
            }
        }
    }

    return .{
        .nonterminals = b.nts[0..b.nt_count],
        .productions = sorted[0..sorted_count],
        .start = b.start,
    };
}

fn expectCnf(comptime source: []const u8, comptime expected: []const u8) !void {
    // Runs entirely at comptime: `toCnf()` produces a Cfg whose slices
    // point to comptime-var storage, so formatting must also be
    // comptime, otherwise we'd try to expose those pointers at runtime.
    const actual = comptime blk: {
        const cfg = Cfg.parse(source);
        const cnf = cfg.toCnf();
        break :blk std.fmt.comptimePrint("{f}", .{cnf});
    };
    try std.testing.expectEqualStrings(expected, actual);
}

test "START: start not on rhs - no change" {
    const cfg = comptime Cfg.parse(
        \\S -> A "x"
        \\A -> "a" | "b"
    );
    const cnf = comptime cfg.toCnf();

    // TERM adds proxy T_x -> "x" (1 NT, 1 prod).
    try std.testing.expectEqual(cfg.nonterminals.len + 1, cnf.nonterminals.len);
    try std.testing.expectEqual(cfg.productions.len + 1, cnf.productions.len);
    try std.testing.expectEqual(cfg.start, cnf.start);
}

test "START: start on rhs - fresh S0 added" {
    const cfg = comptime Cfg.parse(
        \\S -> A "x"
        \\A -> S | "a"
    );
    const cnf = comptime cfg.toCnf();

    // S0 added; unit prods resolved; TERM adds proxy.
    comptime try std.testing.expectEqualStrings("S0", cnf.nonterminalName(cnf.start));
    try std.testing.expectEqual(5, cnf.productions.len);
}

test "START: format with new S0" {
    try expectCnf(
        \\S -> A "x"
        \\A -> S | "a"
    ,
        \\S0 -> A T_x
        \\S -> A T_x
        \\A -> A T_x
        \\A -> "a"
        \\T_x -> "x"
    );
}

test "DEL: no nullable - no change" {
    const cfg = comptime Cfg.parse(
        \\S -> "a" | "b"
    );
    const cnf = comptime cfg.toCnf();

    try std.testing.expectEqual(2, cnf.productions.len);
}

test "DEL: basic ε-elimination" {
    try expectCnf(
        \\S -> A "b"
        \\A -> "a" |
    ,
        \\S -> A T_b
        \\S -> "b"
        \\A -> "a"
        \\T_b -> "b"
    );
}

test "DEL: nullable start gets ε back" {
    try expectCnf(
        \\S -> A
        \\A -> "a" |
    ,
        \\S -> ε
        \\S -> "a"
        \\A -> "a"
    );
}

test "DEL: duplicate combinations collapsed" {
    try expectCnf(
        \\S -> A A
        \\A -> "a" |
    ,
        \\S -> A A
        \\S -> ε
        \\S -> "a"
        \\A -> "a"
    );
}

test "DEL: transitive nullable" {
    try expectCnf(
        \\S -> A
        \\A -> B
        \\B -> "x" |
    ,
        \\S -> ε
        \\S -> "x"
        \\A -> "x"
        \\B -> "x"
    );
}

test "UNIT: basic unit elimination" {
    try expectCnf(
        \\S -> A
        \\A -> "x" "y"
    ,
        \\S -> T_x T_y
        \\A -> T_x T_y
        \\T_x -> "x"
        \\T_y -> "y"
    );
}

test "UNIT: chain A -> B -> C" {
    try expectCnf(
        \\S -> A
        \\A -> B
        \\B -> "x"
    ,
        \\S -> "x"
        \\A -> "x"
        \\B -> "x"
    );
}

test "UNIT: no unit productions - no change" {
    const cfg = comptime Cfg.parse(
        \\S -> "a" "b"
        \\S -> "c"
    );
    const cnf = comptime cfg.toCnf();

    // UNIT: no change. TERM: S -> "a" "b" rewritten + 2 proxies
    // (T_a -> "a", T_b -> "b") + single-terminal S -> "c" kept as-is = 4.
    try std.testing.expectEqual(4, cnf.productions.len);
}

test "TERM: terminal in mixed rhs replaced" {
    try expectCnf(
        \\S -> A "x" B
        \\A -> "a"
        \\B -> "b"
    ,
        \\S -> A B_0
        \\A -> "a"
        \\B -> "b"
        \\T_x -> "x"
        \\B_0 -> T_x B
    );
}

test "TERM: same terminal reused - single proxy" {
    try expectCnf(
        \\S -> "x" A "x"
        \\A -> "a"
    ,
        \\S -> T_x B_0
        \\A -> "a"
        \\T_x -> "x"
        \\B_0 -> A T_x
    );
}

test "TERM: single terminal rhs unchanged" {
    const cfg = comptime Cfg.parse(
        \\S -> "a"
    );
    const cnf = comptime cfg.toCnf();

    try std.testing.expectEqual(1, cnf.productions.len);
    try std.testing.expect(cnf.productions[0].rhs[0].eql(.{ .terminal = .{ .string = "a" } }));
}

test "BIN: binary and shorter rhs unchanged" {
    try expectCnf(
        \\S -> A B
        \\A -> "a"
        \\B -> "b"
    ,
        \\S -> A B
        \\A -> "a"
        \\B -> "b"
    );
}

test "BIN: length-3 rhs splits into binary chain" {
    try expectCnf(
        \\S -> A B C
        \\A -> "a"
        \\B -> "b"
        \\C -> "c"
    ,
        \\S -> A B_0
        \\A -> "a"
        \\B -> "b"
        \\C -> "c"
        \\B_0 -> B C
    );
}

test "BIN: length-4 rhs creates two fresh nonterminals" {
    try expectCnf(
        \\S -> A B C D
        \\A -> "a"
        \\B -> "b"
        \\C -> "c"
        \\D -> "d"
    ,
        \\S -> A B_0
        \\A -> "a"
        \\B -> "b"
        \\C -> "c"
        \\D -> "d"
        \\B_0 -> B B_1
        \\B_1 -> C D
    );
}
