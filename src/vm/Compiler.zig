/// Compiles AST nodes into bytecode for the grammar VM.
const std = @import("std");
const Ast = @import("../Ast.zig");
const I = @import("Instruction.zig");

const Compiler = @This();

const max_code = 4096;
const max_charsets = 256;
const max_patches = 512;

code: [max_code]I.Inst = undefined,
code_len: u32 = 0,

charsets: [max_charsets]I.Charset = undefined,
charset_len: u16 = 0,

/// Pending call patches: instruction addresses that need a rule resolved.
patches: [max_patches]Patch = undefined,
patch_len: u32 = 0,

rules: []const Ast.Rule = &.{},

const Patch = struct {
    /// Address of the call instruction to patch.
    addr: u32,
    /// Rule name to resolve.
    name: []const u8,
};

pub fn compile(rules: []const Ast.Rule) Compiler {
    var c = Compiler{};
    c.rules = rules;

    if (rules.len == 0) {
        c.emit(.{ .op = .match });
        return c;
    }

    // For a single rule (e.g. ERE), compile its node directly.
    if (rules.len == 1) {
        c.compileNode(rules[0].node);
        c.emit(.{ .op = .match });
        c.patchCalls(&.{0}, rules);
        return c;
    }

    // Multi-rule grammar: emit call to first rule, then match,
    // then compile each rule as a callable block.
    const entry_call = c.emitPlaceholder();
    c.emit(.{ .op = .match });

    var rule_addrs: [256]u32 = undefined;
    for (rules, 0..) |rule, i| {
        rule_addrs[i] = c.code_len;
        c.compileNode(rule.node);
        c.emit(.{ .op = .ret });
    }

    // Patch the entry call to point at the first rule.
    c.code[entry_call] = .{ .op = .call, .data = .{ .offset = rule_addrs[0] } };
    c.patchCalls(&rule_addrs, rules);
    return c;
}

/// Resolve all pending rulename call patches.
fn patchCalls(self: *Compiler, rule_addrs: []const u32, rules: []const Ast.Rule) void {
    for (self.patches[0..self.patch_len]) |patch| {
        for (rules, 0..) |rule, i| {
            if (std.mem.eql(u8, rule.name, patch.name)) {
                self.code[patch.addr] = .{
                    .op = .call,
                    .data = .{ .offset = rule_addrs[i] },
                };
                break;
            }
        }
        // Unresolved names stay as fail.
    }
}

fn compileNode(self: *Compiler, node: Ast.Node) void {
    switch (node) {
        .char_val => |cv| {
            for (cv.value) |b| {
                self.emit(.{ .op = .char, .data = .{ .byte = b } });
            }
        },
        .any => self.emit(.{ .op = .any }),
        .concatenation => |nodes| {
            for (nodes) |n| self.compileNode(n);
        },
        .alternation => |alts| self.compileAlternation(alts),
        .repetition => |rep| self.compileRepetition(rep),
        .char_class => |ranges| {
            const idx = self.addCharset(ranges);
            self.emit(.{ .op = .set, .data = .{ .charset = idx } });
        },
        .neg_char_class => |ranges| {
            const idx = self.addCharset(ranges);
            self.emit(.{ .op = .neg_set, .data = .{ .charset = idx } });
        },
        .num_val => |nv| self.compileNumVal(nv),
        .rulename => |name| {
            self.patches[self.patch_len] = .{
                .addr = self.code_len,
                .name = name,
            };
            self.patch_len += 1;
            // Placeholder; patched later by patchCalls.
            self.emit(.{ .op = .fail });
        },
        .and_predicate => |inner| {
            // &e = !!e
            // choice L1; choice L2; <e>; commit L3; fail_twice; L1:
            const outer = self.emitPlaceholder(); // choice L1
            const inner_choice = self.emitPlaceholder(); // choice L2
            self.compileNode(inner.*);
            const commit_addr = self.emitPlaceholder(); // commit L3
            self.emit(.{ .op = .fail_twice }); // L2 target
            // L3: fail_twice
            self.code[inner_choice] = .{ .op = .choice, .data = .{ .offset = self.code_len - 1 } };
            self.code[commit_addr] = .{ .op = .commit, .data = .{ .offset = self.code_len - 1 } };
            // L1:
            self.code[outer] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
            self.emit(.{ .op = .fail_twice });
        },
        .not_predicate => |inner| {
            // !e: choice L; <e>; fail_twice; L:
            const choice_addr = self.emitPlaceholder();
            self.compileNode(inner.*);
            self.emit(.{ .op = .fail_twice });
            self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
        },
        .anchor_start, .anchor_end, .prose_val => {
            // Not supported in VM yet.
        },
    }
}

/// Ordered choice: a / b / c
/// choice Lb; <a>; commit Lend; Lb: choice Lc; <b>; commit Lend; Lc: <c>; Lend:
fn compileAlternation(self: *Compiler, alts: []const Ast.Node) void {
    if (alts.len == 0) return;
    if (alts.len == 1) {
        self.compileNode(alts[0]);
        return;
    }

    var commits: [256]u32 = undefined;
    var commit_count: usize = 0;

    for (alts[0 .. alts.len - 1], 0..) |alt, i| {
        _ = i;
        const choice_addr = self.emitPlaceholder();
        self.compileNode(alt);
        commits[commit_count] = self.emitPlaceholder();
        commit_count += 1;
        // Patch choice to point to next alternative.
        self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
    }

    // Last alternative: no choice needed.
    self.compileNode(alts[alts.len - 1]);

    // Patch all commits to jump past the last alternative.
    const end = self.code_len;
    for (commits[0..commit_count]) |addr| {
        self.code[addr] = .{ .op = .commit, .data = .{ .offset = end } };
    }
}

/// Repetition: e{min,max}
fn compileRepetition(self: *Compiler, rep: Ast.Repetition) void {
    // Emit min required copies.
    for (0..rep.min) |_| {
        self.compileNode(rep.element.*);
    }

    if (rep.max) |max| {
        // Bounded: emit (max - min) optional copies.
        // Each: choice Lskip; <e>; commit Lnext; Lskip:
        const optional = max - rep.min;
        for (0..optional) |_| {
            const choice_addr = self.emitPlaceholder();
            self.compileNode(rep.element.*);
            const commit_addr = self.emitPlaceholder();
            self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
            self.code[commit_addr] = .{ .op = .commit, .data = .{ .offset = self.code_len } };
        }
    } else {
        // Unbounded: e* = L: choice Lend; <e>; commit L; Lend:
        const loop_start = self.code_len;
        const choice_addr = self.emitPlaceholder();
        self.compileNode(rep.element.*);
        self.emit(.{ .op = .commit, .data = .{ .offset = loop_start } });
        self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
    }
}

fn compileNumVal(self: *Compiler, nv: Ast.NumVal) void {
    switch (nv) {
        .single => |b| self.emit(.{ .op = .char, .data = .{ .byte = b } }),
        .range => |r| {
            const ranges = [_][2]u8{.{ r.lo, r.hi }};
            const idx = self.addCharsetFromRaw(&ranges);
            self.emit(.{ .op = .set, .data = .{ .charset = idx } });
        },
        .concat => |bytes| {
            for (bytes) |b| {
                self.emit(.{ .op = .char, .data = .{ .byte = b } });
            }
        },
    }
}

fn addCharset(self: *Compiler, ranges: []const Ast.ClassRange) u16 {
    var cs = I.Charset{ 0, 0, 0, 0 };
    for (ranges) |r| {
        var b: u16 = r.lo;
        while (b <= r.hi) : (b += 1) {
            const byte: u8 = @intCast(b);
            const word = byte >> 6;
            const bit: u6 = @truncate(byte);
            cs[word] |= @as(u64, 1) << bit;
        }
    }
    const idx = self.charset_len;
    self.charsets[idx] = cs;
    self.charset_len += 1;
    return idx;
}

fn addCharsetFromRaw(self: *Compiler, ranges: []const [2]u8) u16 {
    const idx = self.charset_len;
    self.charsets[idx] = I.charsetFromRanges(ranges);
    self.charset_len += 1;
    return idx;
}

fn emit(self: *Compiler, inst: I.Inst) void {
    self.code[self.code_len] = inst;
    self.code_len += 1;
}

fn emitPlaceholder(self: *Compiler) u32 {
    const addr = self.code_len;
    self.emit(.{ .op = .jump, .data = .{ .offset = 0 } });
    return addr;
}

pub fn getCode(self: *const Compiler) []const I.Inst {
    return self.code[0..self.code_len];
}

pub fn getCharsets(self: *const Compiler) []const I.Charset {
    return self.charsets[0..self.charset_len];
}
