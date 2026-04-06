/// Compiles AST nodes into bytecode for the grammar VM.
const std = @import("std");
const Ast = @import("../Ast.zig");
const I = @import("Instruction.zig");

const Optimizer = @import("Optimizer.zig");

pub const Config = struct {
    max_code: u32 = 4096,
    max_charsets: u16 = 256,
    max_patches: u32 = 512,
    max_string_data: u16 = 4096,
    max_captures: u16 = 64,
};

pub const Compiler = CompilerWith(.{});

pub fn CompilerWith(comptime config: Config) type {
    return struct {
        const Self = @This();

        code: [config.max_code]I.Inst = undefined,
        code_len: u32 = 0,

        charsets: [config.max_charsets]I.Charset = undefined,
        charset_len: u16 = 0,

        string_data: [config.max_string_data]u8 = undefined,
        string_data_len: u16 = 0,

        patches: [config.max_patches]Patch = undefined,
        patch_len: u32 = 0,

        capture_count: u16 = 0,
        memo_rule_count: u16 = 0,

        optimize_enabled: bool = true,

        rules: []const Ast.Rule = &.{},

        const Patch = struct {
            addr: u32,
            name: []const u8,
        };

        pub fn compile(rules: []const Ast.Rule) Self {
            return compileOpts(rules, .{});
        }

        pub const Options = struct {
            optimize: bool = true,
            memoize: bool = false,
        };

        pub fn compileOpts(rules: []const Ast.Rule, opts: Options) Self {
            var c = Self{};
            c.rules = rules;
            c.optimize_enabled = opts.optimize;

            if (rules.len == 0) {
                c.emit(.{ .op = .match });
            } else if (rules.len == 1) {
                c.compileNode(rules[0].node);
                c.emit(.{ .op = .match });
                c.patchCalls(&.{0}, rules);
            } else {
                const entry_call = c.emitPlaceholder();
                c.emit(.{ .op = .match });

                var rule_addrs: [256]u32 = undefined;
                var rule_ends: [256]u32 = undefined;
                for (rules, 0..) |rule, i| {
                    rule_addrs[i] = c.code_len;
                    c.compileNode(rule.node);
                    c.emit(.{ .op = .ret });
                    rule_ends[i] = c.code_len;
                }

                c.code[entry_call] = .{ .op = .call, .data = .{ .offset = rule_addrs[0] } };
                c.patchCalls(&rule_addrs, rules);

                if (opts.memoize) {
                    c.rewriteMemoCalls(rule_addrs[0..rules.len], rule_ends[0..rules.len]);
                }
            }

            if (c.optimize_enabled) {
                Optimizer.optimize(&c);
            }
            return c;
        }

        fn rewriteMemoCalls(self: *Self, rule_addrs: []const u32, rule_ends: []const u32) void {
            var memo_id: [256]?u16 = [_]?u16{null} ** 256;
            for (rule_addrs, rule_ends, 0..) |start, end, i| {
                var has_save = false;
                for (self.code[start..end]) |inst| {
                    if (inst.op == .save) {
                        has_save = true;
                        break;
                    }
                }
                if (!has_save) {
                    memo_id[i] = self.memo_rule_count;
                    self.memo_rule_count += 1;
                }
            }

            for (self.code[0..self.code_len]) |*inst| {
                if (inst.op != .call) continue;
                const target = inst.data.offset;
                for (rule_addrs, 0..) |addr, i| {
                    if (addr == target) {
                        if (memo_id[i]) |id| {
                            inst.* = .{
                                .op = .memo_call,
                                .data = .{ .memo = .{ .rule_id = id, .offset = target } },
                            };
                        }
                        break;
                    }
                }
            }
        }

        fn patchCalls(self: *Self, rule_addrs: []const u32, rules: []const Ast.Rule) void {
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
            }
        }

        fn compileNode(self: *Self, node: Ast.Node) void {
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
                    if (self.patch_len >= config.max_patches)
                        @panic("compiler patch buffer exhausted");
                    self.patches[self.patch_len] = .{
                        .addr = self.code_len,
                        .name = name,
                    };
                    self.patch_len += 1;
                    self.emit(.{ .op = .fail });
                },
                .and_predicate => |inner| {
                    const outer = self.emitPlaceholder();
                    const inner_choice = self.emitPlaceholder();
                    self.compileNode(inner.*);
                    const commit_addr = self.emitPlaceholder();
                    self.emit(.{ .op = .fail_twice });
                    self.code[inner_choice] = .{ .op = .choice, .data = .{ .offset = self.code_len - 1 } };
                    self.code[commit_addr] = .{ .op = .commit, .data = .{ .offset = self.code_len - 1 } };
                    self.code[outer] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
                    self.emit(.{ .op = .fail_twice });
                },
                .not_predicate => |inner| {
                    const choice_addr = self.emitPlaceholder();
                    self.compileNode(inner.*);
                    self.emit(.{ .op = .fail_twice });
                    self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
                },
                .capture => |inner| {
                    if (self.capture_count >= config.max_captures / 2)
                        @panic("too many capture groups");
                    const slot = self.capture_count * 2;
                    self.capture_count += 1;
                    self.emit(.{ .op = .save, .data = .{ .slot = slot } });
                    self.compileNode(inner.*);
                    self.emit(.{ .op = .save, .data = .{ .slot = slot + 1 } });
                },
                .anchor_start, .anchor_end, .prose_val => {},
            }
        }

        fn compileAlternation(self: *Self, alts: []const Ast.Node) void {
            if (alts.len == 0) return;
            if (alts.len == 1) {
                self.compileNode(alts[0]);
                return;
            }

            var commits: [256]u32 = undefined;
            var commit_count: usize = 0;

            for (alts[0 .. alts.len - 1]) |alt| {
                const choice_addr = self.emitPlaceholder();
                self.compileNode(alt);
                commits[commit_count] = self.emitPlaceholder();
                commit_count += 1;
                self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
            }

            self.compileNode(alts[alts.len - 1]);

            const end = self.code_len;
            for (commits[0..commit_count]) |addr| {
                self.code[addr] = .{ .op = .commit, .data = .{ .offset = end } };
            }
        }

        fn compileRepetition(self: *Self, rep: Ast.Repetition) void {
            for (0..rep.min) |_| {
                self.compileNode(rep.element.*);
            }

            if (rep.max) |max| {
                const optional = max - rep.min;
                for (0..optional) |_| {
                    const choice_addr = self.emitPlaceholder();
                    self.compileNode(rep.element.*);
                    const commit_addr = self.emitPlaceholder();
                    self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
                    self.code[commit_addr] = .{ .op = .commit, .data = .{ .offset = self.code_len } };
                }
            } else {
                const loop_start = self.code_len;
                const choice_addr = self.emitPlaceholder();
                self.compileNode(rep.element.*);
                self.emit(.{ .op = .commit, .data = .{ .offset = loop_start } });
                self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
            }
        }

        fn compileNumVal(self: *Self, nv: Ast.NumVal) void {
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

        fn addCharset(self: *Self, ranges: []const Ast.ClassRange) u16 {
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
            return self.findOrAddCharset(cs);
        }

        fn addCharsetFromRaw(self: *Self, ranges: []const [2]u8) u16 {
            return self.findOrAddCharset(I.charsetFromRanges(ranges));
        }

        fn findOrAddCharset(self: *Self, cs: I.Charset) u16 {
            for (self.charsets[0..self.charset_len], 0..) |existing, i| {
                if (existing[0] == cs[0] and existing[1] == cs[1] and
                    existing[2] == cs[2] and existing[3] == cs[3])
                {
                    return @intCast(i);
                }
            }
            if (self.charset_len >= config.max_charsets)
                @panic("compiler charset buffer exhausted");
            const idx = self.charset_len;
            self.charsets[idx] = cs;
            self.charset_len += 1;
            return idx;
        }

        fn emit(self: *Self, inst: I.Inst) void {
            if (self.code_len >= config.max_code)
                @panic("compiler code buffer exhausted");
            self.code[self.code_len] = inst;
            self.code_len += 1;
        }

        fn emitPlaceholder(self: *Self) u32 {
            const addr = self.code_len;
            self.emit(.{ .op = .jump, .data = .{ .offset = 0 } });
            return addr;
        }

        pub fn getCode(self: *const Self) []const I.Inst {
            return self.code[0..self.code_len];
        }

        pub fn getCharsets(self: *const Self) []const I.Charset {
            return self.charsets[0..self.charset_len];
        }

        pub fn getStringData(self: *const Self) []const u8 {
            return self.string_data[0..self.string_data_len];
        }

        pub fn getCaptureCount(self: *const Self) u16 {
            return self.capture_count;
        }

        pub fn getMemoRuleCount(self: *const Self) u16 {
            return self.memo_rule_count;
        }
    };
}
