const std = @import("std");
const Ast = @import("Ast.zig");

const Validator = @This();

/// Case-insensitive hash map context for rule name lookups (RFC 5234 §2.1).
const CaseInsensitiveContext = struct {
    pub fn hash(_: CaseInsensitiveContext, s: []const u8) u64 {
        var h: std.hash.Wyhash = .init(0);
        for (s) |c| h.update(&.{std.ascii.toLower(c)});
        return h.final();
    }
    pub fn eql(_: CaseInsensitiveContext, a: []const u8, b: []const u8) bool {
        return std.ascii.eqlIgnoreCase(a, b);
    }
};

fn CiHashMap(comptime V: type) type {
    return std.HashMap([]const u8, V, CaseInsensitiveContext, std.hash_map.default_max_load_percentage);
}

rules: []const Ast.Rule,
allocator: std.mem.Allocator,
diagnostics: std.ArrayList(Validation) = .empty,
/// Alternation slices allocated during `validate()` and embedded in
/// the returned rules. Callers must free them via `freeMerges` once
/// they're done with the merged rule set.
merges: std.ArrayList([]Ast.Node) = .empty,
/// Name of the start rule (exempt from "unused" check).
/// When null, the first rule is assumed to be the start rule.
start_rule: ?[]const u8 = null,
config: Config = .{},

pub const Config = struct {
    duplicate_rule: bool = true,
    undefined_rule: bool = true,
    unused_rule: bool = true,
    unproductive_rule: bool = true,
    left_recursive_rule: bool = true,
    zero_width_loop: bool = true,
};

pub const Validation = struct {
    kind: Kind,
    /// The rule in which the issue was found.
    rule_name: []const u8,
    /// The undefined reference name (only set for `.undefined_rule`).
    ref_name: ?[]const u8 = null,

    pub const Kind = enum {
        duplicate_rule,
        undefined_rule,
        unused_rule,
        unproductive_rule,
        left_recursive_rule,
        zero_width_loop,
    };
};

/// Core rules from RFC 5234 Appendix B — implicitly defined in every
/// ABNF grammar.
const core_rules = [_][]const u8{
    "ALPHA",  "BIT",  "CHAR",  "CR",
    "CRLF",   "CTL",  "DIGIT", "DQUOTE",
    "HEXDIG", "HTAB", "LF",    "LWSP",
    "OCTET",  "SP",   "VCHAR", "WSP",
};

pub fn init(allocator: std.mem.Allocator, rules: []const Ast.Rule) Validator {
    return .{ .rules = rules, .allocator = allocator };
}

/// Free all alternation slices allocated by `mergeAlternation`.
/// Call this once you're done with the merged rule set returned by
/// `validate()` (the slices are embedded in those rules).
pub fn freeMerges(self: *Validator) void {
    for (self.merges.items) |slice| self.allocator.free(slice);
    self.merges.deinit(self.allocator);
}

/// Validate the grammar and return the merged rule set.
///
/// Populates `self.diagnostics` with any issues found. The returned
/// rules have incremental alternations (`=/`) merged.
pub fn validate(self: *Validator) ![]const Ast.Rule {
    // Stage 1: merge rules, detect duplicates.
    var name_index = CiHashMap(usize).init(self.allocator);
    defer name_index.deinit();
    var merged: std.ArrayList(Ast.Rule) = .empty;

    for (self.rules) |rule| {
        const entry = try name_index.getOrPut(rule.name);
        if (entry.found_existing) {
            if (self.config.duplicate_rule and !rule.incremental) {
                try self.diagnostics.append(self.allocator, .{
                    .kind = .duplicate_rule,
                    .rule_name = rule.name,
                });
            }
            const existing = &merged.items[entry.value_ptr.*];
            existing.node = try self.mergeAlternation(existing.node, rule.node);
        } else {
            entry.value_ptr.* = merged.items.len;
            try merged.append(self.allocator, .{
                .name = rule.name,
                .node = rule.node,
                .incremental = false,
            });
        }
    }

    const merged_rules = try merged.toOwnedSlice(self.allocator);

    // Stages 2-4: reference-based checks (undefined, unused).
    // Collect refs only when at least one of these checks is enabled.
    if (self.config.undefined_rule or self.config.unused_rule) {
        var refs = CiHashMap(void).init(self.allocator);
        defer refs.deinit();
        for (merged_rules) |rule| {
            try self.collectRefs(rule.node, &refs);
        }

        if (self.config.undefined_rule) {
            var ref_iter = refs.keyIterator();
            while (ref_iter.next()) |ref_name| {
                if (name_index.contains(ref_name.*)) continue;
                if (isCoreRule(ref_name.*)) continue;
                const owner = self.findReferencer(merged_rules, ref_name.*);
                try self.diagnostics.append(self.allocator, .{
                    .kind = .undefined_rule,
                    .rule_name = owner orelse ref_name.*,
                    .ref_name = ref_name.*,
                });
            }
        }

        if (self.config.unused_rule) {
            for (merged_rules, 0..) |rule, i| {
                if (self.start_rule) |start| {
                    if (std.ascii.eqlIgnoreCase(rule.name, start)) continue;
                } else {
                    if (i == 0) continue;
                }
                if (!refs.contains(rule.name)) {
                    try self.diagnostics.append(self.allocator, .{
                        .kind = .unused_rule,
                        .rule_name = rule.name,
                    });
                }
            }
        }
    }

    // Stage 5: productivity / cycle detection.
    if (self.config.unproductive_rule) {
        var productive = try self.allocator.alloc(bool, merged_rules.len);
        defer self.allocator.free(productive);
        @memset(productive, false);

        var pending = try self.allocator.alloc(bool, merged_rules.len);
        defer self.allocator.free(pending);
        @memset(pending, true);

        var changed = true;
        while (changed) {
            changed = false;
            for (merged_rules, 0..) |rule, i| {
                if (!pending[i]) continue;
                if (isProductive(rule.node, merged_rules, &name_index, productive)) {
                    productive[i] = true;
                    changed = true;
                    pending[i] = false;
                    for (merged_rules, 0..) |other, j| {
                        if (!pending[j] and !productive[j] and
                            nodeReferences(other.node, rule.name))
                        {
                            pending[j] = true;
                        }
                    }
                }
            }
        }

        for (merged_rules, 0..) |rule, i| {
            if (!productive[i]) {
                try self.diagnostics.append(self.allocator, .{
                    .kind = .unproductive_rule,
                    .rule_name = rule.name,
                });
            }
        }
    }

    // Stages 6-8: nullable-based checks (left recursion, zero-width loops).
    // The nullable fixpoint only runs when at least one check needs it.
    if (self.config.left_recursive_rule or self.config.zero_width_loop) {
        var nullable = try self.allocator.alloc(bool, merged_rules.len);
        defer self.allocator.free(nullable);
        @memset(nullable, false);

        var nullable_changed = true;
        while (nullable_changed) {
            nullable_changed = false;
            for (merged_rules, 0..) |rule, i| {
                if (nullable[i]) continue;
                if (isNullable(rule.node, merged_rules, &name_index, nullable)) {
                    nullable[i] = true;
                    nullable_changed = true;
                }
            }
        }

        if (self.config.left_recursive_rule) {
            const visited = try self.allocator.alloc(bool, merged_rules.len);
            defer self.allocator.free(visited);
            for (merged_rules, 0..) |rule, i| {
                @memset(visited, false);
                collectLeftReachable(rule.node, merged_rules, &name_index, nullable, visited);
                if (visited[i]) {
                    try self.diagnostics.append(self.allocator, .{
                        .kind = .left_recursive_rule,
                        .rule_name = rule.name,
                    });
                }
            }
        }

        if (self.config.zero_width_loop) {
            for (merged_rules) |rule| {
                if (hasZeroWidthLoop(rule.node, merged_rules, &name_index, nullable)) {
                    try self.diagnostics.append(self.allocator, .{
                        .kind = .zero_width_loop,
                        .rule_name = rule.name,
                    });
                }
            }
        }
    }

    return merged_rules;
}

fn mergeAlternation(self: *Validator, a: Ast.Node, b: Ast.Node) !Ast.Node {
    var alts: std.ArrayList(Ast.Node) = .empty;

    switch (a) {
        .alternation => |items| for (items) |item| try alts.append(self.allocator, item),
        else => try alts.append(self.allocator, a),
    }
    switch (b) {
        .alternation => |items| for (items) |item| try alts.append(self.allocator, item),
        else => try alts.append(self.allocator, b),
    }

    const slice = try alts.toOwnedSlice(self.allocator);
    // Track the slice so callers can free it via `freeMerges` — merged
    // alternation slices live beyond `validate()` (they're embedded in
    // the returned rules), so the validator holds onto them.
    try self.merges.append(self.allocator, slice);
    return .{ .alternation = slice };
}

fn collectRefs(self: *Validator, node: Ast.Node, refs: *CiHashMap(void)) !void {
    switch (node) {
        .rulename => |name| try refs.put(name, {}),
        .alternation => |items| for (items) |item| try self.collectRefs(item, refs),
        .concatenation => |items| for (items) |item| try self.collectRefs(item, refs),
        .repetition => |rep| try self.collectRefs(rep.element.*, refs),
        .and_predicate => |inner| try self.collectRefs(inner.*, refs),
        .not_predicate => |inner| try self.collectRefs(inner.*, refs),
        .capture => |inner| try self.collectRefs(inner.*, refs),
        .char_val, .num_val, .prose_val, .char_class, .neg_char_class, .any, .anchor_start, .anchor_end => {},
    }
}

fn findReferencer(self: *Validator, rules: []const Ast.Rule, ref_name: []const u8) ?[]const u8 {
    _ = self;
    for (rules) |rule| {
        if (nodeReferences(rule.node, ref_name)) return rule.name;
    }
    return null;
}

fn nodeReferences(node: Ast.Node, name: []const u8) bool {
    return switch (node) {
        .rulename => |n| std.ascii.eqlIgnoreCase(n, name),
        .alternation => |items| for (items) |item| {
            if (nodeReferences(item, name)) return true;
        } else false,
        .concatenation => |items| for (items) |item| {
            if (nodeReferences(item, name)) return true;
        } else false,
        .repetition => |rep| nodeReferences(rep.element.*, name),
        .and_predicate => |inner| nodeReferences(inner.*, name),
        .not_predicate => |inner| nodeReferences(inner.*, name),
        .capture => |inner| nodeReferences(inner.*, name),
        .char_val, .num_val, .prose_val, .char_class, .neg_char_class, .any, .anchor_start, .anchor_end => false,
    };
}

fn isCoreRule(name: []const u8) bool {
    for (core_rules) |core| {
        if (std.ascii.eqlIgnoreCase(core, name)) return true;
    }
    return false;
}

fn isProductive(
    node: Ast.Node,
    merged_rules: []const Ast.Rule,
    name_index: *const CiHashMap(usize),
    productive: []const bool,
) bool {
    return switch (node) {
        .char_val, .num_val, .prose_val, .char_class, .neg_char_class, .any, .anchor_start, .anchor_end => true,
        .rulename => |name| {
            if (isCoreRule(name)) return true;
            if (name_index.get(name)) |idx| return productive[idx];
            // Undefined — treat as non-productive (already reported).
            return false;
        },
        .alternation => |items| for (items) |item| {
            if (isProductive(item, merged_rules, name_index, productive))
                return true;
        } else false,
        .concatenation => |items| {
            for (items) |item| {
                if (!isProductive(item, merged_rules, name_index, productive))
                    return false;
            }
            return true;
        },
        .repetition => |rep| {
            // *0 (min=0) is always productive (can match zero times).
            if (rep.min == 0) return true;
            return isProductive(rep.element.*, merged_rules, name_index, productive);
        },
        .and_predicate => |inner| isProductive(inner.*, merged_rules, name_index, productive),
        .not_predicate => |inner| isProductive(inner.*, merged_rules, name_index, productive),
        .capture => |inner| isProductive(inner.*, merged_rules, name_index, productive),
    };
}

/// Can this node succeed without consuming any input?
fn isNullable(
    node: Ast.Node,
    merged_rules: []const Ast.Rule,
    name_index: *const CiHashMap(usize),
    nullable: []const bool,
) bool {
    return switch (node) {
        // Terminals always consume input.
        .char_val => |cv| cv.value.len == 0,
        .num_val => |nv| switch (nv) {
            .concat => |c| c.len == 0,
            .single, .range => false,
        },
        .char_class, .neg_char_class, .any => false,
        // Anchors and predicates succeed without consuming.
        .anchor_start, .anchor_end => true,
        .and_predicate, .not_predicate => true,
        .prose_val => false,
        .rulename => |name| {
            // Conservatively treat all core rules as non-nullable.
            // LWSP = *(WSP / CRLF WSP) is technically nullable, so a
            // left-recursive path through LWSP would be missed. This is
            // acceptable: core rule semantics are fixed by RFC 5234.
            if (isCoreRule(name)) return false;
            if (name_index.get(name)) |idx| return nullable[idx];
            return false;
        },
        .alternation => |items| for (items) |item| {
            if (isNullable(item, merged_rules, name_index, nullable))
                return true;
        } else false,
        .concatenation => |items| {
            for (items) |item| {
                if (!isNullable(item, merged_rules, name_index, nullable))
                    return false;
            }
            return true;
        },
        .repetition => |rep| {
            if (rep.min == 0) return true;
            return isNullable(rep.element.*, merged_rules, name_index, nullable);
        },
        .capture => |inner| isNullable(inner.*, merged_rules, name_index, nullable),
    };
}

/// Mark all rules that are reachable from `node` before any input is
/// consumed (i.e. through nullable prefixes) in `visited`.
fn collectLeftReachable(
    node: Ast.Node,
    merged_rules: []const Ast.Rule,
    name_index: *const CiHashMap(usize),
    nullable: []const bool,
    visited: []bool,
) void {
    switch (node) {
        .rulename => |name| {
            const idx = name_index.get(name) orelse return;
            if (visited[idx]) return;
            visited[idx] = true;
            // Recurse into the rule body.
            collectLeftReachable(
                merged_rules[idx].node,
                merged_rules,
                name_index,
                nullable,
                visited,
            );
        },
        .alternation => |items| {
            for (items) |item| {
                collectLeftReachable(item, merged_rules, name_index, nullable, visited);
            }
        },
        .concatenation => |items| {
            for (items) |item| {
                collectLeftReachable(item, merged_rules, name_index, nullable, visited);
                if (!isNullable(item, merged_rules, name_index, nullable)) break;
            }
        },
        .repetition => |rep| {
            if (rep.min > 0) {
                collectLeftReachable(rep.element.*, merged_rules, name_index, nullable, visited);
            }
            // min==0 means the repetition can be skipped entirely,
            // so the element is not on the forced left path.
        },
        .and_predicate, .not_predicate => {
            // Predicates are lookaheads: they test but never consume
            // input or descend into the referenced rule, so they do
            // not contribute to left-recursion.
        },
        .capture => |inner| {
            collectLeftReachable(inner.*, merged_rules, name_index, nullable, visited);
        },
        .char_val,
        .num_val,
        .prose_val,
        .char_class,
        .neg_char_class,
        .any,
        .anchor_start,
        .anchor_end,
        => {},
    }
}

/// Does this node contain an unbounded repetition whose body is nullable?
fn hasZeroWidthLoop(
    node: Ast.Node,
    merged_rules: []const Ast.Rule,
    name_index: *const CiHashMap(usize),
    nullable: []const bool,
) bool {
    return switch (node) {
        .repetition => |rep| {
            // Unbounded (max == null) and body can match empty.
            if (rep.max == null and
                isNullable(rep.element.*, merged_rules, name_index, nullable))
            {
                return true;
            }
            // Also check recursively inside the body.
            return hasZeroWidthLoop(rep.element.*, merged_rules, name_index, nullable);
        },
        .alternation => |items| for (items) |item| {
            if (hasZeroWidthLoop(item, merged_rules, name_index, nullable))
                return true;
        } else false,
        .concatenation => |items| for (items) |item| {
            if (hasZeroWidthLoop(item, merged_rules, name_index, nullable))
                return true;
        } else false,
        .and_predicate => |inner| hasZeroWidthLoop(inner.*, merged_rules, name_index, nullable),
        .not_predicate => |inner| hasZeroWidthLoop(inner.*, merged_rules, name_index, nullable),
        .capture => |inner| hasZeroWidthLoop(inner.*, merged_rules, name_index, nullable),
        .char_val,
        .num_val,
        .prose_val,
        .char_class,
        .neg_char_class,
        .any,
        .anchor_start,
        .anchor_end,
        .rulename,
        => false,
    };
}

const Scanner = @import("abnf/Scanner.zig");
const Parser = @import("abnf/Parser.zig");
const PegScanner = @import("peg/Scanner.zig");
const PegParser = @import("peg/Parser.zig");

const TestResult = struct {
    rules: []const Ast.Rule,
    diagnostics: []const Validation,
    merges: [][]Ast.Node,
    allocator: std.mem.Allocator,

    fn deinit(self: TestResult) void {
        for (self.merges) |slice| self.allocator.free(slice);
        self.allocator.free(self.merges);
        self.allocator.free(self.rules);
        self.allocator.free(self.diagnostics);
    }
};

fn parseAndValidate(allocator: std.mem.Allocator, source: []const u8) !TestResult {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    try std.testing.expectEqual(0, parser.getDiagnostics().len);
    var validator = Validator.init(allocator, rules);
    const merged = try validator.validate();
    return .{
        .rules = merged,
        .diagnostics = try validator.diagnostics.toOwnedSlice(allocator),
        .merges = try validator.merges.toOwnedSlice(allocator),
        .allocator = allocator,
    };
}

fn pegParseAndValidate(allocator: std.mem.Allocator, source: []const u8) !TestResult {
    var scanner = PegScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, source);
    const rules = try parser.parse();
    try std.testing.expectEqual(0, parser.getDiagnostics().len);
    var validator = Validator.init(allocator, rules);
    const merged = try validator.validate();
    return .{
        .rules = merged,
        .diagnostics = try validator.diagnostics.toOwnedSlice(allocator),
        .merges = try validator.merges.toOwnedSlice(allocator),
        .allocator = allocator,
    };
}

test "clean grammar — no diagnostics" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = bar\nbar = \"hello\"");
    defer result.deinit();
    try std.testing.expectEqual(0, result.diagnostics.len);
    try std.testing.expectEqual(2, result.rules.len);
}

test "core rules not flagged as undefined" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = ALPHA DIGIT");
    defer result.deinit();
    try std.testing.expectEqual(0, result.diagnostics.len);
}

test "undefined rule reference" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = bar");
    defer result.deinit();
    // "foo" also ends up flagged as unproductive (it depends only on the
    // undefined "bar"), so we just look for the undefined_rule.
    var found: ?Validation = null;
    for (result.diagnostics) |d| if (d.kind == .undefined_rule) {
        found = d;
    };
    try std.testing.expect(found != null);
    try std.testing.expectEqualStrings("bar", found.?.ref_name.?);
}

test "unused rule detected" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = \"a\"\nbar = \"b\"");
    defer result.deinit();
    try std.testing.expectEqual(1, result.diagnostics.len);
    try std.testing.expectEqual(.unused_rule, result.diagnostics[0].kind);
    try std.testing.expectEqualStrings("bar", result.diagnostics[0].rule_name);
}

test "start rule not flagged as unused" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = \"a\"");
    defer result.deinit();
    try std.testing.expectEqual(0, result.diagnostics.len);
}

test "duplicate = definition" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = \"a\"\nfoo = \"b\"");
    defer result.deinit();
    try std.testing.expectEqual(1, result.diagnostics.len);
    try std.testing.expectEqual(.duplicate_rule, result.diagnostics[0].kind);
    try std.testing.expectEqualStrings("foo", result.diagnostics[0].rule_name);
}

test "=/ not flagged as duplicate" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = \"a\"\nfoo =/ \"b\"");
    defer result.deinit();
    try std.testing.expectEqual(0, result.diagnostics.len);
    // Merged into one rule with alternation.
    try std.testing.expectEqual(1, result.rules.len);
    try std.testing.expectEqual(2, result.rules[0].node.alternation.len);
}

test "simple cycle detected as unproductive" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "a = b\nb = a");
    defer result.deinit();
    // Both rules are unproductive (plus undefined refs won't fire since
    // both are defined).
    var unproductive_count: usize = 0;
    for (result.diagnostics) |d| {
        if (d.kind == .unproductive_rule) unproductive_count += 1;
    }
    try std.testing.expectEqual(2, unproductive_count);
}

test "cycle with terminal escape is productive" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "a = b / \"x\"\nb = a");
    defer result.deinit();
    for (result.diagnostics) |d| {
        try std.testing.expect(d.kind != .unproductive_rule);
    }
}

test "incremental alternation merges via validator" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = \"a\"\nfoo =/ \"b\"");
    defer result.deinit();
    try std.testing.expectEqual(1, result.rules.len);
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(2, alts.len);
    try std.testing.expectEqualStrings("a", alts[0].char_val.value);
    try std.testing.expectEqualStrings("b", alts[1].char_val.value);
}

test "mixed-case rule names merge as duplicates" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "Foo = \"a\"\nfoo = \"b\"");
    defer result.deinit();
    try std.testing.expectEqual(1, result.diagnostics.len);
    try std.testing.expectEqual(.duplicate_rule, result.diagnostics[0].kind);
    try std.testing.expectEqual(1, result.rules.len);
}

test "mixed-case incremental alternation merges" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "Foo = \"a\"\nfoo =/ \"b\"");
    defer result.deinit();
    try std.testing.expectEqual(0, result.diagnostics.len);
    try std.testing.expectEqual(1, result.rules.len);
    try std.testing.expectEqual(2, result.rules[0].node.alternation.len);
}

test "mixed-case reference not flagged as undefined" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = Bar\nbar = \"x\"");
    defer result.deinit();
    for (result.diagnostics) |d| {
        try std.testing.expect(d.kind != .undefined_rule);
    }
}

test "mixed-case reference counts as used" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = Bar\nbar = \"x\"");
    defer result.deinit();
    for (result.diagnostics) |d| {
        try std.testing.expect(d.kind != .unused_rule);
    }
}

test "mixed-case undefined reference reports correct owner" {
    const allocator = std.testing.allocator;
    // "foo" references "Missing" which is undefined — diagnostic should
    // identify "foo" as the rule containing the bad reference regardless
    // of casing.
    const result = try parseAndValidate(allocator, "foo = Missing");
    defer result.deinit();
    // "foo" also ends up flagged as unproductive; find the undefined_rule.
    var found: ?Validation = null;
    for (result.diagnostics) |d| if (d.kind == .undefined_rule) {
        found = d;
    };
    try std.testing.expect(found != null);
    try std.testing.expectEqualStrings("foo", found.?.rule_name);
    try std.testing.expectEqualStrings("Missing", found.?.ref_name.?);
}

test "direct left recursion detected" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "a = a \"x\" / \"y\"");
    defer result.deinit();
    var count: usize = 0;
    for (result.diagnostics) |d| {
        if (d.kind == .left_recursive_rule) count += 1;
    }
    try std.testing.expectEqual(1, count);
}

test "indirect left recursion detected" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "a = b \"x\"\nb = a \"y\"");
    defer result.deinit();
    var count: usize = 0;
    for (result.diagnostics) |d| {
        if (d.kind == .left_recursive_rule) count += 1;
    }
    // Both a and b are left-recursive.
    try std.testing.expectEqual(2, count);
}

test "right recursion is not left recursion" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "a = \"x\" a / \"y\"");
    defer result.deinit();
    for (result.diagnostics) |d| {
        try std.testing.expect(d.kind != .left_recursive_rule);
    }
}

test "left recursion through nullable prefix" {
    const allocator = std.testing.allocator;
    // *"x" can match zero chars, so `a` is reachable at position 0.
    const result = try parseAndValidate(allocator, "a = *\"x\" a / \"y\"");
    defer result.deinit();
    var count: usize = 0;
    for (result.diagnostics) |d| {
        if (d.kind == .left_recursive_rule) count += 1;
    }
    try std.testing.expectEqual(1, count);
}

test "zero-width loop detected" {
    const allocator = std.testing.allocator;
    // b = *"x" is nullable; a = *b loops forever.
    const result = try parseAndValidate(allocator, "a = 1*b\nb = *\"x\"");
    defer result.deinit();
    var count: usize = 0;
    for (result.diagnostics) |d| {
        if (d.kind == .zero_width_loop) count += 1;
    }
    try std.testing.expectEqual(1, count);
}

test "bounded repetition of nullable body is not zero-width loop" {
    const allocator = std.testing.allocator;
    // 3*5b where b is nullable — bounded, will terminate.
    const result = try parseAndValidate(allocator, "a = 3*5b\nb = *\"x\"");
    defer result.deinit();
    for (result.diagnostics) |d| {
        try std.testing.expect(d.kind != .zero_width_loop);
    }
}

test "non-nullable repetition body is not zero-width loop" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "a = *\"x\"");
    defer result.deinit();
    for (result.diagnostics) |d| {
        try std.testing.expect(d.kind != .zero_width_loop);
    }
}

test "predicate-guarded self-reference is not left-recursive" {
    const allocator = std.testing.allocator;
    // !a is a lookahead guard, not a recursive descent into a.
    const result = try pegParseAndValidate(allocator, "a <- !a 'x'");
    defer result.deinit();
    for (result.diagnostics) |d| {
        try std.testing.expect(d.kind != .left_recursive_rule);
    }
}

test "and-predicate self-reference is not left-recursive" {
    const allocator = std.testing.allocator;
    const result = try pegParseAndValidate(allocator, "a <- &a 'x'");
    defer result.deinit();
    for (result.diagnostics) |d| {
        try std.testing.expect(d.kind != .left_recursive_rule);
    }
}

test "disabled check suppresses diagnostic" {
    const allocator = std.testing.allocator;
    // "a = a" is direct left recursion, but with the check disabled
    // the validator must not emit a left_recursive_rule diagnostic.
    const result = try parseAndValidate(allocator, "a = a \"x\" / \"y\"");
    defer result.deinit();
    // Sanity: default config does flag it.
    var found = false;
    for (result.diagnostics) |d| {
        if (d.kind == .left_recursive_rule) found = true;
    }
    try std.testing.expect(found);

    // Now with the check disabled.
    var scanner = Scanner.init("a = a \"x\" / \"y\"");
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, "a = a \"x\" / \"y\"");
    const rules = try parser.parse();
    var validator = Validator.init(allocator, rules);
    validator.config.left_recursive_rule = false;
    const merged = try validator.validate();
    defer allocator.free(merged);
    const diags = try validator.diagnostics.toOwnedSlice(allocator);
    defer allocator.free(diags);
    for (diags) |d| {
        try std.testing.expect(d.kind != .left_recursive_rule);
    }
}
