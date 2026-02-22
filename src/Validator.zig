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
/// Name of the start rule (exempt from "unused" check).
/// When null, the first rule is assumed to be the start rule.
start_rule: ?[]const u8 = null,

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
            if (!rule.incremental) {
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

    // Stage 2: collect all referenced rule names.
    var refs = CiHashMap(void).init(self.allocator);
    defer refs.deinit();
    for (merged_rules) |rule| {
        try self.collectRefs(rule.node, &refs);
    }

    // Stage 3: check for undefined references.
    var ref_iter = refs.keyIterator();
    while (ref_iter.next()) |ref_name| {
        if (name_index.contains(ref_name.*)) continue;
        if (isCoreRule(ref_name.*)) continue;
        // Find the first rule that references this undefined name.
        const owner = self.findReferencer(merged_rules, ref_name.*);
        try self.diagnostics.append(self.allocator, .{
            .kind = .undefined_rule,
            .rule_name = owner orelse ref_name.*,
            .ref_name = ref_name.*,
        });
    }

    // Stage 4: unused rules (skip the start rule).
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

    // Stage 5: productivity / cycle detection.
    var productive = try self.allocator.alloc(bool, merged_rules.len);
    defer self.allocator.free(productive);
    @memset(productive, false);

    var changed = true;
    while (changed) {
        changed = false;
        for (merged_rules, 0..) |rule, i| {
            if (productive[i]) continue;
            if (self.isProductive(rule.node, merged_rules, &name_index, productive)) {
                productive[i] = true;
                changed = true;
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

    return merged_rules;
}

// --- Helpers -----------------------------------------------------------------

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

    return .{ .alternation = try alts.toOwnedSlice(self.allocator) };
}

fn collectRefs(self: *Validator, node: Ast.Node, refs: *CiHashMap(void)) !void {
    switch (node) {
        .rulename => |name| try refs.put(name, {}),
        .alternation => |items| for (items) |item| try self.collectRefs(item, refs),
        .concatenation => |items| for (items) |item| try self.collectRefs(item, refs),
        .repetition => |rep| try self.collectRefs(rep.element.*, refs),
        .char_val, .num_val, .prose_val => {},
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
        .char_val, .num_val, .prose_val => false,
    };
}

fn isCoreRule(name: []const u8) bool {
    for (core_rules) |core| {
        if (std.ascii.eqlIgnoreCase(core, name)) return true;
    }
    return false;
}

fn isProductive(
    self: *Validator,
    node: Ast.Node,
    merged_rules: []const Ast.Rule,
    name_index: *const CiHashMap(usize),
    productive: []const bool,
) bool {
    _ = self;
    return switch (node) {
        .char_val, .num_val, .prose_val => true,
        .rulename => |name| {
            if (isCoreRule(name)) return true;
            if (name_index.get(name)) |idx| return productive[idx];
            // Undefined — treat as non-productive (already reported).
            return false;
        },
        .alternation => |items| for (items) |item| {
            if (isProductive(undefined, item, merged_rules, name_index, productive))
                return true;
        } else false,
        .concatenation => |items| {
            for (items) |item| {
                if (!isProductive(undefined, item, merged_rules, name_index, productive))
                    return false;
            }
            return true;
        },
        .repetition => |rep| {
            // *0 (min=0) is always productive (can match zero times).
            if (rep.min == 0) return true;
            return isProductive(undefined, rep.element.*, merged_rules, name_index, productive);
        },
    };
}

// --- Tests -------------------------------------------------------------------

const Scanner = @import("abnf/Scanner.zig");
const Parser = @import("abnf/Parser.zig");

fn parseAndValidate(allocator: std.mem.Allocator, source: []const u8) !struct {
    rules: []const Ast.Rule,
    diagnostics: []const Validation,
} {
    var scanner = Scanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = Parser.init(tokens, source);
    const rules = try parser.parse();
    try std.testing.expectEqual(0, parser.getDiagnostics().len);
    var validator = Validator.init(allocator, rules);
    const merged = try validator.validate();
    return .{ .rules = merged, .diagnostics = try validator.diagnostics.toOwnedSlice(allocator) };
}

test "clean grammar — no diagnostics" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = bar\nbar = \"hello\"");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    try std.testing.expectEqual(0, result.diagnostics.len);
    try std.testing.expectEqual(2, result.rules.len);
}

test "core rules not flagged as undefined" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = ALPHA DIGIT");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    try std.testing.expectEqual(0, result.diagnostics.len);
}

test "undefined rule reference" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = bar");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    try std.testing.expectEqual(1, result.diagnostics.len);
    try std.testing.expectEqual(.undefined_rule, result.diagnostics[0].kind);
    try std.testing.expectEqualStrings("bar", result.diagnostics[0].ref_name.?);
}

test "unused rule detected" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = \"a\"\nbar = \"b\"");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    try std.testing.expectEqual(1, result.diagnostics.len);
    try std.testing.expectEqual(.unused_rule, result.diagnostics[0].kind);
    try std.testing.expectEqualStrings("bar", result.diagnostics[0].rule_name);
}

test "start rule not flagged as unused" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = \"a\"");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    try std.testing.expectEqual(0, result.diagnostics.len);
}

test "duplicate = definition" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = \"a\"\nfoo = \"b\"");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    try std.testing.expectEqual(1, result.diagnostics.len);
    try std.testing.expectEqual(.duplicate_rule, result.diagnostics[0].kind);
    try std.testing.expectEqualStrings("foo", result.diagnostics[0].rule_name);
}

test "=/ not flagged as duplicate" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = \"a\"\nfoo =/ \"b\"");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    try std.testing.expectEqual(0, result.diagnostics.len);
    // Merged into one rule with alternation.
    try std.testing.expectEqual(1, result.rules.len);
    try std.testing.expectEqual(2, result.rules[0].node.alternation.len);
}

test "simple cycle detected as unproductive" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "a = b\nb = a");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
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
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    for (result.diagnostics) |d| {
        try std.testing.expect(d.kind != .unproductive_rule);
    }
}

test "incremental alternation merges via validator" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = \"a\"\nfoo =/ \"b\"");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    try std.testing.expectEqual(1, result.rules.len);
    const alts = result.rules[0].node.alternation;
    try std.testing.expectEqual(2, alts.len);
    try std.testing.expectEqualStrings("a", alts[0].char_val.value);
    try std.testing.expectEqualStrings("b", alts[1].char_val.value);
}

test "mixed-case rule names merge as duplicates" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "Foo = \"a\"\nfoo = \"b\"");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    try std.testing.expectEqual(1, result.diagnostics.len);
    try std.testing.expectEqual(.duplicate_rule, result.diagnostics[0].kind);
    try std.testing.expectEqual(1, result.rules.len);
}

test "mixed-case incremental alternation merges" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "Foo = \"a\"\nfoo =/ \"b\"");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    try std.testing.expectEqual(0, result.diagnostics.len);
    try std.testing.expectEqual(1, result.rules.len);
    try std.testing.expectEqual(2, result.rules[0].node.alternation.len);
}

test "mixed-case reference not flagged as undefined" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = Bar\nbar = \"x\"");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    for (result.diagnostics) |d| {
        try std.testing.expect(d.kind != .undefined_rule);
    }
}

test "mixed-case reference counts as used" {
    const allocator = std.testing.allocator;
    const result = try parseAndValidate(allocator, "foo = Bar\nbar = \"x\"");
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
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
    defer allocator.free(result.rules);
    defer allocator.free(result.diagnostics);
    try std.testing.expectEqual(1, result.diagnostics.len);
    try std.testing.expectEqual(.undefined_rule, result.diagnostics[0].kind);
    try std.testing.expectEqualStrings("foo", result.diagnostics[0].rule_name);
    try std.testing.expectEqualStrings("Missing", result.diagnostics[0].ref_name.?);
}
