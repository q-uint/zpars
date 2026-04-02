/// ERE formatter — reconstructs a POSIX Extended Regular Expression from AST.
const std = @import("std");
const Ast = @import("../Ast.zig");

/// Format an ERE rule (single pattern) to the writer.
pub fn formatRule(rule: Ast.Rule, writer: anytype) !void {
    try formatNode(rule.node, .top, writer);
}

const Context = enum { top, alternation, concatenation, quantified };

fn formatNode(node: Ast.Node, ctx: Context, writer: anytype) anyerror!void {
    switch (node) {
        .alternation => |alts| {
            const needs_group = ctx == .quantified or ctx == .concatenation;
            if (needs_group) try writer.writeByte('(');
            for (alts, 0..) |alt, i| {
                if (i > 0) try writer.writeByte('|');
                try formatNode(alt, .alternation, writer);
            }
            if (needs_group) try writer.writeByte(')');
        },
        .concatenation => |elems| {
            const needs_group = ctx == .quantified;
            if (needs_group) try writer.writeByte('(');
            for (elems) |elem| {
                try formatNode(elem, .concatenation, writer);
            }
            if (needs_group) try writer.writeByte(')');
        },
        .repetition => |rep| {
            try formatNode(rep.element.*, .quantified, writer);
            try formatQuantifier(rep, writer);
        },
        .char_val => |cv| {
            for (cv.value) |c| {
                if (isMetachar(c)) try writer.writeByte('\\');
                try writer.writeByte(c);
            }
        },
        .char_class => |ranges| try formatCharClass(ranges, false, writer),
        .neg_char_class => |ranges| try formatCharClass(ranges, true, writer),
        .any => try writer.writeByte('.'),
        .anchor_start => try writer.writeByte('^'),
        .anchor_end => try writer.writeByte('$'),
        .rulename => |name| try writer.writeAll(name),
        .capture => |inner| {
            try writer.writeByte('(');
            try formatNode(inner.*, .top, writer);
            try writer.writeByte(')');
        },
        // Nodes not produced by the ERE parser.
        .num_val, .prose_val, .and_predicate, .not_predicate => unreachable,
    }
}

fn formatQuantifier(rep: Ast.Repetition, writer: anytype) !void {
    if (rep.min == 0 and rep.max == null) {
        try writer.writeByte('*');
    } else if (rep.min == 1 and rep.max == null) {
        try writer.writeByte('+');
    } else if (rep.min == 0 and rep.max != null and rep.max.? == 1) {
        try writer.writeByte('?');
    } else if (rep.max == null) {
        try writer.print("{{{d},}}", .{rep.min});
    } else if (rep.min == rep.max.?) {
        try writer.print("{{{d}}}", .{rep.min});
    } else {
        try writer.print("{{{d},{d}}}", .{ rep.min, rep.max.? });
    }
}

fn formatCharClass(ranges: []const Ast.ClassRange, negated: bool, writer: anytype) !void {
    try writer.writeByte('[');
    if (negated) try writer.writeByte('^');
    for (ranges) |r| {
        try writeClassChar(r.lo, writer);
        if (r.hi != r.lo) {
            try writer.writeByte('-');
            try writeClassChar(r.hi, writer);
        }
    }
    try writer.writeByte(']');
}

fn writeClassChar(c: u8, writer: anytype) !void {
    switch (c) {
        '\\' => try writer.writeAll("\\\\"),
        ']' => try writer.writeAll("\\]"),
        '[' => try writer.writeAll("\\["),
        '^' => try writer.writeAll("\\^"),
        '-' => try writer.writeAll("\\-"),
        '\n' => try writer.writeAll("\\n"),
        '\r' => try writer.writeAll("\\r"),
        '\t' => try writer.writeAll("\\t"),
        else => {
            if (c >= 0x20 and c <= 0x7E) {
                try writer.writeByte(c);
            } else {
                try writer.print("\\x{X:0>2}", .{c});
            }
        },
    }
}

fn isMetachar(c: u8) bool {
    return switch (c) {
        '.', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|', '^', '$', '\\' => true,
        else => false,
    };
}

// ── Tests ───────────────────────────────────────────────────────────

fn format(node: Ast.Node) ![]const u8 {
    var buf: [4096]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try formatRule(.{ .name = "", .node = node, .incremental = false }, fbs.writer());
    const written = fbs.getWritten();
    // Copy to test allocator so we can return it.
    const copy = try std.testing.allocator.alloc(u8, written.len);
    @memcpy(copy, written);
    return copy;
}

test "char_val escapes metacharacters" {
    const result = try format(.{ .char_val = .{ .value = "a.b", .case_sensitive = true } });
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a\\.b", result);
}

test "negated char class" {
    const ranges = &[_]Ast.ClassRange{.{ .lo = '0', .hi = '9' }};
    const result = try format(.{ .neg_char_class = ranges });
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("[^0-9]", result);
}

test "alternation inside concatenation gets grouped" {
    // Construct: concatenation(alternation(a, b), c)
    // Should produce: (a|b)c
    var nodes: [3]Ast.Node = .{
        .{ .char_val = .{ .value = "a", .case_sensitive = true } },
        .{ .char_val = .{ .value = "b", .case_sensitive = true } },
        .{ .char_val = .{ .value = "c", .case_sensitive = true } },
    };
    var alt_node: Ast.Node = .{ .alternation = nodes[0..2] };
    var cat_elems: [2]Ast.Node = .{ alt_node, nodes[2] };
    _ = &alt_node;
    _ = &cat_elems;
    const result = try format(.{ .concatenation = &cat_elems });
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("(a|b)c", result);
}

test "alternation quantified gets grouped" {
    // Construct: repetition(alternation(a, b), min=1, max=null)
    // Should produce: (a|b)+
    var nodes: [2]Ast.Node = .{
        .{ .char_val = .{ .value = "a", .case_sensitive = true } },
        .{ .char_val = .{ .value = "b", .case_sensitive = true } },
    };
    var alt_node: Ast.Node = .{ .alternation = &nodes };
    _ = &alt_node;
    const result = try format(.{ .repetition = .{
        .min = 1,
        .max = null,
        .element = &alt_node,
    } });
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("(a|b)+", result);
}

test "interval formatting" {
    var atom: Ast.Node = .{ .char_val = .{ .value = "a", .case_sensitive = true } };
    _ = &atom;

    // {3}
    const exact = try format(.{ .repetition = .{ .min = 3, .max = 3, .element = &atom } });
    defer std.testing.allocator.free(exact);
    try std.testing.expectEqualStrings("a{3}", exact);

    // {3,}
    const min_only = try format(.{ .repetition = .{ .min = 3, .max = null, .element = &atom } });
    defer std.testing.allocator.free(min_only);
    try std.testing.expectEqualStrings("a{3,}", min_only);

    // {2,5}
    const bounded = try format(.{ .repetition = .{ .min = 2, .max = 5, .element = &atom } });
    defer std.testing.allocator.free(bounded);
    try std.testing.expectEqualStrings("a{2,5}", bounded);
}

test "anchors" {
    var nodes: [3]Ast.Node = .{
        .anchor_start,
        .{ .char_val = .{ .value = "x", .case_sensitive = true } },
        .anchor_end,
    };
    const result = try format(.{ .concatenation = &nodes });
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("^x$", result);
}

test "class with special chars escaped" {
    const ranges = &[_]Ast.ClassRange{
        .{ .lo = ']', .hi = ']' },
        .{ .lo = '\\', .hi = '\\' },
    };
    const result = try format(.{ .char_class = ranges });
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("[\\]\\\\]", result);
}
