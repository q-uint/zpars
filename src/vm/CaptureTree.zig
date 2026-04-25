/// Capture tree built in a post-pass from the VM's event log.
///
/// The VM (with `capture_events = true`) records events for each rule it
/// enters and exits, plus recovery-era events synthesized by the throw
/// unwinder when labeled failures escape an in-flight rule. Events are
/// undone on regular backtrack and preserved on labeled-failure unwind
/// (the `partial_close` variant fills in the dangling `open`s).
///
/// `buildFromEvents` consumes the resulting event sequence and returns a
/// forest of nodes. Repeated matches of the same group (e.g. `(a)+`)
/// yield siblings; nested groups yield children.
const std = @import("std");

pub const Event = union(enum) {
    /// Rule entered. `group_id` is a rule id.
    open: Marker,
    /// Rule completed normally. `group_id` is a rule id.
    close: Marker,
    /// Matching close synthesized by the throw unwinder for an `open`
    /// whose rule never reached its real `close`. The subtree under it
    /// is kept; the resulting node is tagged `.rule_partial`.
    partial_close: Marker,
    /// Start of a synthesized ERROR node. `group_id` is a label id, not
    /// a rule id.
    error_open: Marker,
    /// Matching end of an ERROR node. `group_id` is a label id.
    error_close: Marker,
    /// Zero-width "expected X here" marker. `group_id` is a label id;
    /// `pos` is the insertion point.
    missing: Marker,

    pub const Marker = struct {
        group_id: u16,
        pos: u32,
    };
};

pub const Span = struct {
    start: u32,
    end: u32,
};

pub const NodeKind = enum {
    rule,
    rule_partial,
    error_node,
    missing_node,
};

pub const Node = struct {
    kind: NodeKind,
    /// Rule id for `.rule`/`.rule_partial`; label id for the others.
    group_id: u16,
    span: Span,
    children: []Node,
};

/// Naming context passed to the renderers. `rules` resolves `group_id`
/// for `.rule`/`.rule_partial` nodes; `labels` resolves it for
/// `.error_node`/`.missing_node` nodes. Either may be empty - missing
/// names render as a hyphen.
pub const Names = struct {
    rules: []const []const u8 = &.{},
    labels: []const []const u8 = &.{},
};

/// Tree owns its nodes via the provided allocator. Call `deinit` to free.
pub const Tree = struct {
    allocator: std.mem.Allocator,
    roots: []Node,

    pub fn deinit(self: *Tree) void {
        freeNodes(self.allocator, self.roots);
        self.allocator.free(self.roots);
        self.roots = &.{};
    }

    /// Tree-sitter style S-expression. Nodes look like
    /// `(NodeType [start,end] children...)`. Closing parens stack at
    /// the end of the last line so the output diffs cleanly. Partial
    /// rule nodes get a trailing ` partial` marker; ERROR / MISSING
    /// nodes carry their label name after the span.
    pub fn writeSExp(self: *const Tree, writer: anytype, names: Names) !void {
        for (self.roots, 0..) |root, i| {
            if (i > 0) try writer.writeByte('\n');
            try writeSExpNode(root, writer, names, 0);
        }
    }

    /// Compact single-line JSON: `{"type":"X","span":[s,e],"children":[...]}`.
    /// Children key is omitted when the node is a leaf. Partial rule
    /// nodes carry `"partial":true`. ERROR / MISSING nodes use a fixed
    /// `"type"` and a separate `"label"` field.
    pub fn writeJson(self: *const Tree, writer: anytype, names: Names) !void {
        try writer.writeByte('[');
        for (self.roots, 0..) |root, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonNode(root, writer, names);
        }
        try writer.writeByte(']');
    }
};

fn ruleName(names: Names, group_id: u16) []const u8 {
    return if (group_id < names.rules.len) names.rules[group_id] else "-";
}

fn labelName(names: Names, group_id: u16) []const u8 {
    return if (group_id < names.labels.len) names.labels[group_id] else "-";
}

fn writeSExpNode(node: Node, writer: anytype, names: Names, depth: usize) !void {
    try writer.splatByteAll(' ', depth);
    switch (node.kind) {
        .rule => try writer.print("({s} [{d},{d}]", .{
            ruleName(names, node.group_id), node.span.start, node.span.end,
        }),
        .rule_partial => try writer.print("({s} [{d},{d}] partial", .{
            ruleName(names, node.group_id), node.span.start, node.span.end,
        }),
        .error_node => try writer.print("(ERROR [{d},{d}] {s}", .{
            node.span.start, node.span.end, labelName(names, node.group_id),
        }),
        .missing_node => try writer.print("(MISSING [{d},{d}] {s}", .{
            node.span.start, node.span.end, labelName(names, node.group_id),
        }),
    }
    for (node.children) |child| {
        try writer.writeByte('\n');
        try writeSExpNode(child, writer, names, depth + 2);
    }
    try writer.writeByte(')');
}

fn writeJsonNode(node: Node, writer: anytype, names: Names) !void {
    switch (node.kind) {
        .rule => try writer.print("{{\"type\":\"{s}\",\"span\":[{d},{d}]", .{
            ruleName(names, node.group_id), node.span.start, node.span.end,
        }),
        .rule_partial => try writer.print("{{\"type\":\"{s}\",\"span\":[{d},{d}],\"partial\":true", .{
            ruleName(names, node.group_id), node.span.start, node.span.end,
        }),
        .error_node => try writer.print("{{\"type\":\"ERROR\",\"label\":\"{s}\",\"span\":[{d},{d}]", .{
            labelName(names, node.group_id), node.span.start, node.span.end,
        }),
        .missing_node => try writer.print("{{\"type\":\"MISSING\",\"label\":\"{s}\",\"span\":[{d},{d}]", .{
            labelName(names, node.group_id), node.span.start, node.span.end,
        }),
    }
    if (node.children.len > 0) {
        try writer.writeAll(",\"children\":[");
        for (node.children, 0..) |child, i| {
            if (i > 0) try writer.writeByte(',');
            try writeJsonNode(child, writer, names);
        }
        try writer.writeByte(']');
    }
    try writer.writeByte('}');
}

fn freeNodes(allocator: std.mem.Allocator, nodes: []Node) void {
    for (nodes) |*n| {
        freeNodes(allocator, n.children);
        allocator.free(n.children);
    }
}

pub const BuildError = error{
    UnbalancedEvents,
    MismatchedClose,
} || std.mem.Allocator.Error;

/// Replay `events` to produce a forest. Returns `UnbalancedEvents` if the
/// stream has an open without a matching close (or vice versa), and
/// `MismatchedClose` if a close refers to a different group or a
/// different open-kind than the innermost open (e.g. an `error_close`
/// landing on an `open` frame).
pub fn buildFromEvents(allocator: std.mem.Allocator, events: []const Event) BuildError!Tree {
    const Frame = struct {
        /// `.rule` for opens from `.open`; `.error_node` for opens from
        /// `.error_open`. The closing-event variant must agree with this.
        open_kind: NodeKind,
        group_id: u16,
        start: u32,
        children: std.ArrayListUnmanaged(Node),
    };
    var stack: std.ArrayListUnmanaged(Frame) = .empty;
    defer stack.deinit(allocator);
    var roots: std.ArrayListUnmanaged(Node) = .empty;
    errdefer {
        for (roots.items) |*n| {
            freeNodes(allocator, n.children);
            allocator.free(n.children);
        }
        roots.deinit(allocator);
        for (stack.items) |*f| {
            for (f.children.items) |*n| {
                freeNodes(allocator, n.children);
                allocator.free(n.children);
            }
            f.children.deinit(allocator);
        }
    }

    for (events) |ev| switch (ev) {
        .open => |o| {
            try stack.append(allocator, .{
                .open_kind = .rule,
                .group_id = o.group_id,
                .start = o.pos,
                .children = .empty,
            });
        },
        .error_open => |o| {
            try stack.append(allocator, .{
                .open_kind = .error_node,
                .group_id = o.group_id,
                .start = o.pos,
                .children = .empty,
            });
        },
        .close => |c| try popInto(&stack, &roots, allocator, c, .rule, .rule),
        .partial_close => |c| try popInto(&stack, &roots, allocator, c, .rule, .rule_partial),
        .error_close => |c| try popInto(&stack, &roots, allocator, c, .error_node, .error_node),
        .missing => |m| {
            const node = Node{
                .kind = .missing_node,
                .group_id = m.group_id,
                .span = .{ .start = m.pos, .end = m.pos },
                .children = &.{},
            };
            try appendNode(&stack, &roots, allocator, node);
        },
    };

    if (stack.items.len != 0) return error.UnbalancedEvents;

    return .{
        .allocator = allocator,
        .roots = try roots.toOwnedSlice(allocator),
    };
}

fn popInto(
    stack: anytype,
    roots: anytype,
    allocator: std.mem.Allocator,
    close: Event.Marker,
    expect_open_kind: NodeKind,
    produce_kind: NodeKind,
) BuildError!void {
    if (stack.items.len == 0) return error.UnbalancedEvents;
    var top = stack.pop().?;
    if (top.open_kind != expect_open_kind or top.group_id != close.group_id) {
        freeNodes(allocator, top.children.items);
        top.children.deinit(allocator);
        return error.MismatchedClose;
    }
    const children = try top.children.toOwnedSlice(allocator);
    const node = Node{
        .kind = produce_kind,
        .group_id = top.group_id,
        .span = .{ .start = top.start, .end = close.pos },
        .children = children,
    };
    try appendNode(stack, roots, allocator, node);
}

fn appendNode(
    stack: anytype,
    roots: anytype,
    allocator: std.mem.Allocator,
    node: Node,
) std.mem.Allocator.Error!void {
    if (stack.items.len == 0) {
        try roots.append(allocator, node);
    } else {
        try stack.items[stack.items.len - 1].children.append(allocator, node);
    }
}

const testing = std.testing;

test "buildFromEvents: empty stream yields empty forest" {
    var tree = try buildFromEvents(testing.allocator, &.{});
    defer tree.deinit();
    try testing.expectEqual(@as(usize, 0), tree.roots.len);
}

test "buildFromEvents: single group" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 1 } },
        .{ .close = .{ .group_id = 0, .pos = 3 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();
    try testing.expectEqual(@as(usize, 1), tree.roots.len);
    try testing.expectEqual(NodeKind.rule, tree.roots[0].kind);
    try testing.expectEqual(@as(u16, 0), tree.roots[0].group_id);
    try testing.expectEqual(Span{ .start = 1, .end = 3 }, tree.roots[0].span);
    try testing.expectEqual(@as(usize, 0), tree.roots[0].children.len);
}

test "buildFromEvents: nested groups" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .open = .{ .group_id = 1, .pos = 0 } },
        .{ .close = .{ .group_id = 1, .pos = 1 } },
        .{ .open = .{ .group_id = 2, .pos = 1 } },
        .{ .close = .{ .group_id = 2, .pos = 2 } },
        .{ .close = .{ .group_id = 0, .pos = 2 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();
    try testing.expectEqual(@as(usize, 1), tree.roots.len);
    const outer = tree.roots[0];
    try testing.expectEqual(@as(u16, 0), outer.group_id);
    try testing.expectEqual(@as(usize, 2), outer.children.len);
    try testing.expectEqual(@as(u16, 1), outer.children[0].group_id);
    try testing.expectEqual(@as(u16, 2), outer.children[1].group_id);
}

test "buildFromEvents: repeated group yields siblings" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .close = .{ .group_id = 0, .pos = 1 } },
        .{ .open = .{ .group_id = 0, .pos = 1 } },
        .{ .close = .{ .group_id = 0, .pos = 2 } },
        .{ .open = .{ .group_id = 0, .pos = 2 } },
        .{ .close = .{ .group_id = 0, .pos = 3 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();
    try testing.expectEqual(@as(usize, 3), tree.roots.len);
    try testing.expectEqual(Span{ .start = 0, .end = 1 }, tree.roots[0].span);
    try testing.expectEqual(Span{ .start = 1, .end = 2 }, tree.roots[1].span);
    try testing.expectEqual(Span{ .start = 2, .end = 3 }, tree.roots[2].span);
}

test "buildFromEvents: unbalanced open returns error" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
    };
    try testing.expectError(error.UnbalancedEvents, buildFromEvents(testing.allocator, &events));
}

test "buildFromEvents: mismatched close returns error" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .close = .{ .group_id = 1, .pos = 1 } },
    };
    try testing.expectError(error.MismatchedClose, buildFromEvents(testing.allocator, &events));
}

test "buildFromEvents: mismatched close frees completed nested children" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .open = .{ .group_id = 1, .pos = 0 } },
        .{ .close = .{ .group_id = 1, .pos = 1 } },
        .{ .close = .{ .group_id = 2, .pos = 2 } },
    };
    try testing.expectError(error.MismatchedClose, buildFromEvents(testing.allocator, &events));
}

test "buildFromEvents: unbalanced close returns error" {
    const events = [_]Event{
        .{ .close = .{ .group_id = 0, .pos = 1 } },
    };
    try testing.expectError(error.UnbalancedEvents, buildFromEvents(testing.allocator, &events));
}

test "buildFromEvents: partial_close produces rule_partial node" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .open = .{ .group_id = 1, .pos = 0 } },
        .{ .close = .{ .group_id = 1, .pos = 2 } },
        .{ .partial_close = .{ .group_id = 0, .pos = 2 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();
    try testing.expectEqual(@as(usize, 1), tree.roots.len);
    try testing.expectEqual(NodeKind.rule_partial, tree.roots[0].kind);
    try testing.expectEqual(Span{ .start = 0, .end = 2 }, tree.roots[0].span);
    try testing.expectEqual(@as(usize, 1), tree.roots[0].children.len);
    try testing.expectEqual(NodeKind.rule, tree.roots[0].children[0].kind);
}

test "buildFromEvents: error_open/error_close produces error_node" {
    const events = [_]Event{
        .{ .error_open = .{ .group_id = 5, .pos = 3 } },
        .{ .error_close = .{ .group_id = 5, .pos = 7 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();
    try testing.expectEqual(@as(usize, 1), tree.roots.len);
    try testing.expectEqual(NodeKind.error_node, tree.roots[0].kind);
    try testing.expectEqual(@as(u16, 5), tree.roots[0].group_id);
    try testing.expectEqual(Span{ .start = 3, .end = 7 }, tree.roots[0].span);
}

test "buildFromEvents: missing produces zero-width missing_node leaf" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .missing = .{ .group_id = 2, .pos = 4 } },
        .{ .close = .{ .group_id = 0, .pos = 4 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();
    try testing.expectEqual(@as(usize, 1), tree.roots.len);
    const outer = tree.roots[0];
    try testing.expectEqual(@as(usize, 1), outer.children.len);
    const m = outer.children[0];
    try testing.expectEqual(NodeKind.missing_node, m.kind);
    try testing.expectEqual(@as(u16, 2), m.group_id);
    try testing.expectEqual(Span{ .start = 4, .end = 4 }, m.span);
    try testing.expectEqual(@as(usize, 0), m.children.len);
}

test "buildFromEvents: error_close on rule open is mismatched" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .error_close = .{ .group_id = 0, .pos = 1 } },
    };
    try testing.expectError(error.MismatchedClose, buildFromEvents(testing.allocator, &events));
}

test "buildFromEvents: close on error_open is mismatched" {
    const events = [_]Event{
        .{ .error_open = .{ .group_id = 0, .pos = 0 } },
        .{ .close = .{ .group_id = 0, .pos = 1 } },
    };
    try testing.expectError(error.MismatchedClose, buildFromEvents(testing.allocator, &events));
}

test "buildFromEvents: throw past two opens (synthesized partial closes)" {
    // Simulates the runtime emitting partial_close events innermost-first
    // when a throw escapes past two enclosing rules.
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } }, // outer
        .{ .open = .{ .group_id = 1, .pos = 0 } }, // inner
        .{ .partial_close = .{ .group_id = 1, .pos = 3 } }, // synthesized
        .{ .partial_close = .{ .group_id = 0, .pos = 3 } }, // synthesized
        .{ .error_open = .{ .group_id = 0, .pos = 3 } },
        .{ .error_close = .{ .group_id = 0, .pos = 5 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();
    try testing.expectEqual(@as(usize, 2), tree.roots.len);
    try testing.expectEqual(NodeKind.rule_partial, tree.roots[0].kind);
    try testing.expectEqual(@as(usize, 1), tree.roots[0].children.len);
    try testing.expectEqual(NodeKind.rule_partial, tree.roots[0].children[0].kind);
    try testing.expectEqual(NodeKind.error_node, tree.roots[1].kind);
}

test "writeSExp: tree-sitter style with stacked closing parens" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .open = .{ .group_id = 1, .pos = 0 } },
        .{ .close = .{ .group_id = 1, .pos = 1 } },
        .{ .open = .{ .group_id = 1, .pos = 2 } },
        .{ .close = .{ .group_id = 1, .pos = 3 } },
        .{ .close = .{ .group_id = 0, .pos = 3 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();

    var buf: [256]u8 = undefined;
    var stream: std.Io.Writer = .fixed(&buf);
    try tree.writeSExp(&stream, .{ .rules = &.{ "Expr", "Term" } });
    try testing.expectEqualStrings(
        \\(Expr [0,3]
        \\  (Term [0,1])
        \\  (Term [2,3]))
    , stream.buffered());
}

test "writeJson: compact single-line representation" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .open = .{ .group_id = 1, .pos = 0 } },
        .{ .close = .{ .group_id = 1, .pos = 1 } },
        .{ .close = .{ .group_id = 0, .pos = 1 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();

    var buf: [256]u8 = undefined;
    var stream: std.Io.Writer = .fixed(&buf);
    try tree.writeJson(&stream, .{ .rules = &.{ "Outer", "Inner" } });
    try testing.expectEqualStrings(
        \\[{"type":"Outer","span":[0,1],"children":[{"type":"Inner","span":[0,1]}]}]
    , stream.buffered());
}

test "writeSExp: out-of-range group_id renders as hyphen" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 7, .pos = 0 } },
        .{ .close = .{ .group_id = 7, .pos = 1 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();

    var buf: [64]u8 = undefined;
    var stream: std.Io.Writer = .fixed(&buf);
    try tree.writeSExp(&stream, .{});
    try testing.expectEqualStrings("(- [0,1])", stream.buffered());
}

test "writeSExp: rule_partial renders with partial marker" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .partial_close = .{ .group_id = 0, .pos = 3 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();

    var buf: [64]u8 = undefined;
    var stream: std.Io.Writer = .fixed(&buf);
    try tree.writeSExp(&stream, .{ .rules = &.{"Stmt"} });
    try testing.expectEqualStrings("(Stmt [0,3] partial)", stream.buffered());
}

test "writeSExp: error_node renders with label name" {
    const events = [_]Event{
        .{ .error_open = .{ .group_id = 1, .pos = 2 } },
        .{ .error_close = .{ .group_id = 1, .pos = 5 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();

    var buf: [64]u8 = undefined;
    var stream: std.Io.Writer = .fixed(&buf);
    try tree.writeSExp(&stream, .{ .labels = &.{ "stmt_garbage", "expr_garbage" } });
    try testing.expectEqualStrings("(ERROR [2,5] expr_garbage)", stream.buffered());
}

test "writeSExp: missing_node renders zero-width with label" {
    const events = [_]Event{
        .{ .missing = .{ .group_id = 0, .pos = 4 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();

    var buf: [64]u8 = undefined;
    var stream: std.Io.Writer = .fixed(&buf);
    try tree.writeSExp(&stream, .{ .labels = &.{"missing_semi"} });
    try testing.expectEqualStrings("(MISSING [4,4] missing_semi)", stream.buffered());
}

test "writeJson: rule_partial carries partial:true" {
    const events = [_]Event{
        .{ .open = .{ .group_id = 0, .pos = 0 } },
        .{ .partial_close = .{ .group_id = 0, .pos = 2 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();

    var buf: [128]u8 = undefined;
    var stream: std.Io.Writer = .fixed(&buf);
    try tree.writeJson(&stream, .{ .rules = &.{"Stmt"} });
    try testing.expectEqualStrings(
        \\[{"type":"Stmt","span":[0,2],"partial":true}]
    , stream.buffered());
}

test "writeJson: error_node uses ERROR type with label field" {
    const events = [_]Event{
        .{ .error_open = .{ .group_id = 0, .pos = 1 } },
        .{ .error_close = .{ .group_id = 0, .pos = 4 } },
    };
    var tree = try buildFromEvents(testing.allocator, &events);
    defer tree.deinit();

    var buf: [128]u8 = undefined;
    var stream: std.Io.Writer = .fixed(&buf);
    try tree.writeJson(&stream, .{ .labels = &.{"stmt_garbage"} });
    try testing.expectEqualStrings(
        \\[{"type":"ERROR","label":"stmt_garbage","span":[1,4]}]
    , stream.buffered());
}
