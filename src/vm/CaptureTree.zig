/// Capture tree built in a post-pass from the VM's open/close event log.
///
/// The VM (with `capture_events = true`) records one event per `save`
/// instruction it successfully executes, with events undone on backtrack.
/// Slot parity distinguishes opens from closes: even = open, odd = close.
///
/// `buildFromEvents` consumes the resulting event sequence and returns a
/// forest of matched capture groups. Repeated matches of the same group
/// (e.g. `(a)+`) yield sibling nodes; nested groups yield children.
///
/// The tagged-union `Event` leaves room for recovery-era event kinds
/// (error nodes, partial opens) to be added without breaking callers.
const std = @import("std");

pub const Event = union(enum) {
    open: Marker,
    close: Marker,

    pub const Marker = struct {
        group_id: u16,
        pos: u32,
    };
};

pub const Span = struct {
    start: u32,
    end: u32,
};

pub const Node = struct {
    group_id: u16,
    span: Span,
    children: []Node,
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
};

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
/// `MismatchedClose` if a close refers to a different group than the
/// innermost open.
pub fn buildFromEvents(allocator: std.mem.Allocator, events: []const Event) BuildError!Tree {
    const Frame = struct {
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
                .group_id = o.group_id,
                .start = o.pos,
                .children = .empty,
            });
        },
        .close => |c| {
            if (stack.items.len == 0) return error.UnbalancedEvents;
            var top = stack.pop().?;
            if (top.group_id != c.group_id) {
                freeNodes(allocator, top.children.items);
                top.children.deinit(allocator);
                return error.MismatchedClose;
            }
            const children = try top.children.toOwnedSlice(allocator);
            const node = Node{
                .group_id = top.group_id,
                .span = .{ .start = top.start, .end = c.pos },
                .children = children,
            };
            if (stack.items.len == 0) {
                try roots.append(allocator, node);
            } else {
                try stack.items[stack.items.len - 1].children.append(allocator, node);
            }
        },
    };

    if (stack.items.len != 0) return error.UnbalancedEvents;

    return .{
        .allocator = allocator,
        .roots = try roots.toOwnedSlice(allocator),
    };
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
