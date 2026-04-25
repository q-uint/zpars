/// Shared capture-event state used by both VM and JIT paths.
///
/// A `save` instruction carries a slot id; even slots open a group, odd
/// slots close one. Each successful `save` appends an `Event` describing
/// the transition and returns the pre-append length so the caller can
/// truncate the log on backtrack (keeping it in lockstep with the
/// capture-slot undo).
///
/// Consolidating the append/truncate semantics here keeps VM and JIT from
/// drifting: the VM calls these functions directly, the JIT emits calls
/// to the C-ABI wrappers at the bottom of this file.
const std = @import("std");
const CaptureTree = @import("CaptureTree.zig");

pub const State = struct {
    list: std.ArrayListUnmanaged(CaptureTree.Event),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) State {
        return .{ .list = .empty, .allocator = allocator };
    }

    pub fn deinit(self: *State) void {
        self.list.deinit(self.allocator);
    }

    pub fn clear(self: *State) void {
        self.list.clearRetainingCapacity();
    }

    pub fn items(self: *const State) []const CaptureTree.Event {
        return self.list.items;
    }
};

/// Append the open/close event for a successful `save` at `pos` and
/// return the length the log had *before* the append. Callers stash the
/// returned value on their undo stack so `truncate` can restore it on
/// backtrack.
pub fn appendSave(state: *State, slot: u16, pos: u32) std.mem.Allocator.Error!u32 {
    const pre_len: u32 = @intCast(state.list.items.len);
    const marker: CaptureTree.Event.Marker = .{
        .group_id = slot >> 1,
        .pos = pos,
    };
    const ev: CaptureTree.Event = if (slot & 1 == 0)
        .{ .open = marker }
    else
        .{ .close = marker };
    try state.list.append(state.allocator, ev);
    return pre_len;
}

/// Truncate the event log to `new_len`, dropping any events appended
/// after the matching `save`. Called from the backtrack unwind.
pub fn truncate(state: *State, new_len: u32) void {
    state.list.shrinkRetainingCapacity(new_len);
}

/// Sentinel the JIT helpers return on OOM. Matches `Jit.null_cap` so the
/// JIT save epilogue can reuse the same compare for match-failure and
/// append-failure paths.
pub const oom_sentinel: u64 = std.math.maxInt(u64);

/// C-ABI wrapper for `appendSave`. Returns `oom_sentinel` on allocator
/// failure; the JIT jumps to the fail handler on that value.
pub fn helperAppendSave(
    state_ptr: *State,
    slot: u64,
    pos: u64,
) callconv(.c) u64 {
    const pre_len = appendSave(state_ptr, @intCast(slot), @intCast(pos)) catch return oom_sentinel;
    return pre_len;
}

/// C-ABI wrapper for `truncate`. Always succeeds.
pub fn helperTruncate(
    state_ptr: *State,
    new_len: u64,
) callconv(.c) void {
    truncate(state_ptr, @intCast(new_len));
}

const testing = std.testing;

test "appendSave: alternating slots yield open/close pair" {
    var state = State.init(testing.allocator);
    defer state.deinit();

    const pre0 = try appendSave(&state, 0, 3);
    const pre1 = try appendSave(&state, 1, 7);
    try testing.expectEqual(@as(u32, 0), pre0);
    try testing.expectEqual(@as(u32, 1), pre1);

    const evs = state.items();
    try testing.expectEqual(@as(usize, 2), evs.len);
    try testing.expectEqual(@as(u16, 0), evs[0].open.group_id);
    try testing.expectEqual(@as(u32, 3), evs[0].open.pos);
    try testing.expectEqual(@as(u16, 0), evs[1].close.group_id);
    try testing.expectEqual(@as(u32, 7), evs[1].close.pos);
}

test "truncate: restores the length returned from an earlier append" {
    var state = State.init(testing.allocator);
    defer state.deinit();

    _ = try appendSave(&state, 0, 1);
    const pre = try appendSave(&state, 2, 2);
    _ = try appendSave(&state, 3, 3);
    truncate(&state, pre);

    try testing.expectEqual(@as(usize, 1), state.items().len);
}
