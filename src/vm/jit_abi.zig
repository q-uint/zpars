/// JIT runtime ABI shared between the JIT entry point and the C-ABI
/// helpers it calls into.
///
/// `Jit.zig` is the JIT entry point and references the architecture
/// backends (`JitAarch64.zig`, `JitX86.zig`); those backends in turn
/// import `Jit.zig` for `Config`, `JitCtx`, etc. The helper modules
/// (`events.zig`, `memo.zig`) need the same `StackEntry` / `MemoCtx`
/// layouts the JIT-emitted code reads and writes -- but they cannot
/// import `Jit.zig` without creating a cycle, since `Jit.zig` already
/// imports them. This module breaks the cycle by holding the layout
/// definitions both sides need to agree on.
const std = @import("std");

/// Maximum depth of the JIT's backtrack stack and (parallel) memo
/// side table. Must stay in sync with the array sizing on
/// `JitWith(...).stack_buf` / `memo_side` in `Jit.zig`.
pub const max_stack = 1024;

/// One backtrack-stack entry. The JIT writes these via inline assembly
/// at every `choice` / `call` / `save` / `event_*` / `lcatch` /
/// `memo_call` site, and reads them again from the backtrack handler
/// and the `ret` lowering. The C-ABI helpers in `memo.zig` also touch
/// them when replaying cached events.
pub const StackEntry = extern struct {
    /// Frame kind. Values: 0=choice, 1=ret, 2=save, 3=event,
    /// 4=lcatch, 5=memo. The backtrack handler dispatches on this; the
    /// `ret` lowering also consults it to distinguish a regular call
    /// return (tag=1) from a memoized rule completion (tag=5).
    tag: u64,
    /// First payload slot. Meaning depends on `tag`:
    ///   choice  -> saved input position
    ///   ret     -> native return address
    ///   save    -> capture slot id
    ///   event   -> unused
    ///   lcatch  -> label id
    ///   memo    -> side-table index (= the entry's own depth at push)
    val1: u64,
    /// Second payload slot. Meaning depends on `tag`:
    ///   choice  -> bytecode pc to resume on backtrack
    ///   save    -> previous value of capture slot
    ///   lcatch  -> handler bytecode pc
    ///   other   -> unused
    val2: u64,
    /// Snapshot of `events.len` captured at a successful `save` /
    /// `event_open` / `event_close` / `lcatch`, used by the backtrack
    /// handler to truncate the log in lockstep with the capture-slot
    /// undo (for save) or alone (for event/lcatch). Unused (zero)
    /// when capture_events is off, and unused for memo frames (the
    /// matching length is on the side-table `memo.Frame`).
    event_len: u64 = 0,
};

comptime {
    if (@sizeOf(StackEntry) != 32) @compileError("StackEntry must be 32 bytes");
}

/// Runtime data needed by memoization codegen. The JIT prologue loads
/// `JitCtx.memo_ctx_ptr` into a stack slot; memo emit sites then index
/// into this struct rather than carrying a dozen separate JitCtx
/// fields.
pub const MemoCtx = extern struct {
    /// Pointer to `[memo_rule_count * stride]memo.Entry`.
    table_ptr: u64,
    /// `input.len + 1`. Used to compute `idx = rule_id * stride + pos`.
    stride: u64,
    /// Pointer to `[max_stack]memo.Frame` indexed by stack depth.
    side_ptr: u64,
    /// `*memo.EventsBuf`. Zero when capture_events is off.
    events_buf_ptr: u64,
    helper_lookup: u64,
    helper_cached_slice: u64,
    helper_replay_events: u64,
    helper_ret_success: u64,
    helper_ret_fail: u64,
};
