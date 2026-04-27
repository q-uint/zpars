/// AArch64 backend for the JIT compiler.
///
/// Encodes native AArch64 machine code from bytecode instructions.
const std = @import("std");
const I = @import("Instruction.zig");
const Jit = @import("Jit.zig");

const page_size = Jit.page_size;

// x19 = pos        (current input position)
// x20 = sp         (backtrack stack index)
// x21 = input_ptr  (base pointer to input bytes)
// x22 = input_len
// x23 = charsets_ptr
// x24 = string_data_ptr
// x25 = captures_ptr  (pointer to captures_buf u64 array)
// x26 = stack_ptr     (pointer to backtrack StackEntry array)
// x27 = jump_table_ptr
// x28 = code_base_ptr (start of native code, for indirect jumps)
//
// Stack-stored (no callee-saved regs left):
//   [sp+0]  = helper_string_match
//   [sp+8]  = helper_charset_match

const Reg = u5;

const pos: Reg = 19;
const bsp: Reg = 20; // backtrack stack pointer (index)
const inp: Reg = 21;
const inl: Reg = 22;
const csp: Reg = 23; // charsets ptr
const sdp: Reg = 24; // string data ptr
const cap: Reg = 25;
const skp: Reg = 26; // stack ptr
const jtp: Reg = 27; // jump table ptr
const cbp: Reg = 28; // code base ptr

// Scratch registers (caller-saved, clobbered by BLR calls)
const t0: Reg = 9;
const t1: Reg = 10;
const t2: Reg = 11;
const t3: Reg = 12;
const t4: Reg = 13;

const xzr: Reg = 31;
const sp_reg: Reg = 31; // SP in addressing context

// Condition codes for B.cond
const CC = struct {
    const eq: u4 = 0x0; // equal
    const ne: u4 = 0x1; // not equal
    const hs: u4 = 0x2; // unsigned >=
    const lo: u4 = 0x3; // unsigned <
};

fn encB(off: i28) u32 {
    const imm: u26 = @truncate(asU32(off >> 2));
    return (0b000101 << 26) | @as(u32, imm);
}

fn encBCond(cond: u4, off: i21) u32 {
    const imm: u19 = @truncate(asU32(@as(i32, off) >> 2));
    return (0b01010100 << 24) | (@as(u32, imm) << 5) | cond;
}

fn encCbz(rt: Reg, off: i21) u32 {
    const imm: u19 = @truncate(asU32(@as(i32, off) >> 2));
    return 0xB4000000 | (@as(u32, imm) << 5) | rt;
}

fn encCbnz(rt: Reg, off: i21) u32 {
    const imm: u19 = @truncate(asU32(@as(i32, off) >> 2));
    return 0xB5000000 | (@as(u32, imm) << 5) | rt;
}

fn encMovz(rd: Reg, imm16: u16, hw: u2) u32 {
    return 0xD2800000 | (@as(u32, hw) << 21) | (@as(u32, imm16) << 5) | rd;
}

fn encMovk(rd: Reg, imm16: u16, hw: u2) u32 {
    return 0xF2800000 | (@as(u32, hw) << 21) | (@as(u32, imm16) << 5) | rd;
}

fn encMovn(rd: Reg, imm16: u16) u32 {
    return 0x92800000 | (@as(u32, imm16) << 5) | rd;
}

fn encMov(rd: Reg, rm: Reg) u32 {
    return 0xAA0003E0 | (@as(u32, rm) << 16) | rd;
}

fn encAdd(rd: Reg, rn: Reg, imm12: u12) u32 {
    return 0x91000000 | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5) | rd;
}

fn encSub(rd: Reg, rn: Reg, imm12: u12) u32 {
    return 0xD1000000 | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5) | rd;
}

fn encCmpImm(rn: Reg, imm12: u12) u32 {
    return 0xF100001F | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5);
}

fn encCmpImm32(rn: Reg, imm12: u12) u32 {
    return 0x7100001F | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5);
}

fn encCmpReg(rn: Reg, rm: Reg) u32 {
    return 0xEB00001F | (@as(u32, rm) << 16) | (@as(u32, rn) << 5);
}

fn encLdrbReg(rt: Reg, rn: Reg, rm: Reg) u32 {
    return 0x38606800 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rt;
}

fn encLdr(rt: Reg, rn: Reg, imm_bytes: u15) u32 {
    return 0xF9400000 | (@as(u32, imm_bytes / 8) << 10) | (@as(u32, rn) << 5) | rt;
}

fn encStr(rt: Reg, rn: Reg, imm_bytes: u15) u32 {
    return 0xF9000000 | (@as(u32, imm_bytes / 8) << 10) | (@as(u32, rn) << 5) | rt;
}

fn encLdrReg(rt: Reg, rn: Reg, rm: Reg) u32 {
    return 0xF8607800 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rt;
}

fn encStrReg(rt: Reg, rn: Reg, rm: Reg) u32 {
    return 0xF8207800 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rt;
}

fn encStpPre(rt1: Reg, rt2: Reg, rn: Reg, imm: i9) u32 {
    const imm7: u7 = @truncate(asU32(@divTrunc(imm, 8)));
    return 0xA9800000 | (@as(u32, imm7) << 15) | (@as(u32, rt2) << 10) |
        (@as(u32, rn) << 5) | rt1;
}

fn encLdpPost(rt1: Reg, rt2: Reg, rn: Reg, imm: i9) u32 {
    const imm7: u7 = @truncate(asU32(@divTrunc(imm, 8)));
    return 0xA8C00000 | (@as(u32, imm7) << 15) | (@as(u32, rt2) << 10) |
        (@as(u32, rn) << 5) | rt1;
}

fn encAdr(rd: Reg, off: i21) u32 {
    const v: u21 = @bitCast(off);
    const immlo: u2 = @truncate(v);
    const immhi: u19 = @truncate(v >> 2);
    return (@as(u32, immlo) << 29) | 0x10000000 | (@as(u32, immhi) << 5) | rd;
}

fn encLsl(rd: Reg, rn: Reg, amount: u6) u32 {
    const immr: u6 = 0 -% amount;
    const imms: u6 = 63 - amount;
    return 0xD3400000 | (@as(u32, immr) << 16) | (@as(u32, imms) << 10) |
        (@as(u32, rn) << 5) | rd;
}

fn encBr(rn: Reg) u32 {
    return 0xD61F0000 | (@as(u32, rn) << 5);
}

fn encBlr(rn: Reg) u32 {
    return 0xD63F0000 | (@as(u32, rn) << 5);
}

fn encRet() u32 {
    return 0xD65F03C0;
}

fn encNop() u32 {
    return 0xD503201F;
}

fn encAddReg(rd: Reg, rn: Reg, rm: Reg) u32 {
    return 0x8B000000 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rd;
}

/// `MUL Xd, Xn, Xm` (alias of `MADD Xd, Xn, Xm, XZR`).
fn encMul(rd: Reg, rn: Reg, rm: Reg) u32 {
    return 0x9B007C00 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rd;
}

fn emitImm64(buf: *Buf, rd: Reg, val: u64) void {
    buf.emit(encMovz(rd, @truncate(val), 0));
    if (val > 0xFFFF)
        buf.emit(encMovk(rd, @truncate(val >> 16), 1));
    if (val > 0xFFFF_FFFF)
        buf.emit(encMovk(rd, @truncate(val >> 32), 2));
    if (val > 0xFFFF_FFFF_FFFF)
        buf.emit(encMovk(rd, @truncate(val >> 48), 3));
}

fn asU32(v: anytype) u32 {
    return @bitCast(@as(i32, @intCast(v)));
}

const Buf = struct {
    ptr: [*]u8,
    len: usize,

    fn emit(self: *Buf, inst: u32) void {
        const bytes = std.mem.asBytes(&inst);
        @memcpy(self.ptr[self.len..][0..4], bytes);
        self.len += 4;
    }

    fn off(self: *const Buf) u32 {
        return @intCast(self.len);
    }

    fn patchAt(self: *Buf, offset: u32, inst: u32) void {
        const bytes = std.mem.asBytes(&inst);
        @memcpy(self.ptr[offset..][0..4], bytes);
    }
};

const FixupKind = enum { b, b_cond, cbz, cbnz };

const FixupTarget = Jit.FixupTarget;

const Fixup = struct {
    code_off: u32,
    target: FixupTarget,
    kind: FixupKind,
    cond: u4,
    reg: Reg,
};

fn addFixup(
    fixups: *[8192]Fixup,
    count: *usize,
    code_off: u32,
    target: FixupTarget,
    kind: FixupKind,
    cond: u4,
    reg: Reg,
) void {
    fixups[count.*] = .{
        .code_off = code_off,
        .target = target,
        .kind = kind,
        .cond = cond,
        .reg = reg,
    };
    count.* += 1;
}

pub const GenerateResult = struct {
    native_len: usize,
    jump_table: [4096]u64,
};

pub fn estimateSize(comptime config: Jit.Config, code_len: usize) usize {
    // memo_call is the largest opcode (~70-90 instructions including
    // the success-path replay setup); bump the budget when it can
    // appear so we don't underprovision the mmap.
    const per_inst: usize = if (config.memoize)
        320
    else if (config.capture_events) 128 else 80;
    return (code_len + 1) * per_inst + 2048;
}

pub fn generate(
    comptime config: Jit.Config,
    code: []const I.Inst,
    output: [*]u8,
) GenerateResult {
    var buf = Buf{ .ptr = output, .len = 0 };
    var fixups: [8192]Fixup = undefined;
    var fcount: usize = 0;
    var bc_map: [4096]u32 = undefined;

    emitPrologue(config, &buf);

    for (code, 0..) |inst, i| {
        bc_map[i] = buf.off();
        emitInst(config, &buf, inst, @intCast(i), &fixups, &fcount);
    }
    if (code.len < 4096)
        bc_map[code.len] = buf.off();

    const bt_off = buf.off();
    emitBacktrackHandler(config, &buf, &fixups, &fcount);

    const fail_off = buf.off();
    buf.emit(encMovn(0, 0)); // MOV x0, #-1 (all ones)

    const succ_off = buf.off();
    emitEpilogue(config, &buf);

    for (fixups[0..fcount]) |f| {
        const tgt_off: u32 = switch (f.target) {
            .backtrack => bt_off,
            .fail => fail_off,
            .success => succ_off,
            _ => bc_map[@intFromEnum(f.target)],
        };
        const rel: i32 = @as(i32, @intCast(tgt_off)) - @as(i32, @intCast(f.code_off));
        const inst: u32 = switch (f.kind) {
            .b => encB(@intCast(rel)),
            .b_cond => encBCond(f.cond, @intCast(rel)),
            .cbz => encCbz(f.reg, @intCast(rel)),
            .cbnz => encCbnz(f.reg, @intCast(rel)),
        };
        buf.patchAt(f.code_off, inst);
    }

    var result = GenerateResult{
        .native_len = buf.len,
        .jump_table = [_]u64{0} ** 4096,
    };
    for (0..code.len) |i| {
        result.jump_table[i] = bc_map[i];
    }
    return result;
}

pub fn compile(self: anytype) !void {
    const config = @TypeOf(self.*).jit_config;
    // Recovery opcodes need an event log to snapshot lcatch frames and
    // synthesize partial-close events. Reject grammars that use them
    // when capture_events is off, rather than miscompiling.
    if (!config.capture_events and I.requiresCaptureEvents(self.code))
        return error.JitDoesNotSupportOp;
    // `memo_call` only has a code path when memoize is on. Reject up
    // front rather than walking into an `unreachable` during emit.
    if (!config.memoize and I.containsMemoCall(self.code))
        return error.JitDoesNotSupportOp;
    const est = estimateSize(config, self.code.len);
    const size = std.mem.alignForward(usize, est, page_size);

    self.native_code = try std.posix.mmap(
        null,
        size,
        .{ .READ = true, .WRITE = true },
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    );

    const result = generate(config, self.code, self.native_code.ptr);
    self.native_len = result.native_len;
    self.jump_table = result.jump_table;

    try std.process.protectMemory(
        @alignCast(self.native_code[0..size]),
        .{ .read = true, .execute = true },
    );
}

/// Stack slots for helper pointers (offsets within the locals frame).
/// Mirrors `Jit.StackSlots` 1:1 via `@offsetOf` so adding or
/// reordering a helper slot updates both backends. The two configs are
/// independent -- memoize works without capture_events; the success
/// path just skips the event-replay branch.
fn spSlot(comptime field: []const u8) u15 {
    return @intCast(@offsetOf(Jit.StackSlots, field));
}

const sp_hsm: u15 = spSlot("helper_string_match");
const sp_hcm: u15 = spSlot("helper_charset_match");
const sp_esp: u15 = spSlot("events_state_ptr");
const sp_has: u15 = spSlot("helper_append_save");
const sp_hte: u15 = spSlot("helper_truncate_events");
const sp_hat: u15 = spSlot("helper_append_token");
const sp_haf: u15 = spSlot("helper_append_field");
const sp_haeo: u15 = spSlot("helper_append_error_open");
const sp_haec: u15 = spSlot("helper_append_error_close");
const sp_haem: u15 = spSlot("helper_append_missing");
const sp_ht: u15 = spSlot("helper_throw");
const sp_hel: u15 = spSlot("helper_events_len");
const sp_call_scratch: u15 = spSlot("call_scratch");
const sp_call_scratch2: u15 = spSlot("call_scratch2");
const sp_memo_ctx: u15 = spSlot("memo_ctx");
const sp_memo_scratch1: u15 = spSlot("memo_scratch1");
const sp_memo_scratch2: u15 = spSlot("memo_scratch2");

fn localsSize(comptime config: Jit.Config) u12 {
    // Must be a multiple of 16 for AArch64 SP alignment. Capture-events
    // adds 11 helper slots (88 bytes) above the 2 base helper slots
    // (= 104 used, padded to 112). Memoize adds another `call_scratch2`
    // + 3 memo slots (= 144 used, padded to 144 since 144 mod 16 = 0).
    if (config.memoize) return 144;
    if (config.capture_events) return 112;
    return 16;
}

/// Comptime offset of a `Jit.JitCtx` field, narrowed to `u15` so it
/// fits the AArch64 LDR/STR immediate-offset encoding. Using
/// `@offsetOf` here keeps the prologue in lock-step with `JitCtx`'s
/// layout: reorder or insert a field and the codegen follows
/// automatically.
fn ctxOff(comptime field: []const u8) u15 {
    return @intCast(@offsetOf(Jit.JitCtx, field));
}

/// Same idea for `Jit.MemoCtx`. Used by the `memo_call` / `ret` /
/// backtrack lowerings when they index into the runtime memo bundle.
fn memoOff(comptime field: []const u8) u15 {
    return @intCast(@offsetOf(Jit.MemoCtx, field));
}

fn emitPrologue(comptime config: Jit.Config, buf: *Buf) void {
    buf.emit(encStpPre(29, 30, sp_reg, -16));
    buf.emit(encStpPre(pos, bsp, sp_reg, -16));
    buf.emit(encStpPre(inp, inl, sp_reg, -16));
    buf.emit(encStpPre(csp, sdp, sp_reg, -16));
    buf.emit(encStpPre(cap, skp, sp_reg, -16));
    buf.emit(encStpPre(jtp, cbp, sp_reg, -16));
    buf.emit(encSub(sp_reg, sp_reg, localsSize(config)));

    // x0 = pointer to JitCtx. Load fields into callee-saved regs.
    buf.emit(encLdr(inp, 0, ctxOff("input_ptr")));
    buf.emit(encLdr(inl, 0, ctxOff("input_len")));
    buf.emit(encLdr(csp, 0, ctxOff("charsets_ptr")));
    buf.emit(encLdr(sdp, 0, ctxOff("string_data_ptr")));
    buf.emit(encLdr(cap, 0, ctxOff("captures_ptr")));
    buf.emit(encLdr(skp, 0, ctxOff("stack_ptr")));
    buf.emit(encLdr(jtp, 0, ctxOff("jump_table_ptr")));
    buf.emit(encLdr(cbp, 0, ctxOff("code_base_ptr")));
    // Store helper function pointers on the stack.
    buf.emit(encLdr(t0, 0, ctxOff("helper_string_match")));
    buf.emit(encStr(t0, sp_reg, sp_hsm));
    buf.emit(encLdr(t0, 0, ctxOff("helper_charset_match")));
    buf.emit(encStr(t0, sp_reg, sp_hcm));
    if (config.capture_events) {
        buf.emit(encLdr(t0, 0, ctxOff("events_state_ptr")));
        buf.emit(encStr(t0, sp_reg, sp_esp));
        buf.emit(encLdr(t0, 0, ctxOff("helper_append_save")));
        buf.emit(encStr(t0, sp_reg, sp_has));
        buf.emit(encLdr(t0, 0, ctxOff("helper_truncate_events")));
        buf.emit(encStr(t0, sp_reg, sp_hte));
        buf.emit(encLdr(t0, 0, ctxOff("helper_append_token")));
        buf.emit(encStr(t0, sp_reg, sp_hat));
        buf.emit(encLdr(t0, 0, ctxOff("helper_append_field")));
        buf.emit(encStr(t0, sp_reg, sp_haf));
        buf.emit(encLdr(t0, 0, ctxOff("helper_append_error_open")));
        buf.emit(encStr(t0, sp_reg, sp_haeo));
        buf.emit(encLdr(t0, 0, ctxOff("helper_append_error_close")));
        buf.emit(encStr(t0, sp_reg, sp_haec));
        buf.emit(encLdr(t0, 0, ctxOff("helper_append_missing")));
        buf.emit(encStr(t0, sp_reg, sp_haem));
        buf.emit(encLdr(t0, 0, ctxOff("helper_throw")));
        buf.emit(encStr(t0, sp_reg, sp_ht));
        buf.emit(encLdr(t0, 0, ctxOff("helper_events_len")));
        buf.emit(encStr(t0, sp_reg, sp_hel));
    }
    if (config.memoize) {
        buf.emit(encLdr(t0, 0, ctxOff("memo_ctx_ptr")));
        buf.emit(encStr(t0, sp_reg, sp_memo_ctx));
    }

    buf.emit(encMovz(pos, 0, 0));
    buf.emit(encMovz(bsp, 0, 0));
}

fn emitEpilogue(comptime config: Jit.Config, buf: *Buf) void {
    buf.emit(encAdd(sp_reg, sp_reg, localsSize(config)));
    buf.emit(encLdpPost(jtp, cbp, sp_reg, 16));
    buf.emit(encLdpPost(cap, skp, sp_reg, 16));
    buf.emit(encLdpPost(csp, sdp, sp_reg, 16));
    buf.emit(encLdpPost(inp, inl, sp_reg, 16));
    buf.emit(encLdpPost(pos, bsp, sp_reg, 16));
    buf.emit(encLdpPost(29, 30, sp_reg, 16));
    buf.emit(encRet());
}

fn emitBacktrackHandler(
    comptime config: Jit.Config,
    buf: *Buf,
    fixups: *[8192]Fixup,
    fcount: *usize,
) void {
    addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, bsp);
    buf.emit(encNop());

    const loop_off = buf.off();

    buf.emit(encSub(bsp, bsp, 1));
    buf.emit(encLsl(t1, bsp, 5));
    buf.emit(encAddReg(t1, skp, t1));

    buf.emit(encLdr(t2, t1, 0)); // tag

    const choice_off = buf.off();
    buf.emit(encCbz(t2, 0));

    buf.emit(encCmpImm(t2, 2));
    const save_off = buf.off();
    buf.emit(encBCond(CC.eq, 0));

    // Memo frames (tag=5) are dispatched before the event check below
    // so the `b.hs` event check stays correct -- it would otherwise
    // catch tag=5 too.
    const memo_off = if (config.memoize) blk: {
        buf.emit(encCmpImm(t2, 5));
        const off = buf.off();
        buf.emit(encBCond(CC.eq, 0));
        break :blk off;
    } else 0;

    const event_off = if (config.capture_events) blk: {
        // Tags 3 (event) and 4 (lcatch) both go to event_handler:
        // both truncate the event log to the snapshot in slot 24 and
        // do not restore any capture slot. `hs` (unsigned >=) catches
        // both. tag 5 was dispatched above when memoize is on.
        buf.emit(encCmpImm(t2, 3));
        const off = buf.off();
        buf.emit(encBCond(CC.hs, 0));
        break :blk off;
    } else 0;

    // tag == 1 (ret): skip, continue loop
    addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, bsp);
    buf.emit(encNop());
    buf.emit(encB(@intCast(@as(i32, @intCast(loop_off)) - @as(i32, @intCast(buf.off())))));

    // save handler
    const save_handler = buf.off();
    if (config.capture_events) {
        // Truncate events to the snapshot taken at save time, then
        // rebuild t1 since BLR clobbers x9-x18.
        buf.emit(encLdr(1, t1, 24)); // x1 = event_len
        buf.emit(encLdr(0, sp_reg, sp_esp)); // x0 = state_ptr
        buf.emit(encLdr(t0, sp_reg, sp_hte));
        buf.emit(encBlr(t0));
        buf.emit(encLsl(t1, bsp, 5));
        buf.emit(encAddReg(t1, skp, t1));
    }
    buf.emit(encLdr(t3, t1, 8));
    buf.emit(encLdr(t4, t1, 16));
    buf.emit(encStrReg(t4, cap, t3));
    addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, bsp);
    buf.emit(encNop());
    buf.emit(encB(@intCast(@as(i32, @intCast(loop_off)) - @as(i32, @intCast(buf.off())))));

    // event handler (capture_events only): truncate events, no slot restore.
    const event_handler = if (config.capture_events) blk: {
        const off = buf.off();
        buf.emit(encLdr(1, t1, 24));
        buf.emit(encLdr(0, sp_reg, sp_esp));
        buf.emit(encLdr(t0, sp_reg, sp_hte));
        buf.emit(encBlr(t0));
        addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, bsp);
        buf.emit(encNop());
        buf.emit(encB(@intCast(@as(i32, @intCast(loop_off)) - @as(i32, @intCast(buf.off())))));
        break :blk off;
    } else 0;

    // memo handler (memoize only): call helperMemoBacktrack.
    // Returns either `bt_continue` (just keep walking the stack) or
    // `bt_redirect` (recall failure with cached success / grow
    // failure with seed -- branch to `out.native_target` with `pos =
    // out.new_pos`). The 16-byte `RetResult` is laid out across the
    // adjacent `memo_scratch1` / `memo_scratch2` slots.
    const memo_handler = if (config.memoize) blk: {
        const off = buf.off();
        buf.emit(encLdr(1, t1, 8)); // x1 = side_idx (val1)
        buf.emit(encLdr(0, sp_reg, sp_memo_ctx)); // x0 = memo_ctx
        buf.emit(encAdd(2, sp_reg, sp_memo_scratch1)); // x2 = &out_result
        buf.emit(encLdr(t0, 0, memoOff("helper_backtrack")));
        buf.emit(encBlr(t0));
        // x0 = action: 0=bt_continue, 1=bt_redirect.
        buf.emit(encCmpImm(0, 1));
        const redirect_off = buf.off();
        buf.emit(encBCond(CC.eq, 0)); // patched to redirect_handler
        // Continue loop (or fail if stack empty).
        addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, bsp);
        buf.emit(encNop());
        buf.emit(encB(@intCast(@as(i32, @intCast(loop_off)) - @as(i32, @intCast(buf.off())))));
        // Redirect: load native target + new pos, branch.
        const redirect_handler = buf.off();
        {
            const rel: i32 = @as(i32, @intCast(redirect_handler)) - @as(i32, @intCast(redirect_off));
            buf.patchAt(redirect_off, encBCond(CC.eq, @intCast(rel)));
        }
        buf.emit(encLdr(t0, sp_reg, sp_memo_scratch1));
        buf.emit(encLdr(pos, sp_reg, sp_memo_scratch2));
        buf.emit(encBr(t0));
        break :blk off;
    } else 0;

    // choice handler
    const choice_handler = buf.off();
    buf.emit(encLdr(pos, t1, 8));
    buf.emit(encLdr(t2, t1, 16));
    buf.emit(encLdrReg(t2, jtp, t2));
    buf.emit(encAddReg(t2, cbp, t2));
    buf.emit(encBr(t2));

    {
        const rel: i32 = @as(i32, @intCast(choice_handler)) - @as(i32, @intCast(choice_off));
        buf.patchAt(choice_off, encCbz(t2, @intCast(rel)));
    }
    {
        const rel: i32 = @as(i32, @intCast(save_handler)) - @as(i32, @intCast(save_off));
        buf.patchAt(save_off, encBCond(CC.eq, @intCast(rel)));
    }
    if (config.capture_events) {
        const rel: i32 = @as(i32, @intCast(event_handler)) - @as(i32, @intCast(event_off));
        buf.patchAt(event_off, encBCond(CC.eq, @intCast(rel)));
    }
    if (config.memoize) {
        const rel: i32 = @as(i32, @intCast(memo_handler)) - @as(i32, @intCast(memo_off));
        buf.patchAt(memo_off, encBCond(CC.eq, @intCast(rel)));
    }
}

/// Splice out the most recent .choice (tag=0) or .lcatch (tag=4) frame
/// without disturbing any .save / .event frames a rule call inside the
/// alt body pushed on top of it. Mirrors `Vm.spliceCtrlFrame`.
///
/// In capture_events mode a successful sub-call's `ret` shifts its body
/// event frames down past the squeezed call frame, so by the time
/// `commit` / `fail_twice` runs the choice frame is no longer at sp-1
/// -- a plain `bsp -= 1` would pop an event frame and leave the choice
/// stranded, causing a later backtrack to find the wrong frame and
/// resume at the wrong control point. Caller guarantees a matching
/// frame exists.
fn emitSpliceCtrlFrame(buf: *Buf) void {
    // t1 = bsp - 1 (start at sp-1)
    buf.emit(encSub(t1, bsp, 1));

    // find_loop: walk down until tag == 0 (choice) or tag == 4 (lcatch).
    const find_loop = buf.off();
    buf.emit(encLsl(t0, t1, 5));
    buf.emit(encAddReg(t0, skp, t0));
    buf.emit(encLdr(t2, t0, 0)); // t2 = tag

    const found_choice_off = buf.off();
    buf.emit(encCbz(t2, 0)); // tag == 0 → found
    buf.emit(encCmpImm(t2, 4));
    const found_lcatch_off = buf.off();
    buf.emit(encBCond(CC.eq, 0)); // tag == 4 → found

    buf.emit(encSub(t1, t1, 1));
    {
        const rel: i32 = @as(i32, @intCast(find_loop)) - @as(i32, @intCast(buf.off()));
        buf.emit(encB(@intCast(rel)));
    }

    // found: shift frames [t1+1..bsp) down by one slot.
    const found = buf.off();
    {
        const rel: i32 = @as(i32, @intCast(found)) - @as(i32, @intCast(found_choice_off));
        buf.patchAt(found_choice_off, encCbz(t2, @intCast(rel)));
    }
    {
        const rel: i32 = @as(i32, @intCast(found)) - @as(i32, @intCast(found_lcatch_off));
        buf.patchAt(found_lcatch_off, encBCond(CC.eq, @intCast(rel)));
    }

    const shift_loop = buf.off();
    buf.emit(encAdd(t2, t1, 1));
    buf.emit(encCmpReg(t2, bsp));
    const shift_done_off = buf.off();
    buf.emit(encBCond(CC.hs, 0));

    buf.emit(encLsl(t0, t1, 5));
    buf.emit(encAddReg(t0, skp, t0));

    buf.emit(encLdr(t4, t0, 32));
    buf.emit(encStr(t4, t0, 0));
    buf.emit(encLdr(t4, t0, 40));
    buf.emit(encStr(t4, t0, 8));
    buf.emit(encLdr(t4, t0, 48));
    buf.emit(encStr(t4, t0, 16));
    buf.emit(encLdr(t4, t0, 56));
    buf.emit(encStr(t4, t0, 24));

    buf.emit(encAdd(t1, t1, 1));
    {
        const rel: i32 = @as(i32, @intCast(shift_loop)) - @as(i32, @intCast(buf.off()));
        buf.emit(encB(@intCast(rel)));
    }

    const shift_done = buf.off();
    {
        const rel: i32 = @as(i32, @intCast(shift_done)) - @as(i32, @intCast(shift_done_off));
        buf.patchAt(shift_done_off, encBCond(CC.hs, @intCast(rel)));
    }

    buf.emit(encSub(bsp, bsp, 1));
}

fn emitCharsetCheck(buf: *Buf, charset: u16, negate: bool, fixups: *[8192]Fixup, fcount: *usize) void {
    buf.emit(encCmpReg(pos, inl));
    addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.hs, 0);
    buf.emit(encNop());
    buf.emit(encLdrbReg(t0, inp, pos));
    buf.emit(encMov(0, csp));
    buf.emit(encMovz(1, @intCast(charset), 0));
    buf.emit(encMov(2, t0));
    buf.emit(encLdr(t0, sp_reg, sp_hcm));
    buf.emit(encBlr(t0));
    addFixup(fixups, fcount, buf.off(), .backtrack, if (negate) .cbnz else .cbz, 0, 0);
    buf.emit(encNop());
    buf.emit(encAdd(pos, pos, 1));
}

fn emitInst(
    comptime config: Jit.Config,
    buf: *Buf,
    inst: I.Inst,
    bc_pc: u32,
    fixups: *[8192]Fixup,
    fcount: *usize,
) void {
    switch (inst.op) {
        .char => {
            buf.emit(encCmpReg(pos, inl));
            addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.hs, 0);
            buf.emit(encNop());
            buf.emit(encLdrbReg(t0, inp, pos));
            buf.emit(encCmpImm32(t0, @intCast(inst.data.byte)));
            addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.ne, 0);
            buf.emit(encNop());
            buf.emit(encAdd(pos, pos, 1));
        },
        .any => {
            buf.emit(encCmpReg(pos, inl));
            addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.hs, 0);
            buf.emit(encNop());
            buf.emit(encAdd(pos, pos, 1));
        },
        .optional_char => {
            buf.emit(encCmpReg(pos, inl));
            buf.emit(encBCond(CC.hs, 20));
            buf.emit(encLdrbReg(t0, inp, pos));
            buf.emit(encCmpImm32(t0, @intCast(inst.data.byte)));
            buf.emit(encBCond(CC.ne, 8));
            buf.emit(encAdd(pos, pos, 1));
        },
        .choice => {
            buf.emit(encLsl(t1, bsp, 5));
            buf.emit(encAddReg(t1, skp, t1));
            buf.emit(encStr(xzr, t1, 0));
            buf.emit(encStr(pos, t1, 8));
            buf.emit(encMovz(t2, @intCast(inst.data.offset), 0));
            buf.emit(encStr(t2, t1, 16));
            buf.emit(encAdd(bsp, bsp, 1));
        },
        .commit => {
            if (config.capture_events) {
                emitSpliceCtrlFrame(buf);
            } else {
                buf.emit(encSub(bsp, bsp, 1));
            }
            addFixup(fixups, fcount, buf.off(), FixupTarget.bytecodePC(inst.data.offset), .b, 0, 0);
            buf.emit(encNop());
        },
        .fail => {
            addFixup(fixups, fcount, buf.off(), .backtrack, .b, 0, 0);
            buf.emit(encNop());
        },
        .fail_twice => {
            if (config.capture_events) {
                emitSpliceCtrlFrame(buf);
            } else {
                buf.emit(encSub(bsp, bsp, 1));
            }
            addFixup(fixups, fcount, buf.off(), .backtrack, .b, 0, 0);
            buf.emit(encNop());
        },
        .jump => {
            addFixup(fixups, fcount, buf.off(), FixupTarget.bytecodePC(inst.data.offset), .b, 0, 0);
            buf.emit(encNop());
        },
        .call => {
            buf.emit(encLsl(t1, bsp, 5));
            buf.emit(encAddReg(t1, skp, t1));
            buf.emit(encMovz(t2, 1, 0));
            buf.emit(encStr(t2, t1, 0));
            buf.emit(encAdr(t2, 16));
            buf.emit(encStr(t2, t1, 8));
            buf.emit(encAdd(bsp, bsp, 1));
            addFixup(fixups, fcount, buf.off(), FixupTarget.bytecodePC(inst.data.offset), .b, 0, 0);
            buf.emit(encNop());
        },
        .memo_call => {
            // Mirrors `Vm`'s `memo_call` arm. The heavy lifting --
            // RECALL detection, table lookup, SETUP-LR, and (for
            // miss/recall) appending the new memo frame to `Side` --
            // all lives in `helperMemoCallBegin`; the JIT just
            // dispatches on its action code:
            //   0 miss     -> stamp tag=5 marker + side_idx, jump body
            //   1 fail     -> backtrack
            //   2 success  -> pos = end_pos, replay cached events
            //   3 lr_seed  -> pos = end_pos, fall through (no replay)
            //   4 recall   -> stamp tag=5 marker + side_idx, jump body
            //   5 cut      -> backtrack
            // Any other return value is the OOM sentinel; route to fail.
            const mc = inst.data.memo;

            // 1. helperMemoCallBegin(memo_ctx, bsp, rule_id, pos,
            //                        pc_pair, &end_pos_out, &side_idx_out)
            //    pc_pair packs return_pc | rule_entry_pc << 32.
            buf.emit(encLdr(0, sp_reg, sp_memo_ctx));
            buf.emit(encMov(1, bsp));
            emitImm64(buf, 2, mc.rule_id);
            buf.emit(encMov(3, pos));
            const pc_pair: u64 = @as(u64, bc_pc + 1) | (@as(u64, mc.offset) << 32);
            emitImm64(buf, 4, pc_pair);
            buf.emit(encAdd(5, sp_reg, sp_memo_scratch1));
            buf.emit(encAdd(6, sp_reg, sp_memo_scratch2));
            buf.emit(encLdr(t3, 0, memoOff("helper_call_begin")));
            buf.emit(encBlr(t3));
            // x0 = action code (or oom_sentinel).

            // OOM check: action_codes are 0..5; oom_sentinel is the
            // only larger return.
            buf.emit(encCmpImm(0, 5));
            const not_oom_off = buf.off();
            buf.emit(encBCond(CC.lo, 0)); // patched to land just past the fail branch
            addFixup(fixups, fcount, buf.off(), .fail, .b, 0, 0);
            buf.emit(encNop());
            const dispatch_off = buf.off();
            {
                const rel: i32 = @as(i32, @intCast(dispatch_off)) - @as(i32, @intCast(not_oom_off));
                buf.patchAt(not_oom_off, encBCond(CC.lo, @intCast(rel)));
            }

            // 2. Dispatch.
            buf.emit(encCmpImm(0, 1)); // call_fail
            addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.eq, 0);
            buf.emit(encNop());
            buf.emit(encCmpImm(0, 5)); // call_cut
            addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.eq, 0);
            buf.emit(encNop());
            buf.emit(encCmpImm(0, 2)); // call_success
            const success_branch_off = buf.off();
            buf.emit(encBCond(CC.eq, 0));
            buf.emit(encCmpImm(0, 3)); // call_lr_seed
            const lr_seed_branch_off = buf.off();
            buf.emit(encBCond(CC.eq, 0));
            // Fall through: action == 0 (miss) or 4 (recall). Both
            // require pushing a tag=5 marker with the helper-supplied
            // side_idx and jumping to the body.

            // Miss / recall: stamp marker + jump.
            // stack[bsp] = { tag=5, val1=*side_idx_out, val2=0, event_len=0 }
            buf.emit(encLsl(t1, bsp, 5));
            buf.emit(encAddReg(t1, skp, t1));
            buf.emit(encMovz(t2, 5, 0));
            buf.emit(encStr(t2, t1, 0));
            buf.emit(encLdr(t2, sp_reg, sp_memo_scratch2)); // side_idx
            buf.emit(encStr(t2, t1, 8));
            buf.emit(encStr(xzr, t1, 16));
            buf.emit(encStr(xzr, t1, 24));
            buf.emit(encAdd(bsp, bsp, 1));
            addFixup(fixups, fcount, buf.off(), FixupTarget.bytecodePC(mc.offset), .b, 0, 0);
            buf.emit(encNop());

            // LR-seed handler: just advance pos, no replay.
            const lr_seed_handler = buf.off();
            {
                const rel: i32 = @as(i32, @intCast(lr_seed_handler)) - @as(i32, @intCast(lr_seed_branch_off));
                buf.patchAt(lr_seed_branch_off, encBCond(CC.eq, @intCast(rel)));
            }
            buf.emit(encLdr(pos, sp_reg, sp_memo_scratch1));
            const lr_seed_skip_off = buf.off();
            buf.emit(encNop()); // patched to branch past the success handler

            // Success path: replay cached events, advance pos.
            const success_handler = buf.off();
            {
                const rel: i32 = @as(i32, @intCast(success_handler)) - @as(i32, @intCast(success_branch_off));
                buf.patchAt(success_branch_off, encBCond(CC.eq, @intCast(rel)));
            }
            if (config.capture_events) {
                // memo_scratch1 holds the cached end_pos (written by
                // helperMemoCallBegin) and is consumed below to set
                // pos. helperMemoCachedSlice writes events_start
                // through its out_start arg, so route that to
                // call_scratch instead -- otherwise it overwrites the
                // end_pos and the success path resumes at the wrong
                // position (or, with events_start = 0 on the first
                // hit, at the start of the input).
                buf.emit(encLdr(t0, sp_reg, sp_memo_ctx));
                buf.emit(encLdr(0, t0, memoOff("table_ptr")));
                buf.emit(encLdr(t1, t0, memoOff("stride")));
                emitImm64(buf, t2, mc.rule_id);
                buf.emit(encMul(t1, t1, t2));
                buf.emit(encAddReg(t1, t1, pos));
                buf.emit(encMov(1, t1));
                buf.emit(encAdd(2, sp_reg, sp_call_scratch));
                buf.emit(encLdr(t3, t0, memoOff("helper_cached_slice")));
                buf.emit(encBlr(t3));
                buf.emit(encMov(t4, 0)); // t4 = count
                const skip_replay_off = buf.off();
                buf.emit(encCbz(t4, 0));
                buf.emit(encLdr(t0, sp_reg, sp_memo_ctx));
                buf.emit(encLdr(0, sp_reg, sp_esp));
                buf.emit(encLdr(1, t0, memoOff("events_buf_ptr")));
                buf.emit(encLdr(2, sp_reg, sp_call_scratch));
                buf.emit(encMov(3, t4));
                buf.emit(encMov(4, skp));
                buf.emit(encStr(bsp, sp_reg, sp_memo_scratch2));
                buf.emit(encAdd(5, sp_reg, sp_memo_scratch2));
                buf.emit(encMov(6, cap));
                buf.emit(encLdr(t3, t0, memoOff("helper_replay_events")));
                buf.emit(encBlr(t3));
                buf.emit(encAdd(t0, 0, 1));
                addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, t0);
                buf.emit(encNop());
                buf.emit(encLdr(bsp, sp_reg, sp_memo_scratch2));
                const replay_done_off = buf.off();
                {
                    const rel: i32 = @as(i32, @intCast(replay_done_off)) - @as(i32, @intCast(skip_replay_off));
                    buf.patchAt(skip_replay_off, encCbz(t4, @intCast(rel)));
                }
            }
            buf.emit(encLdr(pos, sp_reg, sp_memo_scratch1));

            const after_all = buf.off();
            {
                const rel: i32 = @as(i32, @intCast(after_all)) - @as(i32, @intCast(lr_seed_skip_off));
                buf.patchAt(lr_seed_skip_off, encB(@intCast(rel)));
            }
        },
        .ret => {
            if (config.capture_events or config.memoize) {
                // Find the call's ret frame (tag=1) -- or, when memoize
                // is on, the matching memo frame (tag=5) -- by walking
                // down from sp-1, stash the native return address, then
                // shift any save / event / lcatch frames above it down
                // by one so the outer backtrack can still undo them.
                // Required when either capture_events (body may have
                // pushed save/event frames above the call's ret) or
                // memoize (frame may be tag=5 and need writeback) is on.
                buf.emit(encSub(t1, bsp, 1)); // t1 = sp - 1

                const find_loop = buf.off();
                buf.emit(encLsl(t0, t1, 5));
                buf.emit(encAddReg(t0, skp, t0));
                buf.emit(encLdr(t2, t0, 0));
                buf.emit(encCmpImm(t2, 1));
                const found_off = buf.off();
                buf.emit(encBCond(CC.eq, 0));
                const found_memo_branch_off = if (config.memoize) blk: {
                    buf.emit(encCmpImm(t2, 5));
                    const off = buf.off();
                    buf.emit(encBCond(CC.eq, 0));
                    break :blk off;
                } else 0;
                buf.emit(encSub(t1, t1, 1));
                {
                    const rel: i32 = @as(i32, @intCast(find_loop)) - @as(i32, @intCast(buf.off()));
                    buf.emit(encB(@intCast(rel)));
                }

                const found = buf.off();
                buf.emit(encLdr(t3, t0, 8)); // t3 = ret addr (native)

                const after_target_resolved = if (config.memoize) blk: {
                    // Skip past the memo handler block to the shift loop.
                    const skip_off = buf.off();
                    buf.emit(encNop()); // patched to b shift_loop
                    break :blk skip_off;
                } else 0;

                const found_memo = if (config.memoize) blk: {
                    const off = buf.off();
                    // helperMemoRet(memo_ctx, side_idx, end_pos,
                    //               state_ptr, bsp_ptr, &out_result)
                    //   -> action: 0 ret_done | 1 ret_regrow | 2 oom
                    //
                    // The helper handles the entire LR ret dispatch
                    // (recall completion / first-eval-with-head /
                    // grow-iter completion). For `ret_regrow` it
                    // pushes a new memo frame at the JIT's current
                    // `bsp` (read/written via the spilled `bsp_ptr`
                    // scratch). Both done and regrow flow into the
                    // same shift loop below: we always set
                    // `pos = out.new_pos` and branch to
                    // `out.native_target`. The 16-byte RetResult is
                    // laid out across `memo_scratch1` /
                    // `memo_scratch2` (adjacent slots).
                    buf.emit(encLdr(1, t0, 8)); // x1 = side_idx
                    buf.emit(encLdr(0, sp_reg, sp_memo_ctx)); // x0 = memo_ctx
                    buf.emit(encMov(2, pos)); // x2 = end_pos
                    // Spill bsp into the scratch slot the helper
                    // reads/writes through; load &slot into x3.
                    buf.emit(encStr(bsp, sp_reg, sp_call_scratch));
                    buf.emit(encAdd(3, sp_reg, sp_call_scratch));
                    buf.emit(encAdd(4, sp_reg, sp_memo_scratch1)); // x4 = &out_result
                    // Stash t1 (the matched-frame index) too -- it's
                    // caller-saved, and the shift loop depends on it.
                    buf.emit(encStr(t1, sp_reg, sp_call_scratch2));

                    buf.emit(encLdr(t3, 0, memoOff("helper_ret")));
                    buf.emit(encBlr(t3));

                    // Reload t1 (matched-frame index) and bsp.
                    buf.emit(encLdr(t1, sp_reg, sp_call_scratch2));
                    buf.emit(encLdr(bsp, sp_reg, sp_call_scratch));

                    // OOM check (action == 2).
                    buf.emit(encCmpImm(0, 2));
                    addFixup(fixups, fcount, buf.off(), .fail, .b_cond, CC.eq, 0);
                    buf.emit(encNop());

                    // Both `done` and `regrow` use the same downstream
                    // shift + branch: load native target into t3 and
                    // pos override.
                    buf.emit(encLdr(t3, sp_reg, sp_memo_scratch1));
                    buf.emit(encLdr(pos, sp_reg, sp_memo_scratch2));
                    break :blk off;
                } else 0;

                const shift_loop = buf.off();
                if (config.memoize) {
                    // Patch the early "skip past memo" branch above to
                    // jump here (after the memo handler block).
                    const rel: i32 = @as(i32, @intCast(shift_loop)) - @as(i32, @intCast(after_target_resolved));
                    buf.patchAt(after_target_resolved, encB(@intCast(rel)));
                    // And patch the find_loop branch into found_memo.
                    const rel2: i32 = @as(i32, @intCast(found_memo)) - @as(i32, @intCast(found_memo_branch_off));
                    buf.patchAt(found_memo_branch_off, encBCond(CC.eq, @intCast(rel2)));
                }
                buf.emit(encAdd(t2, t1, 1));
                buf.emit(encCmpReg(t2, bsp));
                const shift_done_off = buf.off();
                buf.emit(encBCond(CC.hs, 0));

                buf.emit(encLsl(t0, t1, 5));
                buf.emit(encAddReg(t0, skp, t0));

                buf.emit(encLdr(t4, t0, 32));
                buf.emit(encStr(t4, t0, 0));
                buf.emit(encLdr(t4, t0, 40));
                buf.emit(encStr(t4, t0, 8));
                buf.emit(encLdr(t4, t0, 48));
                buf.emit(encStr(t4, t0, 16));
                buf.emit(encLdr(t4, t0, 56));
                buf.emit(encStr(t4, t0, 24));

                buf.emit(encAdd(t1, t1, 1));
                {
                    const rel: i32 = @as(i32, @intCast(shift_loop)) - @as(i32, @intCast(buf.off()));
                    buf.emit(encB(@intCast(rel)));
                }

                const shift_done = buf.off();
                {
                    const rel: i32 = @as(i32, @intCast(found)) - @as(i32, @intCast(found_off));
                    buf.patchAt(found_off, encBCond(CC.eq, @intCast(rel)));
                }
                {
                    const rel: i32 = @as(i32, @intCast(shift_done)) - @as(i32, @intCast(shift_done_off));
                    buf.patchAt(shift_done_off, encBCond(CC.hs, @intCast(rel)));
                }

                buf.emit(encSub(bsp, bsp, 1));
                buf.emit(encBr(t3));
            } else {
                buf.emit(encSub(bsp, bsp, 1));
                buf.emit(encLsl(t1, bsp, 5));
                buf.emit(encAddReg(t1, skp, t1));
                buf.emit(encLdr(t2, t1, 8));
                buf.emit(encBr(t2));
            }
        },
        .save => {
            const slot: u12 = @intCast(inst.data.slot);
            if (config.capture_events) {
                // Call helper_append_save(state, slot, pos). The helper
                // returns the pre-append length in x0 (or maxInt on OOM);
                // we stash that in the stack entry's event_len so the
                // backtrack path truncates in lockstep.
                buf.emit(encLdr(0, sp_reg, sp_esp)); // x0 = state_ptr
                buf.emit(encMovz(1, slot, 0)); // x1 = slot
                buf.emit(encMov(2, pos)); // x2 = pos
                buf.emit(encLdr(t0, sp_reg, sp_has));
                buf.emit(encBlr(t0));
                // OOM check: x0 + 1 == 0 iff x0 was maxInt(u64).
                buf.emit(encAdd(t0, 0, 1));
                addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, t0);
                buf.emit(encNop());
                // Preserve pre_len in t4 before the rest of save clobbers x0.
                buf.emit(encMov(t4, 0));
                // Build stack entry.
                buf.emit(encMovz(t0, slot, 0));
                buf.emit(encLdrReg(t1, cap, t0));
                buf.emit(encLsl(t2, bsp, 5));
                buf.emit(encAddReg(t2, skp, t2));
                buf.emit(encMovz(t3, 2, 0));
                buf.emit(encStr(t3, t2, 0));
                buf.emit(encStr(t0, t2, 8));
                buf.emit(encStr(t1, t2, 16));
                buf.emit(encStr(t4, t2, 24));
                buf.emit(encAdd(bsp, bsp, 1));
                buf.emit(encStrReg(pos, cap, t0));
            } else {
                buf.emit(encMovz(t0, slot, 0));
                buf.emit(encLdrReg(t1, cap, t0));
                buf.emit(encLsl(t2, bsp, 5));
                buf.emit(encAddReg(t2, skp, t2));
                buf.emit(encMovz(t3, 2, 0));
                buf.emit(encStr(t3, t2, 0));
                buf.emit(encStr(t0, t2, 8));
                buf.emit(encStr(t1, t2, 16));
                buf.emit(encAdd(bsp, bsp, 1));
                buf.emit(encStrReg(pos, cap, t0));
            }
        },
        .match => {
            buf.emit(encMov(0, pos));
            addFixup(fixups, fcount, buf.off(), .success, .b, 0, 0);
            buf.emit(encNop());
        },
        .string => {
            const ref = inst.data.string;
            buf.emit(encMov(0, inp));
            buf.emit(encMov(1, inl));
            buf.emit(encMov(2, pos));
            buf.emit(encMov(3, sdp));
            buf.emit(encMovz(4, ref.offset, 0));
            buf.emit(encMovz(5, @intCast(ref.len), 0));
            buf.emit(encLdr(t0, sp_reg, sp_hsm));
            buf.emit(encBlr(t0));
            addFixup(fixups, fcount, buf.off(), .backtrack, .cbz, 0, 0);
            buf.emit(encNop());
            buf.emit(encAdd(pos, pos, @intCast(ref.len)));
        },
        .set => emitCharsetCheck(buf, inst.data.charset, false, fixups, fcount),
        .neg_set => emitCharsetCheck(buf, inst.data.charset, true, fixups, fcount),
        .event_open, .event_close => {
            if (config.capture_events) {
                const group_id: u16 = inst.data.slot;
                const slot: u16 = if (inst.op == .event_open)
                    group_id << 1
                else
                    (group_id << 1) | 1;
                // Call helper_append_save(state, slot, pos)
                buf.emit(encLdr(0, sp_reg, sp_esp));
                buf.emit(encMovz(1, slot, 0));
                buf.emit(encMov(2, pos));
                buf.emit(encLdr(t0, sp_reg, sp_has));
                buf.emit(encBlr(t0));
                // OOM check: x0 + 1 == 0 iff x0 was -1.
                buf.emit(encAdd(t0, 0, 1));
                addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, t0);
                buf.emit(encNop());
                // Preserve pre_len in t4 (the call clobbered x0..x18).
                buf.emit(encMov(t4, 0));
                // Build stack entry: tag=3, event_len in slot 24.
                buf.emit(encLsl(t1, bsp, 5));
                buf.emit(encAddReg(t1, skp, t1));
                buf.emit(encMovz(t2, 3, 0));
                buf.emit(encStr(t2, t1, 0));
                buf.emit(encStr(t4, t1, 24));
                buf.emit(encAdd(bsp, bsp, 1));
            }
            // capture_events off: no-op.
        },
        .event_token => {
            if (config.capture_events) {
                const len: u8 = inst.data.byte;
                // Call helper_append_token(state, start, end) where
                // start = pos - len and end = pos.
                buf.emit(encLdr(0, sp_reg, sp_esp));
                if (len == 0) {
                    buf.emit(encMov(1, pos));
                } else {
                    buf.emit(encSub(1, pos, len));
                }
                buf.emit(encMov(2, pos));
                buf.emit(encLdr(t0, sp_reg, sp_hat));
                buf.emit(encBlr(t0));
                buf.emit(encAdd(t0, 0, 1));
                addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, t0);
                buf.emit(encNop());
                buf.emit(encMov(t4, 0));
                buf.emit(encLsl(t1, bsp, 5));
                buf.emit(encAddReg(t1, skp, t1));
                buf.emit(encMovz(t2, 3, 0));
                buf.emit(encStr(t2, t1, 0));
                buf.emit(encStr(t4, t1, 24));
                buf.emit(encAdd(bsp, bsp, 1));
            }
            // capture_events off: no-op (mirrors VM behavior).
        },
        .event_field => {
            if (config.capture_events) {
                const field_id: u16 = inst.data.slot;
                buf.emit(encLdr(0, sp_reg, sp_esp));
                buf.emit(encMovz(1, field_id, 0));
                buf.emit(encMov(2, pos));
                buf.emit(encLdr(t0, sp_reg, sp_haf));
                buf.emit(encBlr(t0));
                buf.emit(encAdd(t0, 0, 1));
                addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, t0);
                buf.emit(encNop());
                buf.emit(encMov(t4, 0));
                buf.emit(encLsl(t1, bsp, 5));
                buf.emit(encAddReg(t1, skp, t1));
                buf.emit(encMovz(t2, 3, 0));
                buf.emit(encStr(t2, t1, 0));
                buf.emit(encStr(t4, t1, 24));
                buf.emit(encAdd(bsp, bsp, 1));
            }
            // capture_events off: no-op (mirrors VM behavior).
        },
        .event_error_open, .event_error_close, .event_missing => {
            if (config.capture_events) {
                const label: u16 = inst.data.slot;
                const helper_slot: u15 = switch (inst.op) {
                    .event_error_open => sp_haeo,
                    .event_error_close => sp_haec,
                    .event_missing => sp_haem,
                    else => unreachable,
                };
                buf.emit(encLdr(0, sp_reg, sp_esp));
                buf.emit(encMovz(1, label, 0));
                buf.emit(encMov(2, pos));
                buf.emit(encLdr(t0, sp_reg, helper_slot));
                buf.emit(encBlr(t0));
                buf.emit(encAdd(t0, 0, 1));
                addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, t0);
                buf.emit(encNop());
                buf.emit(encMov(t4, 0));
                buf.emit(encLsl(t1, bsp, 5));
                buf.emit(encAddReg(t1, skp, t1));
                buf.emit(encMovz(t2, 3, 0));
                buf.emit(encStr(t2, t1, 0));
                buf.emit(encStr(t4, t1, 24));
                buf.emit(encAdd(bsp, bsp, 1));
            }
            // capture_events off: no-op.
        },
        .lcatch => {
            // Push a frame with tag=4, val1=label, val2=handler_pc,
            // event_len=current events length. The backtrack handler
            // treats tag>=3 like an event frame (truncate-and-skip), so
            // a regular `fail` walks past lcatch as required by
            // committed-choice semantics. A matching `throw` is what
            // honors the catch.
            //
            // Compile rejects grammars with .lcatch when capture_events
            // is off, so we always have a state pointer here.
            const ch = inst.data.catch_handler;
            // x0 = state_ptr; call helper_events_len.
            buf.emit(encLdr(0, sp_reg, sp_esp));
            buf.emit(encLdr(t0, sp_reg, sp_hel));
            buf.emit(encBlr(t0));
            // x0 = events length. Move to t4 before clobbering scratch
            // regs while building the stack entry.
            buf.emit(encMov(t4, 0));
            buf.emit(encLsl(t1, bsp, 5));
            buf.emit(encAddReg(t1, skp, t1));
            buf.emit(encMovz(t2, 4, 0)); // tag=4 (lcatch)
            buf.emit(encStr(t2, t1, 0));
            buf.emit(encMovz(t2, ch.label, 0));
            buf.emit(encStr(t2, t1, 8));
            // handler_pc may be > 65535; emit as movz+movk to be safe.
            emitImm64(buf, t2, ch.handler_pc);
            buf.emit(encStr(t2, t1, 16));
            buf.emit(encStr(t4, t1, 24));
            buf.emit(encAdd(bsp, bsp, 1));
        },
        .throw => {
            // Call helperThrow(state, stack, &sp, label, throw_pos).
            // The helper modifies *sp via the pointer, then returns
            // either the handler bytecode PC (jump via the existing
            // jump table) or the `throw_miss` sentinel (route to fail).
            const label: u16 = inst.data.slot;

            // Stash current bsp at sp_call_scratch so the helper sees
            // the right depth and we can read back the new depth.
            buf.emit(encStr(bsp, sp_reg, sp_call_scratch));

            buf.emit(encLdr(0, sp_reg, sp_esp)); // x0 = state_ptr
            buf.emit(encMov(1, skp)); // x1 = stack_ptr
            buf.emit(encAdd(2, sp_reg, sp_call_scratch)); // x2 = &sp
            buf.emit(encMovz(3, label, 0)); // x3 = label
            buf.emit(encMov(4, pos)); // x4 = throw_pos

            buf.emit(encLdr(t0, sp_reg, sp_ht));
            buf.emit(encBlr(t0));

            // Reload bsp from the scratch slot (helper updated it).
            buf.emit(encLdr(bsp, sp_reg, sp_call_scratch));

            // Check for throw_miss sentinel (= maxInt(u64)). Same trick
            // as the OOM check elsewhere: x0 + 1 == 0 iff x0 == -1.
            buf.emit(encAdd(t0, 0, 1));
            addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, t0);
            buf.emit(encNop());

            // x0 = handler bytecode PC. Index into jump_table to get
            // the native code offset, add code_base, branch.
            buf.emit(encLsl(t1, 0, 3)); // t1 = bc_pc * 8
            buf.emit(encAddReg(t1, jtp, t1)); // t1 = &jt[bc_pc]
            buf.emit(encLdr(t1, t1, 0)); // t1 = jt[bc_pc]
            buf.emit(encAddReg(t1, cbp, t1)); // t1 = code_base + offset
            buf.emit(encBr(t1));
        },
    }
}
