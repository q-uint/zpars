/// x86_64 backend for the JIT compiler.
///
/// Encodes native x86_64 machine code from bytecode instructions.
const std = @import("std");
const I = @import("Instruction.zig");
const Jit = @import("Jit.zig");

const page_size = Jit.page_size;

// rbx  = pos        (current input position)
// rbp  = bsp        (backtrack stack index)
// r14  = inp        (base pointer to input bytes)
// r13  = inl        (input length)
// r12  = cap        (captures ptr)
// r15  = skp        (backtrack stack ptr)
//
// Stack-stored context (loaded into scratch regs when needed):
//   [rsp+0]  = charsets_ptr
//   [rsp+8]  = string_data_ptr
//   [rsp+16] = jump_table_ptr
//   [rsp+24] = code_base_ptr

const Reg = u4;

const pos: Reg = 3; // rbx
const bsp: Reg = 5; // rbp
const inp: Reg = 14; // r14
const inl: Reg = 13; // r13
const cap: Reg = 12; // r12
const skp: Reg = 15; // r15

// Scratch registers (caller-saved, clobbered by CALL)
const t0: Reg = 0; // rax (also return value)
const t1: Reg = 1; // rcx
const t2: Reg = 2; // rdx
const t3: Reg = 10; // r10
const t4: Reg = 11; // r11

// Argument registers (System V AMD64 ABI)
const rdi_r: Reg = 7;
const rsi_r: Reg = 6;
const r8_r: Reg = 8;
const r9_r: Reg = 9;
const rsp_r: Reg = 4;

// Stack frame layout. The 4 prefix slots hold values aarch64 keeps in
// callee-saved registers (jump table / code base / charsets / string
// data); the rest mirror `Jit.StackSlots` 1:1 -- adding or reordering
// a helper slot there updates both backends.
const stk_csp: i32 = 0;
const stk_sdp: i32 = 8;
const stk_jtp: i32 = 16;
const stk_cbp: i32 = 24;
const stk_shared_base: i32 = 32;

fn stkSlot(comptime field: []const u8) i32 {
    return stk_shared_base + @as(i32, @intCast(@offsetOf(Jit.StackSlots, field)));
}

const stk_hsm: i32 = stkSlot("helper_string_match");
const stk_hcm: i32 = stkSlot("helper_charset_match");
const stk_esp: i32 = stkSlot("events_state_ptr");
const stk_has: i32 = stkSlot("helper_append_save");
const stk_hte: i32 = stkSlot("helper_truncate_events");
const stk_hat: i32 = stkSlot("helper_append_token");
const stk_haf: i32 = stkSlot("helper_append_field");
const stk_haeo: i32 = stkSlot("helper_append_error_open");
const stk_haec: i32 = stkSlot("helper_append_error_close");
const stk_haem: i32 = stkSlot("helper_append_missing");
const stk_ht: i32 = stkSlot("helper_throw");
const stk_hel: i32 = stkSlot("helper_events_len");
const stk_call_scratch: i32 = stkSlot("call_scratch");
const stk_memo_ctx: i32 = stkSlot("memo_ctx");
const stk_memo_scratch1: i32 = stkSlot("memo_scratch1");
const stk_memo_scratch2: i32 = stkSlot("memo_scratch2");

fn stkSize(comptime config: Jit.Config) i32 {
    // SysV: rsp must be 8 mod 16 just before an inner CALL so the
    // callee enters with rsp 0 mod 16. Function entry rsp is 8 mod 16,
    // 6 register pushes drop it by 48 bytes (still 8 mod 16), so
    // stkSize must itself be ≡ 8 mod 16 to preserve the invariant.
    if (config.memoize) return 168;
    if (config.capture_events) return 136;
    return 48;
}

/// Comptime offset of a `Jit.JitCtx` field, narrowed to `i32` for the
/// x86_64 displacement encoding. Using `@offsetOf` here keeps the
/// prologue in lock-step with `JitCtx`'s layout: reorder or insert a
/// field and the codegen follows automatically.
fn ctxOff(comptime field: []const u8) i32 {
    return @intCast(@offsetOf(Jit.JitCtx, field));
}

/// Same idea for `Jit.MemoCtx`. Used by the `memo_call` / `ret` /
/// backtrack lowerings when they index into the runtime memo bundle.
fn memoOff(comptime field: []const u8) i32 {
    return @intCast(@offsetOf(Jit.MemoCtx, field));
}

/// Same idea for `Jit.Frame` (memo side-table entry). Used by the
/// `memo_call` miss path when it writes the new frame.
fn frameOff(comptime field: []const u8) i32 {
    return @intCast(@offsetOf(Jit.Frame, field));
}

// Condition codes for Jcc
const CC = struct {
    const eq: u4 = 0x4; // JE/JZ
    const ne: u4 = 0x5; // JNE/JNZ
    const ae: u4 = 0x3; // JAE (unsigned >=)
    const z: u4 = 0x4;
    const nz: u4 = 0x5;
};

fn regLo(r: Reg) u3 {
    return @truncate(r);
}

fn regHi(r: Reg) u1 {
    return @truncate(r >> 3);
}

fn rexW(reg: Reg, rm: Reg) u8 {
    return 0x48 | (@as(u8, regHi(reg)) << 2) | @as(u8, regHi(rm));
}

fn rexWXB(reg: Reg, index: Reg, base: Reg) u8 {
    return 0x48 | (@as(u8, regHi(reg)) << 2) | (@as(u8, regHi(index)) << 1) | @as(u8, regHi(base));
}

fn modrmByte(mod: u2, reg: u3, rm: u3) u8 {
    return (@as(u8, mod) << 6) | (@as(u8, reg) << 3) | rm;
}

fn sibByte(scale: u2, index: u3, base: u3) u8 {
    return (@as(u8, scale) << 6) | (@as(u8, index) << 3) | base;
}

fn emitModRMDisp(buf: *Buf, reg: Reg, base: Reg, disp: i32) void {
    const rl = regLo(reg);
    const bl = regLo(base);

    if (disp == 0 and bl != 5) {
        if (bl == 4) {
            buf.emit1(modrmByte(0, rl, 4));
            buf.emit1(sibByte(0, 4, 4));
        } else {
            buf.emit1(modrmByte(0, rl, bl));
        }
    } else if (disp >= -128 and disp <= 127) {
        if (bl == 4) {
            buf.emit1(modrmByte(1, rl, 4));
            buf.emit1(sibByte(0, 4, 4));
        } else {
            buf.emit1(modrmByte(1, rl, bl));
        }
        buf.emit1(@bitCast(@as(i8, @intCast(disp))));
    } else {
        if (bl == 4) {
            buf.emit1(modrmByte(2, rl, 4));
            buf.emit1(sibByte(0, 4, 4));
        } else {
            buf.emit1(modrmByte(2, rl, bl));
        }
        buf.emitI32(disp);
    }
}

fn emitModRMSib(buf: *Buf, reg: Reg, base: Reg, index: Reg, scale: u2) void {
    const bl = regLo(base);
    if (bl == 5) {
        buf.emit1(modrmByte(1, regLo(reg), 4));
        buf.emit1(sibByte(scale, regLo(index), bl));
        buf.emit1(0);
    } else {
        buf.emit1(modrmByte(0, regLo(reg), 4));
        buf.emit1(sibByte(scale, regLo(index), bl));
    }
}

fn emitPush(buf: *Buf, r: Reg) void {
    if (regHi(r) != 0) buf.emit1(0x40 | @as(u8, regHi(r)));
    buf.emit1(0x50 | @as(u8, regLo(r)));
}

fn emitPop(buf: *Buf, r: Reg) void {
    if (regHi(r) != 0) buf.emit1(0x40 | @as(u8, regHi(r)));
    buf.emit1(0x58 | @as(u8, regLo(r)));
}

fn emitMovRR(buf: *Buf, dst: Reg, src: Reg) void {
    buf.emit1(rexW(src, dst));
    buf.emit1(0x89);
    buf.emit1(modrmByte(3, regLo(src), regLo(dst)));
}

fn emitMovRM(buf: *Buf, dst: Reg, base: Reg, disp: i32) void {
    buf.emit1(rexW(dst, base));
    buf.emit1(0x8B);
    emitModRMDisp(buf, dst, base, disp);
}

fn emitMovMR(buf: *Buf, base: Reg, disp: i32, src: Reg) void {
    buf.emit1(rexW(src, base));
    buf.emit1(0x89);
    emitModRMDisp(buf, src, base, disp);
}

fn emitMovMI(buf: *Buf, base: Reg, disp: i32, imm: i32) void {
    buf.emit1(0x48 | @as(u8, regHi(base)));
    buf.emit1(0xC7);
    emitModRMDisp(buf, @as(Reg, 0), base, disp);
    buf.emitI32(imm);
}

/// `mov dword ptr [base + disp], imm32`. 32-bit memory store with a
/// 32-bit immediate (no sign extension to 64 bits unlike `emitMovMI`).
fn emitMovMI32(buf: *Buf, base: Reg, disp: i32, imm: i32) void {
    if (regHi(base) != 0) buf.emit1(0x40 | @as(u8, regHi(base)));
    buf.emit1(0xC7);
    emitModRMDisp(buf, @as(Reg, 0), base, disp);
    buf.emitI32(imm);
}

/// `mov dword ptr [base + disp], src` (32-bit register store).
fn emitMov32MR(buf: *Buf, base: Reg, disp: i32, src: Reg) void {
    const need_rex = regHi(base) != 0 or regHi(src) != 0;
    if (need_rex) buf.emit1(0x40 | (@as(u8, regHi(src)) << 2) | @as(u8, regHi(base)));
    buf.emit1(0x89);
    emitModRMDisp(buf, src, base, disp);
}

/// `imul dst, src` (64-bit). `dst = dst * src`.
fn emitImulRR(buf: *Buf, dst: Reg, src: Reg) void {
    buf.emit1(rexW(dst, src));
    buf.emit1(0x0F);
    buf.emit1(0xAF);
    buf.emit1(modrmByte(3, regLo(dst), regLo(src)));
}

/// `add dst, src` (64-bit register-to-register).
fn emitAddRR(buf: *Buf, dst: Reg, src: Reg) void {
    buf.emit1(rexW(src, dst));
    buf.emit1(0x01);
    buf.emit1(modrmByte(3, regLo(src), regLo(dst)));
}

fn emitMovRI64(buf: *Buf, dst: Reg, val: u64) void {
    buf.emit1(0x48 | @as(u8, regHi(dst)));
    buf.emit1(0xB8 | @as(u8, regLo(dst)));
    buf.emitU64(val);
}

fn emitMovRI32(buf: *Buf, dst: Reg, val: u32) void {
    if (regHi(dst) != 0) buf.emit1(0x40 | @as(u8, regHi(dst)));
    buf.emit1(0xB8 | @as(u8, regLo(dst)));
    buf.emitU32(val);
}

fn emitMovzxRM8Sib(buf: *Buf, dst: Reg, base: Reg, index: Reg) void {
    const need_rex = regHi(dst) != 0 or regHi(index) != 0 or regHi(base) != 0;
    if (need_rex) {
        buf.emit1(0x40 | (@as(u8, regHi(dst)) << 2) |
            (@as(u8, regHi(index)) << 1) | @as(u8, regHi(base)));
    }
    buf.emit1(0x0F);
    buf.emit1(0xB6);
    emitModRMSib(buf, dst, base, index, 0);
}

fn emitMovRMSib8(buf: *Buf, dst: Reg, base: Reg, index: Reg) void {
    buf.emit1(rexWXB(dst, index, base));
    buf.emit1(0x8B);
    emitModRMSib(buf, dst, base, index, 3);
}

fn emitMovMRSib8(buf: *Buf, base: Reg, index: Reg, src: Reg) void {
    buf.emit1(rexWXB(src, index, base));
    buf.emit1(0x89);
    emitModRMSib(buf, src, base, index, 3);
}

fn emitLeaRR(buf: *Buf, dst: Reg, base: Reg, index: Reg) void {
    buf.emit1(rexWXB(dst, index, base));
    buf.emit1(0x8D);
    emitModRMSib(buf, dst, base, index, 0);
}

fn emitLeaRip(buf: *Buf, dst: Reg, disp: i32) void {
    buf.emit1(0x48 | (@as(u8, regHi(dst)) << 2));
    buf.emit1(0x8D);
    buf.emit1(modrmByte(0, regLo(dst), 5));
    buf.emitI32(disp);
}

/// `lea dst, [base + disp]`. The ModR/M emitter handles the rsp/r12
/// SIB-byte requirement transparently.
fn emitLeaRMDisp(buf: *Buf, dst: Reg, base: Reg, disp: i32) void {
    buf.emit1(rexW(dst, base));
    buf.emit1(0x8D);
    emitModRMDisp(buf, dst, base, disp);
}

fn emitAddRI(buf: *Buf, dst: Reg, imm: i32) void {
    buf.emit1(0x48 | @as(u8, regHi(dst)));
    if (imm >= -128 and imm <= 127) {
        buf.emit1(0x83);
        buf.emit1(modrmByte(3, 0, regLo(dst)));
        buf.emit1(@bitCast(@as(i8, @intCast(imm))));
    } else {
        buf.emit1(0x81);
        buf.emit1(modrmByte(3, 0, regLo(dst)));
        buf.emitI32(imm);
    }
}

fn emitSubRI(buf: *Buf, dst: Reg, imm: i32) void {
    buf.emit1(0x48 | @as(u8, regHi(dst)));
    if (imm >= -128 and imm <= 127) {
        buf.emit1(0x83);
        buf.emit1(modrmByte(3, 5, regLo(dst)));
        buf.emit1(@bitCast(@as(i8, @intCast(imm))));
    } else {
        buf.emit1(0x81);
        buf.emit1(modrmByte(3, 5, regLo(dst)));
        buf.emitI32(imm);
    }
}

fn emitInc(buf: *Buf, dst: Reg) void {
    buf.emit1(0x48 | @as(u8, regHi(dst)));
    buf.emit1(0xFF);
    buf.emit1(modrmByte(3, 0, regLo(dst)));
}

fn emitDec(buf: *Buf, dst: Reg) void {
    buf.emit1(0x48 | @as(u8, regHi(dst)));
    buf.emit1(0xFF);
    buf.emit1(modrmByte(3, 1, regLo(dst)));
}

fn emitCmpRR(buf: *Buf, r1: Reg, r2: Reg) void {
    buf.emit1(rexW(r2, r1));
    buf.emit1(0x39);
    buf.emit1(modrmByte(3, regLo(r2), regLo(r1)));
}

fn emitCmp32RI8(buf: *Buf, r: Reg, imm: u8) void {
    if (regHi(r) != 0) buf.emit1(0x40 | @as(u8, regHi(r)));
    buf.emit1(0x83);
    buf.emit1(modrmByte(3, 7, regLo(r)));
    buf.emit1(imm);
}

/// 64-bit `cmp r, imm8` with sign-extension. Encodes -128..127 and is
/// useful for comparing against the OOM sentinel (maxInt(u64) = -1).
fn emitCmpRI8(buf: *Buf, r: Reg, imm: i8) void {
    buf.emit1(0x48 | @as(u8, regHi(r)));
    buf.emit1(0x83);
    buf.emit1(modrmByte(3, 7, regLo(r)));
    buf.emit1(@bitCast(imm));
}

fn emitShlRI(buf: *Buf, dst: Reg, amount: u8) void {
    buf.emit1(0x48 | @as(u8, regHi(dst)));
    buf.emit1(0xC1);
    buf.emit1(modrmByte(3, 4, regLo(dst)));
    buf.emit1(amount);
}

fn emitTestRR(buf: *Buf, r1: Reg, r2: Reg) void {
    buf.emit1(rexW(r2, r1));
    buf.emit1(0x85);
    buf.emit1(modrmByte(3, regLo(r2), regLo(r1)));
}

fn emitXorRR32(buf: *Buf, r: Reg) void {
    if (regHi(r) != 0) {
        buf.emit1(0x40 | (@as(u8, regHi(r)) << 2) | @as(u8, regHi(r)));
    }
    buf.emit1(0x31);
    buf.emit1(modrmByte(3, regLo(r), regLo(r)));
}

fn emitJmpRel32(buf: *Buf) u32 {
    buf.emit1(0xE9);
    const off = buf.off();
    buf.emitI32(0);
    return off;
}

fn emitJccRel32(buf: *Buf, cc: u4) u32 {
    buf.emit1(0x0F);
    buf.emit1(0x80 | @as(u8, cc));
    const off = buf.off();
    buf.emitI32(0);
    return off;
}

fn emitCallR(buf: *Buf, r: Reg) void {
    if (regHi(r) != 0) buf.emit1(0x40 | @as(u8, regHi(r)));
    buf.emit1(0xFF);
    buf.emit1(modrmByte(3, 2, regLo(r)));
}

fn emitJmpR(buf: *Buf, r: Reg) void {
    if (regHi(r) != 0) buf.emit1(0x40 | @as(u8, regHi(r)));
    buf.emit1(0xFF);
    buf.emit1(modrmByte(3, 4, regLo(r)));
}

fn emitRetInst(buf: *Buf) void {
    buf.emit1(0xC3);
}

const Buf = struct {
    ptr: [*]u8,
    len: usize,

    fn emit1(self: *Buf, b: u8) void {
        self.ptr[self.len] = b;
        self.len += 1;
    }

    fn emitI32(self: *Buf, v: i32) void {
        @memcpy(self.ptr[self.len..][0..4], &@as([4]u8, @bitCast(v)));
        self.len += 4;
    }

    fn emitU32(self: *Buf, v: u32) void {
        @memcpy(self.ptr[self.len..][0..4], &@as([4]u8, @bitCast(v)));
        self.len += 4;
    }

    fn emitU64(self: *Buf, v: u64) void {
        @memcpy(self.ptr[self.len..][0..8], &@as([8]u8, @bitCast(v)));
        self.len += 8;
    }

    fn off(self: *const Buf) u32 {
        return @intCast(self.len);
    }

    fn patchI32At(self: *Buf, offset: u32, v: i32) void {
        @memcpy(self.ptr[offset..][0..4], &@as([4]u8, @bitCast(v)));
    }
};

const FixupTarget = Jit.FixupTarget;

const Fixup = struct {
    rel32_off: u32,
    target: FixupTarget,
};

fn addFixup(fixups: *[8192]Fixup, count: *usize, rel32_off: u32, target: FixupTarget) void {
    fixups[count.*] = .{ .rel32_off = rel32_off, .target = target };
    count.* += 1;
}

fn patchRel32(buf: *Buf, rel32_off: u32, target_off: u32) void {
    const rel: i32 = @as(i32, @intCast(target_off)) - @as(i32, @intCast(rel32_off + 4));
    buf.patchI32At(rel32_off, rel);
}

pub const GenerateResult = struct {
    native_len: usize,
    jump_table: [4096]u64,
};

pub fn estimateSize(comptime config: Jit.Config, code_len: usize) usize {
    // memo_call is the largest opcode (~140 bytes including the
    // success-path replay setup); bump the budget when it can appear
    // so we don't underprovision the mmap.
    const per_inst: usize = if (config.memoize)
        480
    else if (config.capture_events) 192 else 128;
    return (code_len + 1) * per_inst + 4096;
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
    emitMovRI64(&buf, t0, Jit.null_cap);

    const succ_off = buf.off();
    emitEpilogue(config, &buf);

    for (fixups[0..fcount]) |f| {
        const tgt_off: u32 = switch (f.target) {
            .backtrack => bt_off,
            .fail => fail_off,
            .success => succ_off,
            _ => bc_map[@intFromEnum(f.target)],
        };
        patchRel32(&buf, f.rel32_off, tgt_off);
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
    // memo_call only has a code path when memoize is on. Reject up
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

fn emitPrologue(comptime config: Jit.Config, buf: *Buf) void {
    emitPush(buf, pos); // rbx
    emitPush(buf, bsp); // rbp
    emitPush(buf, cap); // r12
    emitPush(buf, inl); // r13
    emitPush(buf, inp); // r14
    emitPush(buf, skp); // r15

    emitSubRI(buf, rsp_r, stkSize(config));

    // rdi = pointer to JitCtx. Load fields into regs and stack slots.
    emitMovRM(buf, inp, rdi_r, ctxOff("input_ptr"));
    emitMovRM(buf, inl, rdi_r, ctxOff("input_len"));
    emitMovRM(buf, t0, rdi_r, ctxOff("charsets_ptr"));
    emitMovMR(buf, rsp_r, stk_csp, t0);
    emitMovRM(buf, t0, rdi_r, ctxOff("string_data_ptr"));
    emitMovMR(buf, rsp_r, stk_sdp, t0);
    emitMovRM(buf, cap, rdi_r, ctxOff("captures_ptr"));
    emitMovRM(buf, skp, rdi_r, ctxOff("stack_ptr"));
    emitMovRM(buf, t0, rdi_r, ctxOff("jump_table_ptr"));
    emitMovMR(buf, rsp_r, stk_jtp, t0);
    emitMovRM(buf, t0, rdi_r, ctxOff("code_base_ptr"));
    emitMovMR(buf, rsp_r, stk_cbp, t0);
    emitMovRM(buf, t0, rdi_r, ctxOff("helper_string_match"));
    emitMovMR(buf, rsp_r, stk_hsm, t0);
    emitMovRM(buf, t0, rdi_r, ctxOff("helper_charset_match"));
    emitMovMR(buf, rsp_r, stk_hcm, t0);

    if (config.capture_events) {
        emitMovRM(buf, t0, rdi_r, ctxOff("events_state_ptr"));
        emitMovMR(buf, rsp_r, stk_esp, t0);
        emitMovRM(buf, t0, rdi_r, ctxOff("helper_append_save"));
        emitMovMR(buf, rsp_r, stk_has, t0);
        emitMovRM(buf, t0, rdi_r, ctxOff("helper_truncate_events"));
        emitMovMR(buf, rsp_r, stk_hte, t0);
        emitMovRM(buf, t0, rdi_r, ctxOff("helper_append_token"));
        emitMovMR(buf, rsp_r, stk_hat, t0);
        emitMovRM(buf, t0, rdi_r, ctxOff("helper_append_field"));
        emitMovMR(buf, rsp_r, stk_haf, t0);
        emitMovRM(buf, t0, rdi_r, ctxOff("helper_append_error_open"));
        emitMovMR(buf, rsp_r, stk_haeo, t0);
        emitMovRM(buf, t0, rdi_r, ctxOff("helper_append_error_close"));
        emitMovMR(buf, rsp_r, stk_haec, t0);
        emitMovRM(buf, t0, rdi_r, ctxOff("helper_append_missing"));
        emitMovMR(buf, rsp_r, stk_haem, t0);
        emitMovRM(buf, t0, rdi_r, ctxOff("helper_throw"));
        emitMovMR(buf, rsp_r, stk_ht, t0);
        emitMovRM(buf, t0, rdi_r, ctxOff("helper_events_len"));
        emitMovMR(buf, rsp_r, stk_hel, t0);
    }
    if (config.memoize) {
        emitMovRM(buf, t0, rdi_r, ctxOff("memo_ctx_ptr"));
        emitMovMR(buf, rsp_r, stk_memo_ctx, t0);
    }

    emitXorRR32(buf, pos);
    emitXorRR32(buf, bsp);
}

fn emitEpilogue(comptime config: Jit.Config, buf: *Buf) void {
    emitAddRI(buf, rsp_r, stkSize(config));
    emitPop(buf, skp); // r15
    emitPop(buf, inp); // r14
    emitPop(buf, inl); // r13
    emitPop(buf, cap); // r12
    emitPop(buf, bsp); // rbp
    emitPop(buf, pos); // rbx
    emitRetInst(buf);
}

fn emitBacktrackHandler(
    comptime config: Jit.Config,
    buf: *Buf,
    fixups: *[8192]Fixup,
    fcount: *usize,
) void {
    emitTestRR(buf, bsp, bsp);
    addFixup(fixups, fcount, emitJccRel32(buf, CC.z), .fail);

    const loop_off = buf.off();

    emitDec(buf, bsp);

    emitMovRR(buf, t0, bsp);
    emitShlRI(buf, t0, 5);
    emitLeaRR(buf, t0, skp, t0);

    emitMovRM(buf, t1, t0, 0); // tag

    emitTestRR(buf, t1, t1);
    const choice_rel32 = emitJccRel32(buf, CC.z);

    emitCmp32RI8(buf, t1, 2);
    const save_rel32 = emitJccRel32(buf, CC.eq);

    // Memo frames (tag=5) are dispatched before the event check so the
    // `ae` event check stays correct -- it would otherwise catch tag=5
    // too.
    const memo_rel32 = if (config.memoize) blk: {
        emitCmp32RI8(buf, t1, 5);
        break :blk emitJccRel32(buf, CC.eq);
    } else 0;

    const event_rel32 = if (config.capture_events) blk: {
        // Tags 3 (event) and 4 (lcatch) both go to event_handler:
        // both truncate the event log to the snapshot in slot 24 and
        // do not restore any capture slot. `ae` (unsigned >=) catches
        // both. tag 5 was dispatched above when memoize is on.
        emitCmp32RI8(buf, t1, 3);
        break :blk emitJccRel32(buf, CC.ae);
    } else 0;

    // tag == 1 (ret): skip, continue loop
    emitTestRR(buf, bsp, bsp);
    addFixup(fixups, fcount, emitJccRel32(buf, CC.z), .fail);
    {
        const jmp_off = emitJmpRel32(buf);
        patchRel32(buf, jmp_off, loop_off);
    }

    // save handler
    const save_handler_off = buf.off();
    if (config.capture_events) {
        // Truncate events to the snapshot taken at save time.
        // t0 still points to the stack entry; offset 24 = event_len.
        // The call clobbers caller-saved regs, so we re-derive t0 after.
        emitMovRM(buf, rsi_r, t0, 24); // rsi = event_len (2nd arg)
        emitMovRM(buf, rdi_r, rsp_r, stk_esp); // rdi = state_ptr (1st arg)
        emitMovRM(buf, t3, rsp_r, stk_hte); // t3 = helper_truncate_events
        emitCallR(buf, t3);
        // Recompute t0 = &stack_entry[bsp]
        emitMovRR(buf, t0, bsp);
        emitShlRI(buf, t0, 5);
        emitLeaRR(buf, t0, skp, t0);
    }
    emitMovRM(buf, t2, t0, 8);
    emitMovRM(buf, t1, t0, 16);
    emitMovMRSib8(buf, cap, t2, t1);
    emitTestRR(buf, bsp, bsp);
    addFixup(fixups, fcount, emitJccRel32(buf, CC.z), .fail);
    {
        const jmp_off = emitJmpRel32(buf);
        patchRel32(buf, jmp_off, loop_off);
    }

    // event handler (capture_events only)
    const event_handler_off = if (config.capture_events) blk: {
        const off = buf.off();
        emitMovRM(buf, rsi_r, t0, 24); // event_len
        emitMovRM(buf, rdi_r, rsp_r, stk_esp);
        emitMovRM(buf, t3, rsp_r, stk_hte);
        emitCallR(buf, t3);
        emitTestRR(buf, bsp, bsp);
        addFixup(fixups, fcount, emitJccRel32(buf, CC.z), .fail);
        const jmp_off = emitJmpRel32(buf);
        patchRel32(buf, jmp_off, loop_off);
        break :blk off;
    } else 0;

    // memo handler (memoize only): mark the entry .fail and continue.
    const memo_handler_off = if (config.memoize) blk: {
        const off = buf.off();
        emitMovRM(buf, rsi_r, t0, 8); // rsi = side_idx (val1)
        emitMovRM(buf, rdi_r, rsp_r, stk_memo_ctx); // rdi = memo_ctx
        emitMovRM(buf, t3, rdi_r, memoOff("helper_ret_fail"));
        emitCallR(buf, t3);
        emitTestRR(buf, bsp, bsp);
        addFixup(fixups, fcount, emitJccRel32(buf, CC.z), .fail);
        const jmp_off = emitJmpRel32(buf);
        patchRel32(buf, jmp_off, loop_off);
        break :blk off;
    } else 0;

    // choice handler
    const choice_handler_off = buf.off();
    emitMovRM(buf, pos, t0, 8);
    emitMovRM(buf, t1, t0, 16);
    emitMovRM(buf, t2, rsp_r, stk_jtp);
    emitMovRMSib8(buf, t1, t2, t1);
    emitMovRM(buf, t2, rsp_r, stk_cbp);
    emitLeaRR(buf, t1, t2, t1);
    emitJmpR(buf, t1);

    patchRel32(buf, choice_rel32, choice_handler_off);
    patchRel32(buf, save_rel32, save_handler_off);
    if (config.capture_events) patchRel32(buf, event_rel32, event_handler_off);
    if (config.memoize) patchRel32(buf, memo_rel32, memo_handler_off);
}

fn emitCharsetCheck(buf: *Buf, charset: u16, negate: bool, fixups: *[8192]Fixup, fcount: *usize) void {
    emitCmpRR(buf, pos, inl);
    addFixup(fixups, fcount, emitJccRel32(buf, CC.ae), .backtrack);
    emitMovzxRM8Sib(buf, t0, inp, pos);
    emitMovRM(buf, rdi_r, rsp_r, stk_csp);
    emitMovRI32(buf, rsi_r, @intCast(charset));
    emitMovRR(buf, t2, t0);
    emitMovRM(buf, t0, rsp_r, stk_hcm);
    emitCallR(buf, t0);
    emitTestRR(buf, t0, t0);
    addFixup(fixups, fcount, emitJccRel32(buf, if (negate) CC.nz else CC.z), .backtrack);
    emitInc(buf, pos);
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
            emitCmpRR(buf, pos, inl);
            addFixup(fixups, fcount, emitJccRel32(buf, CC.ae), .backtrack);
            emitMovzxRM8Sib(buf, t0, inp, pos);
            emitCmp32RI8(buf, t0, inst.data.byte);
            addFixup(fixups, fcount, emitJccRel32(buf, CC.ne), .backtrack);
            emitInc(buf, pos);
        },
        .any => {
            emitCmpRR(buf, pos, inl);
            addFixup(fixups, fcount, emitJccRel32(buf, CC.ae), .backtrack);
            emitInc(buf, pos);
        },
        .optional_char => {
            emitCmpRR(buf, pos, inl);
            const skip1 = emitJccRel32(buf, CC.ae);
            emitMovzxRM8Sib(buf, t0, inp, pos);
            emitCmp32RI8(buf, t0, inst.data.byte);
            const skip2 = emitJccRel32(buf, CC.ne);
            emitInc(buf, pos);
            const skip_off = buf.off();
            patchRel32(buf, skip1, skip_off);
            patchRel32(buf, skip2, skip_off);
        },
        .choice => {
            emitMovRR(buf, t0, bsp);
            emitShlRI(buf, t0, 5);
            emitLeaRR(buf, t0, skp, t0);
            emitMovMI(buf, t0, 0, 0);
            emitMovMR(buf, t0, 8, pos);
            emitMovMI(buf, t0, 16, @intCast(inst.data.offset));
            emitInc(buf, bsp);
        },
        .commit => {
            emitDec(buf, bsp);
            addFixup(fixups, fcount, emitJmpRel32(buf), FixupTarget.bytecodePC(inst.data.offset));
        },
        .fail => {
            addFixup(fixups, fcount, emitJmpRel32(buf), .backtrack);
        },
        .fail_twice => {
            emitDec(buf, bsp);
            addFixup(fixups, fcount, emitJmpRel32(buf), .backtrack);
        },
        .jump => {
            addFixup(fixups, fcount, emitJmpRel32(buf), FixupTarget.bytecodePC(inst.data.offset));
        },
        .call => {
            emitMovRR(buf, t0, bsp);
            emitShlRI(buf, t0, 5);
            emitLeaRR(buf, t0, skp, t0);
            emitMovMI(buf, t0, 0, 1);
            const lea_disp_off = buf.off() + 3;
            emitLeaRip(buf, t1, 0);
            emitMovMR(buf, t0, 8, t1);
            emitInc(buf, bsp);
            addFixup(fixups, fcount, emitJmpRel32(buf), FixupTarget.bytecodePC(inst.data.offset));
            patchRel32(buf, lea_disp_off, buf.off());
        },
        .memo_call => {
            // Mirrors `Vm`'s `memo_call` switch: look up the memo
            // entry, dispatch to fail/success/miss. The miss path
            // builds a side-table frame at depth `bsp`, pushes a
            // tag=5 marker onto the stack, and jumps to the rule
            // body. The eventual `ret` walks down to that marker
            // (the tag dispatches it through `helperMemoRetSuccess`
            // instead of treating it as a regular call return).
            const mc = inst.data.memo;

            // 1. Compute idx = rule_id * stride + pos.
            emitMovRM(buf, t0, rsp_r, stk_memo_ctx); // t0 = MemoCtx*
            emitMovRM(buf, t1, t0, memoOff("stride"));
            emitMovRI32(buf, t2, mc.rule_id); // t2 = rule_id
            emitImulRR(buf, t1, t2); // t1 = stride * rule_id
            emitAddRR(buf, t1, pos); // t1 = idx

            // 2. helperMemoLookup(table, idx, &end_pos_out).
            emitMovRM(buf, rdi_r, t0, memoOff("table_ptr"));
            emitMovRR(buf, rsi_r, t1); // rsi = idx
            emitLeaRMDisp(buf, t2, rsp_r, stk_memo_scratch1); // rdx = &end_pos_out
            emitMovRM(buf, t3, t0, memoOff("helper_lookup"));
            emitCallR(buf, t3);

            // 3. Dispatch on action code (rax = t0).
            emitCmpRI8(buf, t0, 1); // == lookup_fail?
            addFixup(fixups, fcount, emitJccRel32(buf, CC.eq), .backtrack);

            emitCmpRI8(buf, t0, 2); // == lookup_success?
            const success_branch_off = emitJccRel32(buf, CC.eq);

            // `lookup_lr` falls through into the miss path here. Once
            // Warth's left-recursion SETUP-LR path is wired up (not on
            // this backend yet), replace this fallthrough with a
            // SETUP-LR dispatch.

            // ----- Miss path
            // 4a. Get events_len_at_entry into t4.
            if (config.capture_events) {
                emitMovRM(buf, rdi_r, rsp_r, stk_esp);
                emitMovRM(buf, t3, rsp_r, stk_hel);
                emitCallR(buf, t3);
                emitMovRR(buf, t4, t0); // t4 = events_len
            } else {
                emitXorRR32(buf, t4); // t4 = 0
            }

            // 4b. Compute &side[bsp].
            emitMovRM(buf, t0, rsp_r, stk_memo_ctx);
            emitMovRM(buf, t1, t0, memoOff("side_ptr"));
            emitMovRR(buf, t2, bsp);
            emitShlRI(buf, t2, 4);
            emitAddRR(buf, t1, t2); // t1 = &side[bsp]

            // 4c. Write Frame fields.
            emitMovMI32(buf, t1, frameOff("rule_id"), mc.rule_id);
            emitMov32MR(buf, t1, frameOff("start_pos"), pos);
            emitMovMI32(buf, t1, frameOff("return_pc"), @intCast(bc_pc + 1));
            emitMov32MR(buf, t1, frameOff("events_len_at_entry"), t4);

            // 4d. Push tag=5 marker at stack[bsp].
            emitMovRR(buf, t1, bsp);
            emitShlRI(buf, t1, 5);
            emitLeaRR(buf, t1, skp, t1);
            emitMovMI(buf, t1, 0, 5); // tag=5
            emitMovMR(buf, t1, 8, bsp); // val1=bsp
            emitMovMI(buf, t1, 16, 0); // val2=0
            emitMovMI(buf, t1, 24, 0); // event_len=0
            emitInc(buf, bsp);

            // 4e. Jump to rule body.
            addFixup(fixups, fcount, emitJmpRel32(buf), FixupTarget.bytecodePC(mc.offset));

            // ----- Success path
            const success_handler = buf.off();
            patchRel32(buf, success_branch_off, success_handler);

            if (config.capture_events) {
                // Look up cached event range, replay if non-empty.
                emitMovRM(buf, t0, rsp_r, stk_memo_ctx);
                emitMovRM(buf, rdi_r, t0, memoOff("table_ptr"));
                // Recompute idx (regs were clobbered by the lookup call).
                emitMovRM(buf, t1, t0, memoOff("stride"));
                emitMovRI32(buf, t2, mc.rule_id);
                emitImulRR(buf, t1, t2);
                emitAddRR(buf, t1, pos);
                emitMovRR(buf, rsi_r, t1);
                emitLeaRMDisp(buf, t2, rsp_r, stk_memo_scratch1); // rdx = &cached_start
                emitMovRM(buf, t3, t0, memoOff("helper_cached_slice"));
                emitCallR(buf, t3);
                // rax = count.
                emitMovRR(buf, t4, t0); // preserve count

                // If count == 0 skip replay.
                emitTestRR(buf, t4, t4);
                const skip_replay_off = emitJccRel32(buf, CC.eq);

                // helperMemoReplayEvents(state, events_buf, start, count, stack, &sp, captures).
                // 7 args -> 6 in registers + 1 on the stack. We sub
                // rsp by 16 first so the outgoing-arg area sits below
                // our own locals (preserving stk_csp/stk_sdp at the
                // bottom of the locals frame). The 16-byte adjustment
                // also keeps rsp ≡ 0 mod 16 across the inner CALL.
                emitMovRM(buf, t0, rsp_r, stk_memo_ctx);
                emitMovRM(buf, rdi_r, rsp_r, stk_esp); // arg1 = state
                emitMovRM(buf, rsi_r, t0, memoOff("events_buf_ptr"));
                emitMovRM(buf, t2, rsp_r, stk_memo_scratch1); // arg3 = start (rdx)
                emitMovRR(buf, t1, t4); // arg4 = count (rcx)
                emitMovRR(buf, r8_r, skp); // arg5 = stack
                emitMovMR(buf, rsp_r, stk_memo_scratch2, bsp); // stash bsp
                emitLeaRMDisp(buf, r9_r, rsp_r, stk_memo_scratch2); // arg6 = &sp
                emitMovRM(buf, t3, t0, memoOff("helper_replay_events"));
                // Make outgoing-arg area below our locals; arg7 lives
                // at [new_rsp + 0] = [old_rsp - 16].
                emitSubRI(buf, rsp_r, 16);
                emitMovMR(buf, rsp_r, 0, cap); // [rsp+0] = captures
                emitCallR(buf, t3);
                emitAddRI(buf, rsp_r, 16);
                // rax = 0 ok or oom_sentinel.
                emitCmpRI8(buf, t0, -1);
                addFixup(fixups, fcount, emitJccRel32(buf, CC.eq), .fail);
                emitMovRM(buf, bsp, rsp_r, stk_memo_scratch2); // reload bsp

                patchRel32(buf, skip_replay_off, buf.off());
            }

            // Advance pos to the cached end position and fall through.
            emitMovRM(buf, pos, rsp_r, stk_memo_scratch1);
        },
        .ret => {
            if (config.capture_events or config.memoize) {
                // Find the call's ret frame (tag=1) -- or, when memoize
                // is on, the matching memo frame (tag=5) -- by walking
                // down from sp-1, stash the native return address, then
                // shift any save / event / lcatch frames above it down
                // by one so the outer backtrack can still undo them.
                emitMovRR(buf, t1, bsp);
                emitDec(buf, t1); // t1 = sp - 1 (top frame index)

                const find_loop_off = buf.off();
                emitMovRR(buf, t0, t1);
                emitShlRI(buf, t0, 5);
                emitLeaRR(buf, t0, skp, t0); // t0 = &stack[t1]
                emitMovRM(buf, t2, t0, 0); // t2 = tag
                emitCmp32RI8(buf, t2, 1);
                const found_rel32 = emitJccRel32(buf, CC.eq);
                const found_memo_branch_off = if (config.memoize) blk: {
                    emitCmp32RI8(buf, t2, 5);
                    break :blk emitJccRel32(buf, CC.eq);
                } else 0;
                emitDec(buf, t1);
                {
                    const back = emitJmpRel32(buf);
                    patchRel32(buf, back, find_loop_off);
                }

                patchRel32(buf, found_rel32, buf.off());
                // t0 -> ret frame, t1 = ret_idx. Stash ret addr in t3
                // so it survives the shift loop.
                emitMovRM(buf, t3, t0, 8);

                const skip_memo_block_off = if (config.memoize) emitJmpRel32(buf) else 0;

                if (config.memoize) {
                    patchRel32(buf, found_memo_branch_off, buf.off());
                    // Stash t1 (= the matched-frame index from
                    // find_loop) before the call clobbers caller-saved
                    // regs; the shift loop below depends on it.
                    emitMovMR(buf, rsp_r, stk_call_scratch, t1);
                    // helperMemoRetSuccess(memo_ctx, side_idx, end_pos,
                    // state_ptr, jump_table, code_base) -> native_addr
                    // (or oom_sentinel on OOM). 6 args, all in regs.
                    emitMovRM(buf, rsi_r, t0, 8); // arg2 = side_idx (val1)
                    emitMovRM(buf, rdi_r, rsp_r, stk_memo_ctx); // arg1 = memo_ctx
                    emitMovRR(buf, t2, pos); // arg3 = end_pos (rdx)
                    if (config.capture_events) {
                        emitMovRM(buf, t1, rsp_r, stk_esp); // arg4 = state (rcx)
                    } else {
                        emitXorRR32(buf, t1); // arg4 = 0
                    }
                    emitMovRM(buf, r8_r, rsp_r, stk_jtp); // arg5 = jump_table
                    emitMovRM(buf, r9_r, rsp_r, stk_cbp); // arg6 = code_base
                    emitMovRM(buf, t3, rdi_r, memoOff("helper_ret_success"));
                    emitCallR(buf, t3);
                    // rax = native target or oom_sentinel.
                    emitCmpRI8(buf, t0, -1);
                    addFixup(fixups, fcount, emitJccRel32(buf, CC.eq), .fail);
                    emitMovRR(buf, t3, t0); // t3 = native target
                    emitMovRM(buf, t1, rsp_r, stk_call_scratch); // restore t1

                    patchRel32(buf, skip_memo_block_off, buf.off());
                }

                const shift_loop_off = buf.off();
                emitMovRR(buf, t2, t1);
                emitInc(buf, t2);
                emitCmpRR(buf, t2, bsp);
                const shift_done_rel32 = emitJccRel32(buf, CC.ae);

                emitMovRR(buf, t0, t1);
                emitShlRI(buf, t0, 5);
                emitLeaRR(buf, t0, skp, t0); // dest = &stack[t1]

                // Copy 32 bytes (4 qwords) from [t0+32] to [t0].
                emitMovRM(buf, t4, t0, 32);
                emitMovMR(buf, t0, 0, t4);
                emitMovRM(buf, t4, t0, 40);
                emitMovMR(buf, t0, 8, t4);
                emitMovRM(buf, t4, t0, 48);
                emitMovMR(buf, t0, 16, t4);
                emitMovRM(buf, t4, t0, 56);
                emitMovMR(buf, t0, 24, t4);

                emitInc(buf, t1);
                {
                    const back = emitJmpRel32(buf);
                    patchRel32(buf, back, shift_loop_off);
                }

                patchRel32(buf, shift_done_rel32, buf.off());
                emitDec(buf, bsp);
                emitJmpR(buf, t3);
            } else {
                emitDec(buf, bsp);
                emitMovRR(buf, t0, bsp);
                emitShlRI(buf, t0, 5);
                emitLeaRR(buf, t0, skp, t0);
                emitMovRM(buf, t0, t0, 8);
                emitJmpR(buf, t0);
            }
        },
        .save => {
            const slot: u12 = @intCast(inst.data.slot);
            if (config.capture_events) {
                // Append the event first; the helper returns the
                // pre-append length, which we stash in the stack entry
                // so the backtrack path can truncate in lockstep with
                // the capture-slot undo. OOM surfaces as the sentinel
                // maxInt(u64) and routes us to the fail handler.
                emitMovRM(buf, rdi_r, rsp_r, stk_esp); // rdi = state_ptr
                emitMovRI32(buf, rsi_r, @intCast(slot)); // rsi = slot
                emitMovRR(buf, t2, pos); // rdx = pos (t2 is rdx)
                emitMovRM(buf, t3, rsp_r, stk_has); // t3 = helper_append_save
                emitCallR(buf, t3);
                // rax (t0) = pre_len or maxInt(u64) on OOM.
                emitCmpRI8(buf, t0, -1);
                addFixup(fixups, fcount, emitJccRel32(buf, CC.eq), .fail);
                // Build stack entry. t1 = slot, t2 = old cap[slot],
                // t3 = &stack_entry, t0 still holds pre_len.
                emitMovRI32(buf, t1, @intCast(slot));
                emitMovRMSib8(buf, t2, cap, t1);
                emitMovRR(buf, t3, bsp);
                emitShlRI(buf, t3, 5);
                emitLeaRR(buf, t3, skp, t3);
                emitMovMI(buf, t3, 0, 2);
                emitMovMR(buf, t3, 8, t1);
                emitMovMR(buf, t3, 16, t2);
                emitMovMR(buf, t3, 24, t0);
                emitInc(buf, bsp);
                emitMovMRSib8(buf, cap, t1, pos);
            } else {
                emitMovRI32(buf, t0, @intCast(slot));
                emitMovRMSib8(buf, t1, cap, t0);
                emitMovRR(buf, t2, bsp);
                emitShlRI(buf, t2, 5);
                emitLeaRR(buf, t2, skp, t2);
                emitMovMI(buf, t2, 0, 2);
                emitMovMR(buf, t2, 8, t0);
                emitMovMR(buf, t2, 16, t1);
                emitInc(buf, bsp);
                emitMovMRSib8(buf, cap, t0, pos);
            }
        },
        .match => {
            emitMovRR(buf, t0, pos);
            addFixup(fixups, fcount, emitJmpRel32(buf), .success);
        },
        .string => {
            const ref = inst.data.string;
            emitMovRR(buf, rdi_r, inp);
            emitMovRR(buf, rsi_r, inl);
            emitMovRR(buf, t2, pos);
            emitMovRM(buf, t1, rsp_r, stk_sdp);
            emitMovRI32(buf, r8_r, ref.offset);
            emitMovRI32(buf, r9_r, @intCast(ref.len));
            emitMovRM(buf, t0, rsp_r, stk_hsm);
            emitCallR(buf, t0);
            emitTestRR(buf, t0, t0);
            addFixup(fixups, fcount, emitJccRel32(buf, CC.z), .backtrack);
            emitAddRI(buf, pos, @intCast(ref.len));
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
                emitMovRM(buf, rdi_r, rsp_r, stk_esp);
                emitMovRI32(buf, rsi_r, @intCast(slot));
                emitMovRR(buf, t2, pos);
                emitMovRM(buf, t3, rsp_r, stk_has);
                emitCallR(buf, t3);
                // OOM check
                emitCmpRI8(buf, t0, -1);
                addFixup(fixups, fcount, emitJccRel32(buf, CC.eq), .fail);
                // Build stack entry: tag=3 (event), event_len = pre_len.
                emitMovRR(buf, t1, bsp);
                emitShlRI(buf, t1, 5);
                emitLeaRR(buf, t1, skp, t1);
                emitMovMI(buf, t1, 0, 3);
                emitMovMR(buf, t1, 24, t0);
                emitInc(buf, bsp);
            }
            // capture_events off: no-op (tree mode requires events on).
        },
        .event_token => {
            if (config.capture_events) {
                const len: u8 = inst.data.byte;
                // Call helper_append_token(state, start, end) where
                // start = pos - len and end = pos.
                emitMovRM(buf, rdi_r, rsp_r, stk_esp);
                emitMovRR(buf, rsi_r, pos);
                if (len > 0) emitSubRI(buf, rsi_r, @intCast(len));
                emitMovRR(buf, t2, pos); // t2 is rdx
                emitMovRM(buf, t3, rsp_r, stk_hat);
                emitCallR(buf, t3);
                emitCmpRI8(buf, t0, -1);
                addFixup(fixups, fcount, emitJccRel32(buf, CC.eq), .fail);
                // Build event stack entry: tag=3, event_len = pre_len.
                emitMovRR(buf, t1, bsp);
                emitShlRI(buf, t1, 5);
                emitLeaRR(buf, t1, skp, t1);
                emitMovMI(buf, t1, 0, 3);
                emitMovMR(buf, t1, 24, t0);
                emitInc(buf, bsp);
            }
            // capture_events off: no-op (mirrors VM behavior).
        },
        .event_field => {
            if (config.capture_events) {
                const field_id: u16 = inst.data.slot;
                emitMovRM(buf, rdi_r, rsp_r, stk_esp);
                emitMovRI32(buf, rsi_r, @intCast(field_id));
                emitMovRR(buf, t2, pos);
                emitMovRM(buf, t3, rsp_r, stk_haf);
                emitCallR(buf, t3);
                emitCmpRI8(buf, t0, -1);
                addFixup(fixups, fcount, emitJccRel32(buf, CC.eq), .fail);
                emitMovRR(buf, t1, bsp);
                emitShlRI(buf, t1, 5);
                emitLeaRR(buf, t1, skp, t1);
                emitMovMI(buf, t1, 0, 3);
                emitMovMR(buf, t1, 24, t0);
                emitInc(buf, bsp);
            }
            // capture_events off: no-op (mirrors VM behavior).
        },
        .event_error_open, .event_error_close, .event_missing => {
            if (config.capture_events) {
                const label: u16 = inst.data.slot;
                const helper_slot: i32 = switch (inst.op) {
                    .event_error_open => stk_haeo,
                    .event_error_close => stk_haec,
                    .event_missing => stk_haem,
                    else => unreachable,
                };
                emitMovRM(buf, rdi_r, rsp_r, stk_esp);
                emitMovRI32(buf, rsi_r, @intCast(label));
                emitMovRR(buf, t2, pos);
                emitMovRM(buf, t3, rsp_r, helper_slot);
                emitCallR(buf, t3);
                emitCmpRI8(buf, t0, -1);
                addFixup(fixups, fcount, emitJccRel32(buf, CC.eq), .fail);
                emitMovRR(buf, t1, bsp);
                emitShlRI(buf, t1, 5);
                emitLeaRR(buf, t1, skp, t1);
                emitMovMI(buf, t1, 0, 3);
                emitMovMR(buf, t1, 24, t0);
                emitInc(buf, bsp);
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
            // rdi = state_ptr; call helper_events_len.
            emitMovRM(buf, rdi_r, rsp_r, stk_esp);
            emitMovRM(buf, t3, rsp_r, stk_hel);
            emitCallR(buf, t3);
            // rax (t0) = events length. Build the stack entry; t1 holds
            // the &stack_entry, t0 still holds events_len for the final
            // store.
            emitMovRR(buf, t1, bsp);
            emitShlRI(buf, t1, 5);
            emitLeaRR(buf, t1, skp, t1);
            emitMovMI(buf, t1, 0, 4); // tag=4 (lcatch)
            emitMovMI(buf, t1, 8, @intCast(ch.label));
            emitMovMI(buf, t1, 16, @intCast(ch.handler_pc));
            emitMovMR(buf, t1, 24, t0); // event_len = events_len
            emitInc(buf, bsp);
        },
        .throw => {
            // Call helperThrow(state, stack, &sp, label, throw_pos).
            // The helper modifies *sp via the pointer, then returns
            // either the handler bytecode PC (jump via the existing
            // jump table) or the `throw_miss` sentinel (route to fail).
            const label: u16 = inst.data.slot;

            // Stash current bsp at stk_call_scratch so the helper sees
            // the right depth and we can read back the new depth.
            emitMovMR(buf, rsp_r, stk_call_scratch, bsp);

            emitMovRM(buf, rdi_r, rsp_r, stk_esp); // arg1 = state_ptr
            emitMovRR(buf, rsi_r, skp); // arg2 = stack_ptr
            emitLeaRMDisp(buf, t2, rsp_r, stk_call_scratch); // arg3 = &sp
            emitMovRI32(buf, t1, @intCast(label)); // arg4 = label (rcx)
            emitMovRR(buf, r8_r, pos); // arg5 = throw_pos

            emitMovRM(buf, t3, rsp_r, stk_ht);
            emitCallR(buf, t3);

            // Reload bsp from the scratch slot (helper updated it).
            emitMovRM(buf, bsp, rsp_r, stk_call_scratch);

            // Check throw_miss sentinel (= -1).
            emitCmpRI8(buf, t0, -1);
            addFixup(fixups, fcount, emitJccRel32(buf, CC.eq), .fail);

            // rax = handler bytecode PC. Index into jump_table to get
            // the native code offset, add code_base, jmp.
            emitMovRM(buf, t1, rsp_r, stk_jtp);
            emitMovRMSib8(buf, t0, t1, t0); // rax = jt[bc_pc] (scale=8)
            emitMovRM(buf, t1, rsp_r, stk_cbp);
            emitLeaRR(buf, t0, t1, t0); // rax = cbp + offset
            emitJmpR(buf, t0);
        },
    }
}
