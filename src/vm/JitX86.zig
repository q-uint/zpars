/// x86_64 backend for the JIT compiler.
///
/// Encodes native x86_64 machine code from bytecode instructions.
const std = @import("std");
const I = @import("Instruction.zig");
const Jit = @import("Jit.zig");

const page_size = Jit.page_size;
const StackEntry = Jit.StackEntry;

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

// Stack frame layout
const stk_csp: i32 = 0;
const stk_sdp: i32 = 8;
const stk_jtp: i32 = 16;
const stk_cbp: i32 = 24;
const stk_hsm: i32 = 32; // helper_string_match
const stk_hcm: i32 = 40; // helper_charset_match
const stk_size: i32 = 48;

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

pub fn estimateSize(code_len: usize) usize {
    return (code_len + 1) * 128 + 4096;
}

pub fn generate(code: []const I.Inst, output: [*]u8) GenerateResult {
    var buf = Buf{ .ptr = output, .len = 0 };
    var fixups: [8192]Fixup = undefined;
    var fcount: usize = 0;
    var bc_map: [4096]u32 = undefined;

    emitPrologue(&buf);

    for (code, 0..) |inst, i| {
        bc_map[i] = buf.off();
        emitInst(&buf, inst, &fixups, &fcount);
    }
    if (code.len < 4096)
        bc_map[code.len] = buf.off();

    const bt_off = buf.off();
    emitBacktrackHandler(&buf, &fixups, &fcount);

    const fail_off = buf.off();
    emitMovRI64(&buf, t0, Jit.null_cap);

    const succ_off = buf.off();
    emitEpilogue(&buf);

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

pub fn compile(self: *Jit) !void {
    const est = estimateSize(self.code.len);
    const size = std.mem.alignForward(usize, est, page_size);

    self.native_code = try std.posix.mmap(
        null,
        size,
        std.c.PROT.READ | std.c.PROT.WRITE,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    );

    const result = generate(self.code, self.native_code.ptr);
    self.native_len = result.native_len;
    self.jump_table = result.jump_table;

    try std.posix.mprotect(
        @alignCast(self.native_code[0..size]),
        std.c.PROT.READ | std.c.PROT.EXEC,
    );
}

fn emitPrologue(buf: *Buf) void {
    emitPush(buf, pos); // rbx
    emitPush(buf, bsp); // rbp
    emitPush(buf, cap); // r12
    emitPush(buf, inl); // r13
    emitPush(buf, inp); // r14
    emitPush(buf, skp); // r15

    emitSubRI(buf, rsp_r, stk_size);

    // rdi = pointer to JitCtx. Load fields into regs and stack slots.
    emitMovRM(buf, inp, rdi_r, 0); // r14 = ctx->input_ptr
    emitMovRM(buf, inl, rdi_r, 8); // r13 = ctx->input_len
    emitMovRM(buf, t0, rdi_r, 16);
    emitMovMR(buf, rsp_r, stk_csp, t0); // [rsp+0] = charsets_ptr
    emitMovRM(buf, t0, rdi_r, 24);
    emitMovMR(buf, rsp_r, stk_sdp, t0); // [rsp+8] = string_data_ptr
    emitMovRM(buf, cap, rdi_r, 32); // r12 = ctx->captures_ptr
    emitMovRM(buf, skp, rdi_r, 40); // r15 = ctx->stack_ptr
    emitMovRM(buf, t0, rdi_r, 48);
    emitMovMR(buf, rsp_r, stk_jtp, t0); // [rsp+16] = jump_table_ptr
    emitMovRM(buf, t0, rdi_r, 56);
    emitMovMR(buf, rsp_r, stk_cbp, t0); // [rsp+24] = code_base_ptr
    emitMovRM(buf, t0, rdi_r, 64);
    emitMovMR(buf, rsp_r, stk_hsm, t0); // [rsp+32] = helper_string_match
    emitMovRM(buf, t0, rdi_r, 72);
    emitMovMR(buf, rsp_r, stk_hcm, t0); // [rsp+40] = helper_charset_match

    emitXorRR32(buf, pos);
    emitXorRR32(buf, bsp);
}

fn emitEpilogue(buf: *Buf) void {
    emitAddRI(buf, rsp_r, stk_size);
    emitPop(buf, skp); // r15
    emitPop(buf, inp); // r14
    emitPop(buf, inl); // r13
    emitPop(buf, cap); // r12
    emitPop(buf, bsp); // rbp
    emitPop(buf, pos); // rbx
    emitRetInst(buf);
}

fn emitBacktrackHandler(buf: *Buf, fixups: *[8192]Fixup, fcount: *usize) void {
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

    // tag == 1 (ret): skip, continue loop
    emitTestRR(buf, bsp, bsp);
    addFixup(fixups, fcount, emitJccRel32(buf, CC.z), .fail);
    {
        const jmp_off = emitJmpRel32(buf);
        patchRel32(buf, jmp_off, loop_off);
    }

    // save handler
    const save_handler_off = buf.off();
    emitMovRM(buf, t2, t0, 8);
    emitMovRM(buf, t1, t0, 16);
    emitMovMRSib8(buf, cap, t2, t1);
    emitTestRR(buf, bsp, bsp);
    addFixup(fixups, fcount, emitJccRel32(buf, CC.z), .fail);
    {
        const jmp_off = emitJmpRel32(buf);
        patchRel32(buf, jmp_off, loop_off);
    }

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
    buf: *Buf,
    inst: I.Inst,
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
        .ret => {
            emitDec(buf, bsp);
            emitMovRR(buf, t0, bsp);
            emitShlRI(buf, t0, 5);
            emitLeaRR(buf, t0, skp, t0);
            emitMovRM(buf, t0, t0, 8);
            emitJmpR(buf, t0);
        },
        .save => {
            const slot: u12 = @intCast(inst.data.slot);
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
    }
}
