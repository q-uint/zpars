/// AArch64 backend for the JIT compiler.
///
/// Encodes native AArch64 machine code from bytecode instructions.
const std = @import("std");
const I = @import("Instruction.zig");
const Jit = @import("Jit.zig");

const page_size = Jit.page_size;
const StackEntry = Jit.StackEntry;

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

pub fn estimateSize(code_len: usize) usize {
    return (code_len + 1) * 80 + 2048;
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
    buf.emit(encMovn(0, 0)); // MOV x0, #-1 (all ones)

    const succ_off = buf.off();
    emitEpilogue(&buf);

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
    buf.emit(encStpPre(29, 30, sp_reg, -16));
    buf.emit(encStpPre(pos, bsp, sp_reg, -16));
    buf.emit(encStpPre(inp, inl, sp_reg, -16));
    buf.emit(encStpPre(csp, sdp, sp_reg, -16));
    buf.emit(encStpPre(cap, skp, sp_reg, -16));
    buf.emit(encStpPre(jtp, cbp, sp_reg, -16));
    // Allocate 16 bytes for helper function pointers.
    buf.emit(encSub(sp_reg, sp_reg, 16));

    // x0 = pointer to JitCtx. Load fields into callee-saved regs.
    buf.emit(encLdr(inp, 0, 0)); // x21 = ctx->input_ptr
    buf.emit(encLdr(inl, 0, 8)); // x22 = ctx->input_len
    buf.emit(encLdr(csp, 0, 16)); // x23 = ctx->charsets_ptr
    buf.emit(encLdr(sdp, 0, 24)); // x24 = ctx->string_data_ptr
    buf.emit(encLdr(cap, 0, 32)); // x25 = ctx->captures_ptr
    buf.emit(encLdr(skp, 0, 40)); // x26 = ctx->stack_ptr
    buf.emit(encLdr(jtp, 0, 48)); // x27 = ctx->jump_table_ptr
    buf.emit(encLdr(cbp, 0, 56)); // x28 = ctx->code_base_ptr
    // Store helper function pointers on the stack.
    buf.emit(encLdr(t0, 0, 64)); // helper_string_match
    buf.emit(encStr(t0, sp_reg, 0)); // [sp+0]
    buf.emit(encLdr(t0, 0, 72)); // helper_charset_match
    buf.emit(encStr(t0, sp_reg, 8)); // [sp+8]

    buf.emit(encMovz(pos, 0, 0));
    buf.emit(encMovz(bsp, 0, 0));
}

fn emitEpilogue(buf: *Buf) void {
    buf.emit(encAdd(sp_reg, sp_reg, 16)); // deallocate helper slots
    buf.emit(encLdpPost(jtp, cbp, sp_reg, 16));
    buf.emit(encLdpPost(cap, skp, sp_reg, 16));
    buf.emit(encLdpPost(csp, sdp, sp_reg, 16));
    buf.emit(encLdpPost(inp, inl, sp_reg, 16));
    buf.emit(encLdpPost(pos, bsp, sp_reg, 16));
    buf.emit(encLdpPost(29, 30, sp_reg, 16));
    buf.emit(encRet());
}

fn emitBacktrackHandler(buf: *Buf, fixups: *[8192]Fixup, fcount: *usize) void {
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

    // tag == 1 (ret): skip, continue loop
    addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, bsp);
    buf.emit(encNop());
    buf.emit(encB(@intCast(@as(i32, @intCast(loop_off)) - @as(i32, @intCast(buf.off())))));

    // save handler
    const save_handler = buf.off();
    buf.emit(encLdr(t3, t1, 8));
    buf.emit(encLdr(t4, t1, 16));
    buf.emit(encStrReg(t4, cap, t3));
    addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, bsp);
    buf.emit(encNop());
    buf.emit(encB(@intCast(@as(i32, @intCast(loop_off)) - @as(i32, @intCast(buf.off())))));

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
}

fn emitCharsetCheck(buf: *Buf, charset: u16, negate: bool, fixups: *[8192]Fixup, fcount: *usize) void {
    buf.emit(encCmpReg(pos, inl));
    addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.hs, 0);
    buf.emit(encNop());
    buf.emit(encLdrbReg(t0, inp, pos));
    buf.emit(encMov(0, csp));
    buf.emit(encMovz(1, @intCast(charset), 0));
    buf.emit(encMov(2, t0));
    buf.emit(encLdr(t0, sp_reg, 8)); // helper_charset_match from stack
    buf.emit(encBlr(t0));
    addFixup(fixups, fcount, buf.off(), .backtrack, if (negate) .cbnz else .cbz, 0, 0);
    buf.emit(encNop());
    buf.emit(encAdd(pos, pos, 1));
}

fn emitInst(
    buf: *Buf,
    inst: I.Inst,
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
            buf.emit(encSub(bsp, bsp, 1));
            addFixup(fixups, fcount, buf.off(), FixupTarget.bytecodePC(inst.data.offset), .b, 0, 0);
            buf.emit(encNop());
        },
        .fail => {
            addFixup(fixups, fcount, buf.off(), .backtrack, .b, 0, 0);
            buf.emit(encNop());
        },
        .fail_twice => {
            buf.emit(encSub(bsp, bsp, 1));
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
        .ret => {
            buf.emit(encSub(bsp, bsp, 1));
            buf.emit(encLsl(t1, bsp, 5));
            buf.emit(encAddReg(t1, skp, t1));
            buf.emit(encLdr(t2, t1, 8));
            buf.emit(encBr(t2));
        },
        .save => {
            const slot: u12 = @intCast(inst.data.slot);
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
            buf.emit(encLdr(t0, sp_reg, 0)); // helper_string_match from stack
            buf.emit(encBlr(t0));
            addFixup(fixups, fcount, buf.off(), .backtrack, .cbz, 0, 0);
            buf.emit(encNop());
            buf.emit(encAdd(pos, pos, @intCast(ref.len)));
        },
        .set => emitCharsetCheck(buf, inst.data.charset, false, fixups, fcount),
        .neg_set => emitCharsetCheck(buf, inst.data.charset, true, fixups, fcount),
    }
}
