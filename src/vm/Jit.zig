/// AArch64 JIT compiler for the grammar parsing VM.
///
/// Translates bytecode produced by the Compiler into native AArch64
/// machine code, eliminating the interpreter dispatch overhead.
/// Optional alternative to the interpreter VM -- same API surface.
const std = @import("std");
const I = @import("Instruction.zig");
const Vm = @import("Vm.zig");

const Jit = @This();

const max_stack = 1024;
const max_captures = 64;
const null_cap = std.math.maxInt(u64);
const page_size = std.heap.page_size_min;

// -- Register assignments (callee-saved x19-x28) --
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

// -- Backtrack stack entry (32 bytes for LSL #5 addressing) --
const StackEntry = extern struct {
    tag: u64, // 0=choice, 1=ret, 2=save
    val1: u64,
    val2: u64,
    _pad: u64 = 0,
};

comptime {
    if (@sizeOf(StackEntry) != 32) @compileError("StackEntry must be 32 bytes");
}

// Context struct passed to the JIT function (8 fields x 8 bytes = 64 bytes).
const JitCtx = extern struct {
    input_ptr: u64,
    input_len: u64,
    charsets_ptr: u64,
    string_data_ptr: u64,
    captures_ptr: u64,
    stack_ptr: u64,
    jump_table_ptr: u64,
    code_base_ptr: u64,
};

// -- Jit fields --

code: []const I.Inst,
charsets: []const I.Charset,
string_data: []const u8,
input: []const u8,
native_code: []align(page_size) u8,
native_len: usize,
jump_table: [4096]u64,
captures_buf: [max_captures]u64,
stack_buf: [max_stack]StackEntry,

// -- Public API --

pub fn init(
    code: []const I.Inst,
    charsets: []const I.Charset,
    string_data: []const u8,
    input: []const u8,
) !Jit {
    var self = Jit{
        .code = code,
        .charsets = charsets,
        .string_data = string_data,
        .input = input,
        .native_code = undefined,
        .native_len = 0,
        .jump_table = [_]u64{0} ** 4096,
        .captures_buf = [_]u64{null_cap} ** max_captures,
        .stack_buf = undefined,
    };
    try self.compile();
    return self;
}

pub fn deinit(self: *Jit) void {
    std.posix.munmap(self.native_code);
}

pub fn execute(self: *Jit) ?usize {
    @memset(&self.captures_buf, null_cap);

    const ctx = JitCtx{
        .input_ptr = @intFromPtr(self.input.ptr),
        .input_len = self.input.len,
        .charsets_ptr = @intFromPtr(self.charsets.ptr),
        .string_data_ptr = @intFromPtr(self.string_data.ptr),
        .captures_ptr = @intFromPtr(&self.captures_buf),
        .stack_ptr = @intFromPtr(&self.stack_buf),
        .jump_table_ptr = @intFromPtr(&self.jump_table),
        .code_base_ptr = @intFromPtr(self.native_code.ptr),
    };

    const jit_fn: *const fn (*const JitCtx) callconv(.c) u64 =
        @ptrCast(self.native_code.ptr);
    const result = jit_fn(&ctx);

    if (result == null_cap) return null;
    return @intCast(result);
}

pub fn getCapture(self: *const Jit, i: u16) ?Vm.Span {
    const s = self.captures_buf[i * 2];
    if (s == null_cap) return null;
    const e = self.captures_buf[i * 2 + 1];
    if (e == null_cap) return null;
    return .{ .start = @intCast(s), .end = @intCast(e) };
}

pub fn getCaptureSlice(self: *const Jit, i: u16) ?[]const u8 {
    const span = self.getCapture(i) orelse return null;
    return self.input[span.start..span.end];
}

// ============================================================
// AArch64 instruction encoding helpers
// ============================================================

/// B (unconditional branch), PC-relative byte offset.
fn encB(off: i28) u32 {
    const imm: u26 = @truncate(asU32(off >> 2));
    return (0b000101 << 26) | @as(u32, imm);
}

/// B.cond (conditional branch), PC-relative byte offset.
fn encBCond(cond: u4, off: i21) u32 {
    const imm: u19 = @truncate(asU32(@as(i32, off) >> 2));
    return (0b01010100 << 24) | (@as(u32, imm) << 5) | cond;
}

/// CBZ Xt, offset (branch if zero, 64-bit).
fn encCbz(rt: Reg, off: i21) u32 {
    const imm: u19 = @truncate(asU32(@as(i32, off) >> 2));
    return 0xB4000000 | (@as(u32, imm) << 5) | rt;
}

/// CBNZ Xt, offset (branch if not zero, 64-bit).
fn encCbnz(rt: Reg, off: i21) u32 {
    const imm: u19 = @truncate(asU32(@as(i32, off) >> 2));
    return 0xB5000000 | (@as(u32, imm) << 5) | rt;
}

/// MOVZ Xd, #imm16, LSL #(hw*16).
fn encMovz(rd: Reg, imm16: u16, hw: u2) u32 {
    return 0xD2800000 | (@as(u32, hw) << 21) | (@as(u32, imm16) << 5) | rd;
}

/// MOVK Xd, #imm16, LSL #(hw*16).
fn encMovk(rd: Reg, imm16: u16, hw: u2) u32 {
    return 0xF2800000 | (@as(u32, hw) << 21) | (@as(u32, imm16) << 5) | rd;
}

/// MOVN Xd, #imm16 (move NOT). MOVN Xd,#0 -> Xd = 0xFFFF_FFFF_FFFF_FFFF.
fn encMovn(rd: Reg, imm16: u16) u32 {
    return 0x92800000 | (@as(u32, imm16) << 5) | rd;
}

/// MOV Xd, Xm  (alias for ORR Xd, XZR, Xm).
fn encMov(rd: Reg, rm: Reg) u32 {
    return 0xAA0003E0 | (@as(u32, rm) << 16) | rd;
}

/// ADD Xd, Xn, #imm12.
fn encAdd(rd: Reg, rn: Reg, imm12: u12) u32 {
    return 0x91000000 | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5) | rd;
}

/// SUB Xd, Xn, #imm12.
fn encSub(rd: Reg, rn: Reg, imm12: u12) u32 {
    return 0xD1000000 | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5) | rd;
}

/// CMP Xn, #imm12  (SUBS XZR, Xn, #imm12).
fn encCmpImm(rn: Reg, imm12: u12) u32 {
    return 0xF100001F | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5);
}

/// CMP Wn, #imm12  (32-bit).
fn encCmpImm32(rn: Reg, imm12: u12) u32 {
    return 0x7100001F | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5);
}

/// CMP Xn, Xm  (SUBS XZR, Xn, Xm).
fn encCmpReg(rn: Reg, rm: Reg) u32 {
    return 0xEB00001F | (@as(u32, rm) << 16) | (@as(u32, rn) << 5);
}

/// LDRB Wt, [Xn, Xm]  (byte load, register offset).
fn encLdrbReg(rt: Reg, rn: Reg, rm: Reg) u32 {
    return 0x38606800 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rt;
}

/// LDR Xt, [Xn, #imm]  (64-bit load, unsigned offset in bytes, must be 8-aligned).
fn encLdr(rt: Reg, rn: Reg, imm_bytes: u15) u32 {
    return 0xF9400000 | (@as(u32, imm_bytes / 8) << 10) | (@as(u32, rn) << 5) | rt;
}

/// STR Xt, [Xn, #imm]  (64-bit store, unsigned offset in bytes, must be 8-aligned).
fn encStr(rt: Reg, rn: Reg, imm_bytes: u15) u32 {
    return 0xF9000000 | (@as(u32, imm_bytes / 8) << 10) | (@as(u32, rn) << 5) | rt;
}

/// LDR Xt, [Xn, Xm, LSL #3]  (64-bit load, register offset scaled by 8).
fn encLdrReg(rt: Reg, rn: Reg, rm: Reg) u32 {
    return 0xF8607800 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rt;
}

/// STR Xt, [Xn, Xm, LSL #3]  (64-bit store, register offset scaled by 8).
fn encStrReg(rt: Reg, rn: Reg, rm: Reg) u32 {
    return 0xF8207800 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rt;
}

/// STP Xt1, Xt2, [Xn, #imm]!  (pre-index, signed offset, must be 8-aligned).
fn encStpPre(rt1: Reg, rt2: Reg, rn: Reg, imm: i9) u32 {
    const imm7: u7 = @truncate(asU32(@divTrunc(imm, 8)));
    return 0xA9800000 | (@as(u32, imm7) << 15) | (@as(u32, rt2) << 10) |
        (@as(u32, rn) << 5) | rt1;
}

/// LDP Xt1, Xt2, [Xn], #imm  (post-index, signed offset, must be 8-aligned).
fn encLdpPost(rt1: Reg, rt2: Reg, rn: Reg, imm: i9) u32 {
    const imm7: u7 = @truncate(asU32(@divTrunc(imm, 8)));
    return 0xA8C00000 | (@as(u32, imm7) << 15) | (@as(u32, rt2) << 10) |
        (@as(u32, rn) << 5) | rt1;
}

/// ADR Xd, PC + offset_bytes.
fn encAdr(rd: Reg, off: i21) u32 {
    const v: u21 = @bitCast(off);
    const immlo: u2 = @truncate(v);
    const immhi: u19 = @truncate(v >> 2);
    return (@as(u32, immlo) << 29) | 0x10000000 | (@as(u32, immhi) << 5) | rd;
}

/// LSL Xd, Xn, #amount  (via UBFM).
fn encLsl(rd: Reg, rn: Reg, amount: u6) u32 {
    const immr: u6 = 0 -% amount;
    const imms: u6 = 63 - amount;
    return 0xD3400000 | (@as(u32, immr) << 16) | (@as(u32, imms) << 10) |
        (@as(u32, rn) << 5) | rd;
}

/// BR Xn  (branch to register).
fn encBr(rn: Reg) u32 {
    return 0xD61F0000 | (@as(u32, rn) << 5);
}

/// BLR Xn  (branch with link to register).
fn encBlr(rn: Reg) u32 {
    return 0xD63F0000 | (@as(u32, rn) << 5);
}

/// RET (return to x30).
fn encRet() u32 {
    return 0xD65F03C0;
}

/// NOP.
fn encNop() u32 {
    return 0xD503201F;
}

/// Emit a 64-bit immediate into rd using MOVZ + up to 3 MOVK.
fn emitImm64(buf: *Buf, rd: Reg, val: u64) void {
    buf.emit(encMovz(rd, @truncate(val), 0));
    if (val > 0xFFFF)
        buf.emit(encMovk(rd, @truncate(val >> 16), 1));
    if (val > 0xFFFF_FFFF)
        buf.emit(encMovk(rd, @truncate(val >> 32), 2));
    if (val > 0xFFFF_FFFF_FFFF)
        buf.emit(encMovk(rd, @truncate(val >> 48), 3));
}

/// Bitcast signed to unsigned (helper to avoid verbose @as/@bitCast chains).
fn asU32(v: anytype) u32 {
    return @bitCast(@as(i32, @intCast(v)));
}

// ============================================================
// Code buffer
// ============================================================

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

// ============================================================
// Fixup records (branches needing patching after emission)
// ============================================================

const FixupKind = enum { b, b_cond, cbz, cbnz };

const FixupTarget = enum(u32) {
    backtrack = 0xFFFF_FFFE,
    fail = 0xFFFF_FFFD,
    success = 0xFFFF_FFFC,
    _, // bytecode PC
};

const Fixup = struct {
    code_off: u32, // byte offset of the instruction to patch
    target: FixupTarget,
    kind: FixupKind,
    cond: u4, // for b_cond
    reg: Reg, // for cbz/cbnz
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

// ============================================================
// Compilation
// ============================================================

fn compile(self: *Jit) !void {
    const est = (self.code.len + 1) * 80 + 2048;
    const size = std.mem.alignForward(usize, est, page_size);

    self.native_code = try std.posix.mmap(
        null,
        size,
        std.c.PROT.READ | std.c.PROT.WRITE,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    );

    var buf = Buf{ .ptr = self.native_code.ptr, .len = 0 };
    var fixups: [8192]Fixup = undefined;
    var fcount: usize = 0;
    var bc_map: [4096]u32 = undefined;

    // -- Prologue: save callee-saved regs, load context --
    emitPrologue(&buf);

    // -- Per-instruction code --
    for (self.code, 0..) |inst, i| {
        bc_map[i] = buf.off();
        self.emitInst(&buf, inst, @intCast(i), &fixups, &fcount);
    }
    if (self.code.len < 4096)
        bc_map[self.code.len] = buf.off();

    // -- Backtrack handler --
    const bt_off = buf.off();
    emitBacktrackHandler(&buf, &fixups, &fcount);

    // -- Fail epilogue: set result = null_cap, fall through to epilogue --
    const fail_off = buf.off();
    buf.emit(encMovn(0, 0)); // MOV x0, #-1 (all ones)
    // fall through to epilogue

    // -- Success epilogue: x0 already set, restore regs + RET --
    const succ_off = buf.off();
    emitEpilogue(&buf);

    // -- Resolve fixups --
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

    // -- Fill jump table (bytecode PC -> native byte offset) --
    for (0..self.code.len) |i| {
        self.jump_table[i] = bc_map[i];
    }

    self.native_len = buf.len;

    // -- Switch to executable --
    try std.posix.mprotect(
        @alignCast(self.native_code[0..size]),
        std.c.PROT.READ | std.c.PROT.EXEC,
    );
}

// -- Prologue: save regs, load context struct fields into callee-saved regs --
fn emitPrologue(buf: *Buf) void {
    // Save callee-saved registers (6 pairs = 96 bytes)
    buf.emit(encStpPre(29, 30, sp_reg, -16));
    buf.emit(encStpPre(pos, bsp, sp_reg, -16));
    buf.emit(encStpPre(inp, inl, sp_reg, -16));
    buf.emit(encStpPre(csp, sdp, sp_reg, -16));
    buf.emit(encStpPre(cap, skp, sp_reg, -16));
    buf.emit(encStpPre(jtp, cbp, sp_reg, -16));

    // x0 = pointer to JitCtx. Load fields into callee-saved regs.
    buf.emit(encLdr(inp, 0, 0)); // x21 = ctx->input_ptr
    buf.emit(encLdr(inl, 0, 8)); // x22 = ctx->input_len
    buf.emit(encLdr(csp, 0, 16)); // x23 = ctx->charsets_ptr
    buf.emit(encLdr(sdp, 0, 24)); // x24 = ctx->string_data_ptr
    buf.emit(encLdr(cap, 0, 32)); // x25 = ctx->captures_ptr
    buf.emit(encLdr(skp, 0, 40)); // x26 = ctx->stack_ptr
    buf.emit(encLdr(jtp, 0, 48)); // x27 = ctx->jump_table_ptr
    buf.emit(encLdr(cbp, 0, 56)); // x28 = ctx->code_base_ptr

    // Initialize pos = 0, backtrack stack pointer = 0.
    buf.emit(encMovz(pos, 0, 0));
    buf.emit(encMovz(bsp, 0, 0));
}

// -- Epilogue: restore regs + RET --
fn emitEpilogue(buf: *Buf) void {
    buf.emit(encLdpPost(jtp, cbp, sp_reg, 16));
    buf.emit(encLdpPost(cap, skp, sp_reg, 16));
    buf.emit(encLdpPost(csp, sdp, sp_reg, 16));
    buf.emit(encLdpPost(inp, inl, sp_reg, 16));
    buf.emit(encLdpPost(pos, bsp, sp_reg, 16));
    buf.emit(encLdpPost(29, 30, sp_reg, 16));
    buf.emit(encRet());
}

// -- Backtrack handler (called via B from failing instructions) --
fn emitBacktrackHandler(buf: *Buf, fixups: *[8192]Fixup, fcount: *usize) void {
    // if (sp == 0) goto fail
    addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, bsp);
    buf.emit(encNop()); // placeholder, patched later

    // loop:
    const loop_off = buf.off();

    // sp--
    buf.emit(encSub(bsp, bsp, 1));

    // x10 = &stack[sp] (sp * 32 = sp << 5)
    buf.emit(encLsl(t1, bsp, 5));
    buf.emit(encAdd(t1, skp, 0)); // placeholder: ADD x10, x26, x10
    // Actually we need ADD Xd, Xn, Xm (register). Let me use a different encoding.
    // ADD Xd, Xn, Xm is not an immediate add. We need the shifted-register form.
    // For now, overwrite with the correct approach:
    // We already have t1 = sp << 5. We need t1 = t1 + x26.
    // ADD X10, X26, X10 -- this is a register ADD. Let me re-emit.
    buf.len -= 4; // undo placeholder
    // ADD Xd, Xn, Xm (shifted register, no shift)
    // 1 00 01011 00 0 Rm 000000 Rn Rd
    // = 0x8B000000 | (Rm << 16) | (Rn << 5) | Rd
    buf.emit(encAddReg(t1, skp, t1));

    // Load tag
    buf.emit(encLdr(t2, t1, 0)); // x11 = tag

    // tag == 0 (choice)?
    const choice_off = buf.off();
    buf.emit(encCbz(t2, 0)); // placeholder: patched to choice handler below

    // tag == 2 (save)?
    buf.emit(encCmpImm(t2, 2));
    const save_off = buf.off();
    buf.emit(encBCond(CC.eq, 0)); // placeholder: patched to save handler below

    // tag == 1 (ret): skip, continue loop
    const skip_off = buf.off();
    addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, bsp);
    buf.emit(encNop()); // if sp == 0, goto fail
    buf.emit(encB(@intCast(@as(i32, @intCast(loop_off)) - @as(i32, @intCast(buf.off())))));

    // save handler: restore captures[slot] = old_value
    const save_handler = buf.off();
    buf.emit(encLdr(t3, t1, 8)); // x12 = slot
    buf.emit(encLdr(t4, t1, 16)); // x13 = old value
    buf.emit(encStrReg(t4, cap, t3)); // captures[slot] = old (STR x13, [x25, x12, LSL #3])
    addFixup(fixups, fcount, buf.off(), .fail, .cbz, 0, bsp);
    buf.emit(encNop()); // if sp == 0, goto fail
    buf.emit(encB(@intCast(@as(i32, @intCast(loop_off)) - @as(i32, @intCast(buf.off())))));

    // choice handler: restore pos, look up native addr via jump table, BR
    const choice_handler = buf.off();
    buf.emit(encLdr(pos, t1, 8)); // restore pos = val1
    buf.emit(encLdr(t2, t1, 16)); // x11 = bytecode PC (val2)
    buf.emit(encLdrReg(t2, jtp, t2)); // x11 = jump_table[bc_pc]
    buf.emit(encAddReg(t2, cbp, t2)); // x11 = code_base + offset
    buf.emit(encBr(t2)); // BR x11

    // Patch internal CBZ/B.cond placeholders
    {
        const rel: i32 = @as(i32, @intCast(choice_handler)) - @as(i32, @intCast(choice_off));
        buf.patchAt(choice_off, encCbz(t2, @intCast(rel)));
    }
    {
        const rel: i32 = @as(i32, @intCast(save_handler)) - @as(i32, @intCast(save_off));
        buf.patchAt(save_off, encBCond(CC.eq, @intCast(rel)));
    }
    _ = skip_off;
}

/// ADD Xd, Xn, Xm (register, no shift).
fn encAddReg(rd: Reg, rn: Reg, rm: Reg) u32 {
    return 0x8B000000 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rd;
}

// ============================================================
// Per-instruction code emission
// ============================================================

fn emitInst(
    self: *const Jit,
    buf: *Buf,
    inst: I.Inst,
    bc_pc: u32,
    fixups: *[8192]Fixup,
    fcount: *usize,
) void {
    _ = self;
    switch (inst.op) {
        .char => {
            // CMP pos, input_len
            buf.emit(encCmpReg(pos, inl));
            // B.HS -> backtrack
            addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.hs, 0);
            buf.emit(encNop());
            // LDRB w9, [input_ptr, pos]
            buf.emit(encLdrbReg(t0, inp, pos));
            // CMP w9, #byte
            buf.emit(encCmpImm32(t0, @intCast(inst.data.byte)));
            // B.NE -> backtrack
            addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.ne, 0);
            buf.emit(encNop());
            // pos++
            buf.emit(encAdd(pos, pos, 1));
        },
        .any => {
            buf.emit(encCmpReg(pos, inl));
            addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.hs, 0);
            buf.emit(encNop());
            buf.emit(encAdd(pos, pos, 1));
        },
        .optional_char => {
            // CMP pos, input_len; B.HS skip
            buf.emit(encCmpReg(pos, inl));
            buf.emit(encBCond(CC.hs, 20)); // skip 4 instructions = 16 bytes ahead + 4 = 20
            // LDRB w9, [input_ptr, pos]
            buf.emit(encLdrbReg(t0, inp, pos));
            // CMP w9, #byte; B.NE skip
            buf.emit(encCmpImm32(t0, @intCast(inst.data.byte)));
            buf.emit(encBCond(CC.ne, 8)); // skip ADD = 4 bytes ahead + 4 = 8
            // pos++
            buf.emit(encAdd(pos, pos, 1));
            // skip:
        },
        .choice => {
            // Push choice entry: {tag=0, val1=pos, val2=target_bc_pc}
            buf.emit(encLsl(t1, bsp, 5)); // t1 = sp * 32
            buf.emit(encAddReg(t1, skp, t1)); // t1 = &stack[sp]
            buf.emit(encStr(xzr, t1, 0)); // tag = 0
            buf.emit(encStr(pos, t1, 8)); // val1 = pos
            buf.emit(encMovz(t2, @intCast(inst.data.offset), 0)); // t2 = target bc pc
            buf.emit(encStr(t2, t1, 16)); // val2 = bc pc
            buf.emit(encAdd(bsp, bsp, 1)); // sp++
        },
        .commit => {
            // Pop backtrack entry, jump to target
            buf.emit(encSub(bsp, bsp, 1));
            addFixup(fixups, fcount, buf.off(), @enumFromInt(inst.data.offset), .b, 0, 0);
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
            addFixup(fixups, fcount, buf.off(), @enumFromInt(inst.data.offset), .b, 0, 0);
            buf.emit(encNop());
        },
        .call => {
            // Push ret entry: {tag=1, val1=native_return_addr}
            buf.emit(encLsl(t1, bsp, 5));
            buf.emit(encAddReg(t1, skp, t1));
            buf.emit(encMovz(t2, 1, 0)); // tag = 1 (ret)
            buf.emit(encStr(t2, t1, 0));
            // ADR t2, return_addr (4 more instructions after ADR = +16 bytes)
            buf.emit(encAdr(t2, 16));
            buf.emit(encStr(t2, t1, 8)); // val1 = return addr
            buf.emit(encAdd(bsp, bsp, 1));
            // B -> callee
            addFixup(fixups, fcount, buf.off(), @enumFromInt(inst.data.offset), .b, 0, 0);
            buf.emit(encNop());
            // return_addr: (next bytecode instruction's code follows)
        },
        .ret => {
            buf.emit(encSub(bsp, bsp, 1));
            buf.emit(encLsl(t1, bsp, 5));
            buf.emit(encAddReg(t1, skp, t1));
            buf.emit(encLdr(t2, t1, 8)); // val1 = native return addr
            buf.emit(encBr(t2));
        },
        .save => {
            const slot: u12 = @intCast(inst.data.slot);
            // Load current capture value
            buf.emit(encMovz(t0, slot, 0)); // t0 = slot
            buf.emit(encLdrReg(t1, cap, t0)); // t1 = captures[slot]
            // Push save entry
            buf.emit(encLsl(t2, bsp, 5));
            buf.emit(encAddReg(t2, skp, t2)); // t2 = &stack[sp]
            buf.emit(encMovz(t3, 2, 0)); // tag = 2 (save)
            buf.emit(encStr(t3, t2, 0)); // tag
            buf.emit(encStr(t0, t2, 8)); // val1 = slot
            buf.emit(encStr(t1, t2, 16)); // val2 = old value
            buf.emit(encAdd(bsp, bsp, 1)); // sp++
            // Set captures[slot] = pos
            buf.emit(encStrReg(pos, cap, t0));
        },
        .match => {
            buf.emit(encMov(0, pos)); // x0 = pos
            addFixup(fixups, fcount, buf.off(), .success, .b, 0, 0);
            buf.emit(encNop());
        },
        .string => {
            const ref = inst.data.string;
            // Call helperStringMatch(input_ptr, input_len, pos,
            //                        string_data_ptr, str_offset, str_len)
            buf.emit(encMov(0, inp));
            buf.emit(encMov(1, inl));
            buf.emit(encMov(2, pos));
            buf.emit(encMov(3, sdp));
            buf.emit(encMovz(4, ref.offset, 0));
            buf.emit(encMovz(5, @intCast(ref.len), 0));
            emitImm64(buf, t0, @intFromPtr(&helperStringMatch));
            buf.emit(encBlr(t0));
            // result in x0: 1 = match, 0 = fail
            addFixup(fixups, fcount, buf.off(), .backtrack, .cbz, 0, 0);
            buf.emit(encNop()); // CBZ x0, backtrack
            buf.emit(encAdd(pos, pos, @intCast(ref.len))); // pos += len
        },
        .set => {
            buf.emit(encCmpReg(pos, inl));
            addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.hs, 0);
            buf.emit(encNop());
            buf.emit(encLdrbReg(t0, inp, pos)); // byte
            buf.emit(encMov(0, csp));
            buf.emit(encMovz(1, @intCast(inst.data.charset), 0));
            buf.emit(encMov(2, t0));
            emitImm64(buf, t0, @intFromPtr(&helperCharsetMatch));
            buf.emit(encBlr(t0));
            addFixup(fixups, fcount, buf.off(), .backtrack, .cbz, 0, 0);
            buf.emit(encNop());
            buf.emit(encAdd(pos, pos, 1));
        },
        .neg_set => {
            buf.emit(encCmpReg(pos, inl));
            addFixup(fixups, fcount, buf.off(), .backtrack, .b_cond, CC.hs, 0);
            buf.emit(encNop());
            buf.emit(encLdrbReg(t0, inp, pos));
            buf.emit(encMov(0, csp));
            buf.emit(encMovz(1, @intCast(inst.data.charset), 0));
            buf.emit(encMov(2, t0));
            emitImm64(buf, t0, @intFromPtr(&helperCharsetMatch));
            buf.emit(encBlr(t0));
            // neg_set: fail if IN set (result != 0)
            addFixup(fixups, fcount, buf.off(), .backtrack, .cbnz, 0, 0);
            buf.emit(encNop());
            buf.emit(encAdd(pos, pos, 1));
        },
    }
    _ = bc_pc;
}

// ============================================================
// Helper functions (called from JIT code via BLR)
// ============================================================

fn helperStringMatch(
    input_ptr: [*]const u8,
    input_len: usize,
    pos_arg: usize,
    str_data: [*]const u8,
    str_off: usize,
    str_len: usize,
) callconv(.c) usize {
    if (pos_arg + str_len > input_len) return 0;
    const a = input_ptr[pos_arg..][0..str_len];
    const b = str_data[str_off..][0..str_len];
    return if (std.mem.eql(u8, a, b)) 1 else 0;
}

fn helperCharsetMatch(
    charsets_ptr: [*]const I.Charset,
    idx: usize,
    byte: usize,
) callconv(.c) usize {
    return if (I.charsetContains(charsets_ptr[idx], @intCast(byte))) 1 else 0;
}

// ============================================================
// Tests
// ============================================================

const testing = std.testing;
const Compiler = @import("Compiler.zig");
const EreScanner = @import("../ere/Scanner.zig");
const EreParser = @import("../ere/Parser.zig");
const PegScanner = @import("../peg/Scanner.zig");
const PegParser = @import("../peg/Parser.zig");

fn compileEre(source: []const u8) Compiler {
    var scanner = EreScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = EreParser.init(tokens, source);
    const rules = parser.parse() catch return Compiler{};
    return Compiler.compile(rules);
}

fn compilePeg(source: []const u8) Compiler {
    var scanner = PegScanner.init(source);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, source);
    const rules = parser.parse() catch return Compiler{};
    return Compiler.compile(rules);
}

fn expectMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = compileEre(source);
    var jit = try Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), input);
    defer jit.deinit();
    const result = jit.execute();
    try testing.expectEqual(expected, result);
}

fn expectPegMatch(source: []const u8, input: []const u8, expected: ?usize) !void {
    var compiler = compilePeg(source);
    var jit = try Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), input);
    defer jit.deinit();
    const result = jit.execute();
    try testing.expectEqual(expected, result);
}

test "jit: literal match" {
    try expectMatch("abc", "abc", 3);
    try expectMatch("abc", "abx", null);
    try expectMatch("abc", "ab", null);
}

test "jit: alternation" {
    try expectMatch("a|b", "a", 1);
    try expectMatch("a|b", "b", 1);
    try expectMatch("a|b", "c", null);
}

test "jit: star repetition" {
    try expectMatch("a*", "", 0);
    try expectMatch("a*", "aaa", 3);
    try expectMatch("a*b", "aaab", 4);
    try expectMatch("a*b", "b", 1);
}

test "jit: plus repetition" {
    try expectMatch("a+", "", null);
    try expectMatch("a+", "aaa", 3);
}

test "jit: optional" {
    try expectMatch("a?b", "ab", 2);
    try expectMatch("a?b", "b", 1);
}

test "jit: character class" {
    try expectMatch("[a-z]+", "hello", 5);
    try expectMatch("[a-z]+", "HELLO", null);
    try expectMatch("[0-9]+", "42", 2);
}

test "jit: negated character class" {
    try expectMatch("[^0-9]+", "abc", 3);
    try expectMatch("[^0-9]+", "123", null);
}

test "jit: dot wildcard" {
    try expectMatch("a.c", "abc", 3);
    try expectMatch("a.c", "aXc", 3);
    try expectMatch("a.c", "ac", null);
}

test "jit: grouped alternation" {
    try expectMatch("(ab|cd)e", "abe", 3);
    try expectMatch("(ab|cd)e", "cde", 3);
    try expectMatch("(ab|cd)e", "ace", null);
}

test "jit: interval repetition" {
    try expectMatch("a{2,4}", "a", null);
    try expectMatch("a{2,4}", "aa", 2);
    try expectMatch("a{2,4}", "aaa", 3);
    try expectMatch("a{2,4}", "aaaa", 4);
    try expectMatch("a{2,4}", "aaaaa", 4);
}

test "jit: alternation with common prefix" {
    try expectMatch("https|http", "https", 5);
    try expectMatch("https|http", "http", 4);
    try expectMatch("https|http", "httq", null);
}

test "jit: peg single rule" {
    try expectPegMatch("Main <- \"hello\"", "hello", 5);
    try expectPegMatch("Main <- \"hello\"", "world", null);
}

test "jit: peg rule references" {
    try expectPegMatch(
        \\Main  <- Greeting " " Name
        \\Greeting <- "hi" / "hello"
        \\Name <- [a-z]+
    , "hi world", 8);
}

test "jit: peg recursive rules" {
    try expectPegMatch(
        \\Expr   <- Term ("+" Term)*
        \\Term   <- Factor ("*" Factor)*
        \\Factor <- "(" Expr ")" / [0-9]+
    , "1+2*3", 5);
}

test "jit: capture single group" {
    var compiler = compileEre("a(bc)d");
    var jit = try Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "abcd");
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 4), jit.execute());
    try testing.expectEqualStrings("bc", jit.getCaptureSlice(0).?);
}

test "jit: capture multiple groups" {
    var compiler = compileEre("(a+)(b+)");
    var jit = try Jit.init(compiler.getCode(), compiler.getCharsets(), compiler.getStringData(), "aaabb");
    defer jit.deinit();
    try testing.expectEqual(@as(?usize, 5), jit.execute());
    try testing.expectEqualStrings("aaa", jit.getCaptureSlice(0).?);
    try testing.expectEqualStrings("bb", jit.getCaptureSlice(1).?);
}
