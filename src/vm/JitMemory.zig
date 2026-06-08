/// Cross-platform allocation of the executable code region for the JIT.
///
/// Three concerns:
///
/// 1. **W^X on hardened runtime.** macOS arm64 under hardened runtime
///    rejects `mprotect(... PROT_EXEC)` on a plain anonymous RW mapping
///    with `EACCES` unless the process holds `com.apple.security.cs.
///    allow-jit` *and* the page was mapped with `MAP_JIT`. We try
///    `MAP_JIT` first on Apple Silicon and fall back to the plain path
///    when the kernel rejects it (e.g. the entitlement is missing — on
///    an unsigned dev binary the plain path still works, so we don't
///    want to hard-fail there).
///
/// 2. **W^X toggling.** With MAP_JIT the page is in W^X mode, gated by
///    `pthread_jit_write_protect_np`: pass 0 before writing code, 1
///    before executing it. The thread-local flip makes this cheap.
///
/// 3. **icache invalidation.** arm64 has separate I- and D-caches; CPUs
///    can hold stale icache lines from when the page was zero or RW
///    data. Without an explicit flush, freshly written branches can
///    execute as garbage (SIGILL or silent miscompile). On Linux arm64
///    `mprotect(PROT_EXEC)` does NOT flush the icache, so we must do it
///    ourselves. macOS's `mprotect`-to-PROT_EXEC implementation does
///    issue cache maintenance, but `pthread_jit_write_protect_np` is
///    the documented barrier, so we use it. x86 keeps the I- and
///    D-caches coherent in hardware, no explicit flush needed.
const std = @import("std");
const builtin = @import("builtin");

pub const page_size = std.heap.page_size_min;

const is_darwin = builtin.os.tag == .macos or
    builtin.os.tag == .ios or
    builtin.os.tag == .tvos or
    builtin.os.tag == .watchos or
    builtin.os.tag == .visionos;
const is_arm64 = builtin.cpu.arch == .aarch64;
const want_map_jit = is_darwin and is_arm64;

/// Tracks how a region was allocated so `finalize` and `free` know
/// whether to use the MAP_JIT pthread flip or plain mprotect.
pub const CodeMem = struct {
    slice: []align(page_size) u8,
    used_map_jit: bool,
};

/// Allocate a writable code region of `size` bytes. On Apple Silicon
/// (where it's available), tries `MAP_JIT` first and falls back to a
/// plain `mmap(RW)` when the kernel rejects it (no JIT entitlement).
/// Other platforms always use `mmap(RW)` and rely on a later
/// `mprotect(RX)` in `finalize`.
pub fn alloc(size: usize) !CodeMem {
    const aligned_size = std.mem.alignForward(usize, size, page_size);

    if (want_map_jit) {
        // mmap with MAP_JIT requires PROT_READ|WRITE|EXEC up front;
        // the per-thread `pthread_jit_write_protect_np` then chooses
        // which side of W^X is active. The kernel returns EACCES
        // (PermissionDenied) when the JIT entitlement is missing.
        const result = std.posix.mmap(
            null,
            aligned_size,
            .{ .READ = true, .WRITE = true, .EXEC = true },
            .{ .TYPE = .PRIVATE, .ANONYMOUS = true, .JIT = true },
            -1,
            0,
        );
        if (result) |slice| {
            // Page comes back exec-protected (W^X default state).
            // Flip to writable for the codegen phase.
            jitWriteProtect(false);
            return .{ .slice = @alignCast(slice[0..aligned_size]), .used_map_jit = true };
        } else |_| {
            // Fall through to plain path.
        }
    }

    const slice = try std.posix.mmap(
        null,
        aligned_size,
        .{ .READ = true, .WRITE = true },
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    );
    return .{ .slice = @alignCast(slice[0..aligned_size]), .used_map_jit = false };
}

/// Finalize the region for execution. Call after all writes have
/// completed. Flips MAP_JIT pages back to executable, or `mprotect`s
/// plain pages to RX. Always issues an icache flush on arm64.
pub fn finalize(mem: CodeMem) !void {
    if (mem.used_map_jit) {
        jitWriteProtect(true);
        // pthread_jit_write_protect_np documents that it issues the
        // appropriate barriers, but doesn't promise icache flush. Be
        // explicit so we don't depend on undocumented behavior.
        flushICache(mem.slice.ptr, mem.slice.len);
        return;
    }

    try std.process.protectMemory(mem.slice, .{ .read = true, .execute = true });
    if (is_arm64) flushICache(mem.slice.ptr, mem.slice.len);
}

/// Release the region. Safe to call on partially-finalized regions.
pub fn free(mem: CodeMem) void {
    std.posix.munmap(mem.slice);
}

// ---- Platform helpers ----

/// `pthread_jit_write_protect_np(enable)` — Apple Silicon W^X toggle.
/// Linked from libSystem; only call when `want_map_jit` is true.
extern "c" fn pthread_jit_write_protect_np(enabled: c_int) void;

fn jitWriteProtect(enabled: bool) void {
    if (!want_map_jit) return;
    pthread_jit_write_protect_np(if (enabled) 1 else 0);
}

/// arm64 icache flush. macOS exposes `sys_icache_invalidate`; on Linux
/// arm64 we issue the equivalent `ic ivau` / `dsb ish` / `isb` sequence
/// inline so we don't depend on a libc helper.
fn flushICache(ptr: [*]const u8, len: usize) void {
    if (!is_arm64) return;

    if (is_darwin) {
        sys_icache_invalidate(ptr, len);
        return;
    }

    // Generic arm64 sequence per ARMv8 ARM B2.4.4. dsb ishst orders
    // the prior writes; ic ivau invalidates icache by VA to PoU; dsb
    // ish waits for completion; isb refetches.
    const cache_line: usize = 64; // conservative; real value via ctr_el0
    asm volatile ("dsb ishst" ::: .{ .memory = true });
    var addr: usize = @intFromPtr(ptr);
    const end = addr + len;
    while (addr < end) : (addr += cache_line) {
        asm volatile ("ic ivau, %[addr]"
            :
            : [addr] "r" (addr),
        );
    }
    asm volatile ("dsb ish" ::: .{ .memory = true });
    asm volatile ("isb" ::: .{ .memory = true });
}

extern "c" fn sys_icache_invalidate(start: [*]const u8, len: usize) void;
