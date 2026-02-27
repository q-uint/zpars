const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("zpars", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
    });

    const exe = b.addExecutable(.{
        .name = "zpars",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zpars", .module = mod },
            },
        }),
    });

    b.installArtifact(exe);

    const run_step = b.step("run", "Run the app");
    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const bench_step = b.step("bench", "Run benchmarks");
    const bench_exe = b.addExecutable(.{
        .name = "bench",
        .root_module = b.createModule(.{
            .root_source_file = b.path("bench.zig"),
            .target = target,
            .optimize = .ReleaseFast,
            .imports = &.{
                .{ .name = "zpars", .module = mod },
            },
        }),
    });
    bench_step.dependOn(&b.addRunArtifact(bench_exe).step);

    const test_step = b.step("test", "Run tests");
    const mod_tests = b.addTest(.{ .root_module = mod });
    const exe_tests = b.addTest(.{ .root_module = exe.root_module });
    test_step.dependOn(&b.addRunArtifact(mod_tests).step);
    test_step.dependOn(&b.addRunArtifact(exe_tests).step);

    // --- WASM target for the VSCode extension ---
    const wasm_step = b.step("wasm", "Build WASM module for the VSCode extension");
    const wasm_lib = b.addExecutable(.{
        .name = "zpars",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm.zig"),
            .target = b.resolveTargetQuery(.{
                .cpu_arch = .wasm32,
                .os_tag = .freestanding,
            }),
            .optimize = .ReleaseSmall,
        }),
    });
    wasm_lib.entry = .disabled;
    wasm_lib.root_module.export_symbol_names = &.{
        "alloc",
        "free",
        "analyze",
    };
    const install_wasm = b.addInstallArtifact(wasm_lib, .{
        .dest_dir = .{ .override = .{ .custom = "../editors/vscode/wasm" } },
    });
    wasm_step.dependOn(&install_wasm.step);

    // --- VSCode extension (WASM + TypeScript) ---
    const vscode_step = b.step("vscode", "Build the VSCode extension (WASM + TypeScript)");
    const npm_compile = b.addSystemCommand(&.{ "npm", "run", "compile" });
    npm_compile.setCwd(b.path("editors/vscode"));
    npm_compile.step.dependOn(&install_wasm.step);
    vscode_step.dependOn(&npm_compile.step);
}
