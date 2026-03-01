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

    // --- LSP server ---
    const lsp_step = b.step("lsp", "Build the LSP server");
    const lsp_exe = b.addExecutable(.{
        .name = "zpars-lsp",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/lsp.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zpars", .module = mod },
            },
        }),
    });
    b.installArtifact(lsp_exe);
    lsp_step.dependOn(b.getInstallStep());

    // --- Vim syntax files ---
    const vim_step = b.step("vim", "Generate Vim syntax files");
    const vim_exe = b.addExecutable(.{
        .name = "zpars-vim",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/vim.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const vim_run = b.addRunArtifact(vim_exe);
    if (b.args) |args| {
        vim_run.addArgs(args);
    }
    vim_step.dependOn(&vim_run.step);

    // --- Shared WASM settings ---
    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    });

    // --- WASM target for the Open VSX extension (analyze only) ---
    const wasm_step = b.step("wasm", "Build WASM module for the Open VSX extension");
    const wasm_lib = b.addExecutable(.{
        .name = "zpars",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm.zig"),
            .target = wasm_target,
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
        .dest_dir = .{ .override = .{ .custom = "../editors/vsx/wasm" } },
    });
    wasm_step.dependOn(&install_wasm.step);

    // --- WASM target for the web demo (analyze + match) ---
    const web_step = b.step("web", "Build WASM module for the web demo");
    const web_wasm = b.addExecutable(.{
        .name = "zpars",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/wasm.zig"),
            .target = wasm_target,
            .optimize = .ReleaseSmall,
        }),
    });
    web_wasm.entry = .disabled;
    web_wasm.root_module.export_symbol_names = &.{
        "alloc",
        "free",
        "analyze",
        "match",
        "format",
    };
    const install_web_wasm = b.addInstallArtifact(web_wasm, .{
        .dest_dir = .{ .override = .{ .custom = "../web" } },
    });
    web_step.dependOn(&install_web_wasm.step);

    // --- Open VSX extension (WASM + TypeScript) ---
    const vsx_step = b.step("vsx", "Build the Open VSX extension (WASM + TypeScript)");
    const npm_compile = b.addSystemCommand(&.{ "npm", "run", "compile" });
    npm_compile.setCwd(b.path("editors/vsx"));
    npm_compile.step.dependOn(&install_wasm.step);
    vsx_step.dependOn(&npm_compile.step);

    // --- Package VSIX ---
    const vsix_step = b.step("vsix", "Package the Open VSX extension as a .vsix");
    const vsce_package = b.addSystemCommand(&.{ "vsce", "package" });
    vsce_package.setCwd(b.path("editors/vsx"));
    vsce_package.step.dependOn(&npm_compile.step);
    vsix_step.dependOn(&vsce_package.step);
}
