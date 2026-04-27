// zpars build configuration.
//
// Build steps:
//   zig build              Build the CLI executable
//   zig build run          Run the CLI (pass args with -- after)
//   zig build test         Run all tests (library + CLI)
//   zig build bench        Run benchmarks (ReleaseFast)
//   zig build lsp          Build the LSP server
//   zig build vim          Generate Vim syntax files
//   zig build wasm         Build WASM module for the Open VSX extension
//   zig build web          Build WASM module for the web demo
//   zig build vsx          Build the full Open VSX extension (WASM + TypeScript)
//   zig build vsix         Package the extension as a .vsix
//
// The library module ("zpars") is rooted at src/root.zig and re-exported
// as a dependency for the CLI (src/main.zig), LSP, benchmarks, and WASM
// targets. Tests cover both the library and the CLI entry point.

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
    // Drive the test binary through a plain `Run` step (spawns it
    // directly with inherited stdio) instead of the default
    // `--listen=-` protocol, so per-test output and the final
    // "All N tests passed." line are always printed without needing
    // `--summary all` on the command line. src/main.zig currently has
    // no tests, so it isn't included here.
    const run_mod_tests = std.Build.Step.Run.create(b, "run mod tests");
    run_mod_tests.addArtifactArg(mod_tests);
    run_mod_tests.has_side_effects = true;
    test_step.dependOn(&run_mod_tests.step);

    // Companion step for coverage-guided fuzzing. The default `test`
    // step above uses a plain `Run` (no `--listen=-`) for readable
    // per-test output, but the fuzzer needs the listen protocol so
    // the build runner can discover fuzz tests and feed them mutated
    // inputs. Run with:
    //   zig build fuzz --fuzz
    // The `--fuzz` is a build-runner flag, not a step argument.
    const fuzz_step = b.step("fuzz", "Run tests via the listen protocol (pair with --fuzz)");
    const fuzz_run = b.addRunArtifact(mod_tests);
    fuzz_step.dependOn(&fuzz_run.step);

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

    const wasm_target = b.resolveTargetQuery(.{
        .cpu_arch = .wasm32,
        .os_tag = .freestanding,
    });

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
        "tree",
    };
    const install_wasm = b.addInstallArtifact(wasm_lib, .{
        .dest_dir = .{ .override = .{ .custom = "../editors/vsx/wasm" } },
    });
    wasm_step.dependOn(&install_wasm.step);

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
        "tree",
    };
    const install_web_wasm = b.addInstallArtifact(web_wasm, .{
        .dest_dir = .{ .override = .{ .custom = "../web" } },
    });
    web_step.dependOn(&install_web_wasm.step);

    const vsx_step = b.step("vsx", "Build the Open VSX extension (WASM + TypeScript)");
    const npm_compile = b.addSystemCommand(&.{ "npm", "run", "compile" });
    npm_compile.setCwd(b.path("editors/vsx"));
    npm_compile.step.dependOn(&install_wasm.step);
    vsx_step.dependOn(&npm_compile.step);

    const vsix_step = b.step("vsix", "Package the Open VSX extension as a .vsix");
    const vsce_package = b.addSystemCommand(&.{ "vsce", "package" });
    vsce_package.setCwd(b.path("editors/vsx"));
    vsce_package.step.dependOn(&npm_compile.step);
    vsix_step.dependOn(&vsce_package.step);
}
