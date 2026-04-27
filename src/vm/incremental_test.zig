//! End-to-end tests for incremental memo invalidation.
//!
//! Constructs a small grammar programmatically, compiles it through
//! `Compiler` -> `LookaheadAnalysis` -> `Aot`, runs the AOT engine to
//! populate the memo table, then exercises `RuntimeState.applyEdit` with
//! edits chosen relative to the static `examined_max` bound. Closes the
//! loop on the analysis with realistic bytecode (rather than the
//! hand-built fixtures in `LookaheadAnalysis.zig` and `RuntimeState.zig`).

const std = @import("std");
const testing = std.testing;
const Ast = @import("../Ast.zig");
const Compiler = @import("Compiler.zig").Compiler;
const Aot = @import("Aot.zig");
const AotRuntime = @import("AotRuntime.zig");
const memo_mod = @import("memo.zig");

const MemoEngine = AotRuntime.EngineWith(.{ .memoize = true });

test "incremental: examined_max from real grammar matches analysis" {
    // Grammar: Outer = Word; Word = "a" !"b"
    // Word's bytecode: char 'a'; choice L; char 'b'; fail_twice; L: ret
    //   -> consume_max = 1, examined_max = 2.
    // Outer's bytecode: memo_call Word; ret
    //   -> consume_max = 1, examined_max = 2.
    const a_lit: Ast.Node = .{ .char_val = .{ .value = "a", .case_sensitive = true } };
    const b_lit: Ast.Node = .{ .char_val = .{ .value = "b", .case_sensitive = true } };
    const not_b: Ast.Node = .{ .not_predicate = &b_lit };
    const word_seq = [_]Ast.Node{ a_lit, not_b };
    const word_body: Ast.Node = .{ .concatenation = &word_seq };
    const outer_body: Ast.Node = .{ .rulename = "Word" };
    const rules = [_]Ast.Rule{
        .{ .name = "Outer", .node = outer_body, .incremental = false },
        .{ .name = "Word", .node = word_body, .incremental = false },
    };

    var compiler = try Compiler.compileOpts(&rules, .{ .memoize = true, .optimize = false });
    const examined = compiler.getExaminedMax();

    try testing.expectEqual(@as(u16, 2), compiler.getMemoRuleCount());
    try testing.expectEqual(@as(u32, 2), examined[0]); // Outer.
    try testing.expectEqual(@as(u32, 2), examined[1]); // Word.
}

test "incremental: applyEdit keeps memo entries outside the lookahead reach" {
    const a_lit: Ast.Node = .{ .char_val = .{ .value = "a", .case_sensitive = true } };
    const b_lit: Ast.Node = .{ .char_val = .{ .value = "b", .case_sensitive = true } };
    const not_b: Ast.Node = .{ .not_predicate = &b_lit };
    const word_seq = [_]Ast.Node{ a_lit, not_b };
    const word_body: Ast.Node = .{ .concatenation = &word_seq };
    const outer_body: Ast.Node = .{ .rulename = "Word" };
    const rules = [_]Ast.Rule{
        .{ .name = "Outer", .node = outer_body, .incremental = false },
        .{ .name = "Word", .node = word_body, .incremental = false },
    };

    var compiler = try Compiler.compileOpts(&rules, .{ .memoize = true, .optimize = false });

    var blob = try Aot.compileToBlobWith(
        .{ .memoize = true },
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        compiler.getCaptureCount(),
        compiler.getMemoRuleCount(),
        compiler.getExaminedMax(),
    );
    defer Aot.freeBlob(testing.allocator, &blob);

    var engine = try MemoEngine.initPackrat(testing.allocator, blob);
    defer engine.deinit();

    // Execute on "aXYZ": Word matches "a" at pos 0 (since input[1]='X'
    // is not 'b'); Outer succeeds at end=1.
    const input = "aXYZ";
    try testing.expectEqual(@as(?usize, 1), engine.execute(input));

    // After execute, the memo table has success entries at (Word=1, p=0)
    // and (Outer=0, p=0). Word reads up to byte 1 (exclusive: examines
    // input[1]='X' for the !"b" predicate); Outer covers the same span.
    const stride_pre = engine.state.memo_stride;
    const word_entry_pre = engine.state.memo_table[1 * stride_pre + 0];
    try testing.expectEqual(memo_mod.State.success, word_entry_pre.state);
    try testing.expectEqual(@as(u32, 1), word_entry_pre.next_pos_or_frame);

    // Edit at byte 2 (pure insertion). Word's reach is 0 + 2 = 2 <= 2 =
    // edit.start, so the entry must survive. Same for Outer.
    try engine.state.applyEdit(.{ .start = 2, .old_end = 2, .new_end = 3 });
    const stride_post = engine.state.memo_stride;
    try testing.expectEqual(@as(usize, 6), stride_post); // 5 + insertion of 1 -> 5 + 1 + 1.

    const word_entry_post = engine.state.memo_table[1 * stride_post + 0];
    try testing.expectEqual(memo_mod.State.success, word_entry_post.state);
    try testing.expectEqual(@as(u32, 1), word_entry_post.next_pos_or_frame);

    const outer_entry_post = engine.state.memo_table[0 * stride_post + 0];
    try testing.expectEqual(memo_mod.State.success, outer_entry_post.state);
    try testing.expectEqual(@as(u32, 1), outer_entry_post.next_pos_or_frame);
}

test "incremental: applyEdit drops memo entries reached by the lookahead" {
    const a_lit: Ast.Node = .{ .char_val = .{ .value = "a", .case_sensitive = true } };
    const b_lit: Ast.Node = .{ .char_val = .{ .value = "b", .case_sensitive = true } };
    const not_b: Ast.Node = .{ .not_predicate = &b_lit };
    const word_seq = [_]Ast.Node{ a_lit, not_b };
    const word_body: Ast.Node = .{ .concatenation = &word_seq };
    const outer_body: Ast.Node = .{ .rulename = "Word" };
    const rules = [_]Ast.Rule{
        .{ .name = "Outer", .node = outer_body, .incremental = false },
        .{ .name = "Word", .node = word_body, .incremental = false },
    };

    var compiler = try Compiler.compileOpts(&rules, .{ .memoize = true, .optimize = false });

    var blob = try Aot.compileToBlobWith(
        .{ .memoize = true },
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        compiler.getCaptureCount(),
        compiler.getMemoRuleCount(),
        compiler.getExaminedMax(),
    );
    defer Aot.freeBlob(testing.allocator, &blob);

    var engine = try MemoEngine.initPackrat(testing.allocator, blob);
    defer engine.deinit();

    const input = "aXYZ";
    try testing.expectEqual(@as(?usize, 1), engine.execute(input));

    const stride_pre = engine.state.memo_stride;
    try testing.expectEqual(memo_mod.State.success, engine.state.memo_table[1 * stride_pre + 0].state);

    // Edit at byte 1 (pure insertion). Word's reach is 0 + 2 = 2 > 1 =
    // edit.start, so the entry must be invalidated.
    try engine.state.applyEdit(.{ .start = 1, .old_end = 1, .new_end = 2 });
    const stride_post = engine.state.memo_stride;

    try testing.expectEqual(memo_mod.State.empty, engine.state.memo_table[1 * stride_post + 0].state);
    try testing.expectEqual(memo_mod.State.empty, engine.state.memo_table[0 * stride_post + 0].state);
}
