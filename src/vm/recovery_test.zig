//! Runtime tests for labeled-failure recovery.
//!
//! Constructs ASTs programmatically (no front-end), compiles them with
//! `rules_as_captures = true`, runs the resulting bytecode through the
//! VM with capture_events on, and asserts the resulting tree.

const std = @import("std");
const testing = std.testing;
const Ast = @import("../Ast.zig");
const CaptureTree = @import("CaptureTree.zig");
const Compiler = @import("Compiler.zig").Compiler;
const Vm = @import("Vm.zig");

const EventVm = Vm.VmWith(.{ .capture_events = true });

const Outcome = struct {
    end: ?usize,
    tree: CaptureTree.Tree,
    label_count: u16,
};

fn runRecovery(rules: []const Ast.Rule, input: []const u8) !Outcome {
    var compiler = try Compiler.compileOpts(rules, .{ .rules_as_captures = true });
    var vm = EventVm.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        input,
    );
    defer vm.deinit();
    const end = try vm.execute();
    const tree = try vm.buildCaptureTree(testing.allocator);
    return .{ .end = end, .tree = tree, .label_count = compiler.label_count };
}

test "recovery: throw caught at top emits MISSING via recover_missing" {
    // Single rule:
    //   stmt = lcatch(L, throw L, missing_label L)
    // The body unconditionally throws; the catch handler is the bare
    // missing_label form (matches PEG `recover_missing`) so the
    // compiler MUST NOT wrap with event_error_open/close.
    // Expected tree: (stmt [0,0] (MISSING [0,0] L)).
    const throw_node: Ast.Node = .{ .throw_label = "L" };
    const missing_node: Ast.Node = .{ .missing_label = "L" };
    const lcatch_node: Ast.Node = .{ .lcatch = .{
        .label = "L",
        .body = &throw_node,
        .handler = &missing_node,
    } };
    const rules = [_]Ast.Rule{
        .{ .name = "stmt", .node = lcatch_node, .incremental = false },
    };

    var r = try runRecovery(&rules, "");
    defer r.tree.deinit();

    try testing.expectEqual(@as(?usize, 0), r.end);
    try testing.expectEqual(@as(usize, 1), r.tree.roots.len);
    const root = r.tree.roots[0];
    try testing.expectEqual(CaptureTree.NodeKind.rule, root.kind);
    try testing.expectEqual(CaptureTree.Span{ .start = 0, .end = 0 }, root.span);
    try testing.expectEqual(@as(usize, 1), root.children.len);

    const m = root.children[0];
    try testing.expectEqual(CaptureTree.NodeKind.missing_node, m.kind);
    try testing.expectEqual(@as(u16, 0), m.group_id);
    try testing.expectEqual(CaptureTree.Span{ .start = 0, .end = 0 }, m.span);
}

test "recovery: throw across rule boundary leaves inner partial" {
    // Two rules, throw fires *inside* inner's body, caught at outer:
    //   outer = lcatch(L, body=rulename(inner), handler=missing_label L)
    //   inner = "a" throw_label L
    // Input "a": inner consumes 'a', then throws L. The unwinder finds
    // outer's lcatch, synthesizes partial_close(inner) at pos 1, then
    // runs the missing handler. Expected:
    //   (outer [0,1] (inner [0,1] partial) (MISSING [1,1] L))
    const a_lit: Ast.Node = .{ .char_val = .{ .value = "a", .case_sensitive = true } };
    const throw_node: Ast.Node = .{ .throw_label = "L" };
    const missing_node: Ast.Node = .{ .missing_label = "L" };

    const inner_concat_elems = [_]Ast.Node{ a_lit, throw_node };
    const inner_body: Ast.Node = .{ .concatenation = &inner_concat_elems };

    const inner_ref: Ast.Node = .{ .rulename = "inner" };
    const lcatch_node: Ast.Node = .{ .lcatch = .{
        .label = "L",
        .body = &inner_ref,
        .handler = &missing_node,
    } };

    const rules = [_]Ast.Rule{
        .{ .name = "outer", .node = lcatch_node, .incremental = false },
        .{ .name = "inner", .node = inner_body, .incremental = false },
    };

    var r = try runRecovery(&rules, "a");
    defer r.tree.deinit();

    try testing.expectEqual(@as(?usize, 1), r.end);
    try testing.expectEqual(@as(usize, 1), r.tree.roots.len);
    const outer = r.tree.roots[0];
    try testing.expectEqual(CaptureTree.NodeKind.rule, outer.kind);
    try testing.expectEqual(@as(u16, 0), outer.group_id);
    try testing.expectEqual(CaptureTree.Span{ .start = 0, .end = 1 }, outer.span);
    try testing.expectEqual(@as(usize, 2), outer.children.len);

    const inner = outer.children[0];
    try testing.expectEqual(CaptureTree.NodeKind.rule_partial, inner.kind);
    try testing.expectEqual(@as(u16, 1), inner.group_id);
    try testing.expectEqual(CaptureTree.Span{ .start = 0, .end = 1 }, inner.span);

    const m = outer.children[1];
    try testing.expectEqual(CaptureTree.NodeKind.missing_node, m.kind);
    try testing.expectEqual(CaptureTree.Span{ .start = 1, .end = 1 }, m.span);
}

test "recovery: rule handler wraps recovery in ERROR node" {
    // Two rules, handler is a real rule (not missing_label):
    //   outer = lcatch(L, body=throw L, handler=rulename(error_handler))
    //   error_handler = "x"
    // Compiler wraps the handler with event_error_open/event_error_close.
    // Input "x": body throws, handler matches "x", the recovered region
    // is surfaced as an ERROR subtree. Expected:
    //   (outer [0,1]
    //     (ERROR [0,1] L
    //       (error_handler [0,1])))
    const throw_node: Ast.Node = .{ .throw_label = "L" };
    const x_lit: Ast.Node = .{ .char_val = .{ .value = "x", .case_sensitive = true } };
    const handler_ref: Ast.Node = .{ .rulename = "error_handler" };
    const lcatch_node: Ast.Node = .{ .lcatch = .{
        .label = "L",
        .body = &throw_node,
        .handler = &handler_ref,
    } };

    const rules = [_]Ast.Rule{
        .{ .name = "outer", .node = lcatch_node, .incremental = false },
        .{ .name = "error_handler", .node = x_lit, .incremental = false },
    };

    var r = try runRecovery(&rules, "x");
    defer r.tree.deinit();

    try testing.expectEqual(@as(?usize, 1), r.end);
    try testing.expectEqual(@as(usize, 1), r.tree.roots.len);
    const outer = r.tree.roots[0];
    try testing.expectEqual(CaptureTree.NodeKind.rule, outer.kind);
    try testing.expectEqual(CaptureTree.Span{ .start = 0, .end = 1 }, outer.span);
    try testing.expectEqual(@as(usize, 1), outer.children.len);

    const err_node = outer.children[0];
    try testing.expectEqual(CaptureTree.NodeKind.error_node, err_node.kind);
    try testing.expectEqual(@as(u16, 0), err_node.group_id);
    try testing.expectEqual(CaptureTree.Span{ .start = 0, .end = 1 }, err_node.span);
    try testing.expectEqual(@as(usize, 1), err_node.children.len);

    const handler_node = err_node.children[0];
    try testing.expectEqual(CaptureTree.NodeKind.rule, handler_node.kind);
    try testing.expectEqual(@as(u16, 1), handler_node.group_id);
}

test "recovery: PEG meta-grammar with #@ directives recovers from malformed PEG" {
    // End-to-end: a PEG meta-grammar (subset) annotated with `#@`
    // recovery directives is parsed by the recovery-enabled PEG
    // front-end, compiled to bytecode, and run against malformed PEG
    // input. The throw fires when a definition lacks `<-`, the catch
    // on Definition emits a MISSING marker, and the parse continues
    // with subsequent definitions instead of aborting.
    const PegScanner = @import("../peg/Scanner.zig").Scanner;
    const PegParser = @import("../peg/Parser.zig").ParserWith(.{ .recovery = true });

    // A trimmed PEG meta-grammar - just enough to recognize sequences
    // of `Identifier <- Expression` definitions, with a recovery branch
    // for definitions missing the arrow. Real PEG metasyntax has more
    // structure (predicates, suffixes, etc.) but the pieces below
    // exercise the full recovery pipeline.
    const meta_grammar =
        \\Grammar    <- Spacing Definition+ EndOfFile
        \\Definition <- Identifier LEFTARROW Body
        \\            / Identifier (!EndOfLine .)* EndOfLine? Spacing  #@ throw missing_arrow
        \\
        \\Body       <- (!EndOfLine .)+ EndOfLine? Spacing
        \\
        \\Identifier <- IdentStart IdentCont* InlineSpacing
        \\IdentStart <- [a-zA-Z_]
        \\IdentCont  <- [a-zA-Z_0-9]
        \\
        \\LEFTARROW  <- '<-' Spacing
        \\
        \\Spacing       <- (Space / EndOfLine)*
        \\InlineSpacing <- Space*
        \\Space         <- ' ' / '\t'
        \\EndOfLine     <- '\n' / '\r'
        \\EndOfFile     <- !.
        \\
        \\#@ rule Definition catches missing_arrow -> recover_missing
    ;

    var scanner = PegScanner.init(meta_grammar);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, meta_grammar);
    const rules = try parser.parse();
    try testing.expect(parser.diagnostics.count == 0);

    var c = try Compiler.compileOpts(rules, .{ .rules_as_captures = true });

    // Malformed PEG: second definition has no `<-`. The recovery
    // branch should fire and emit a MISSING(missing_arrow) leaf
    // inside the second Definition node.
    const malformed = "A <- xy\nB cd\n";
    var vm = EventVm.initEvents(
        testing.allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        malformed,
    );
    defer vm.deinit();
    const end = try vm.execute();
    try testing.expect(end != null);

    var tree = try vm.buildCaptureTree(testing.allocator);
    defer tree.deinit();

    // Walk the tree; we expect at least one missing_node anywhere.
    const found_missing = countNodesOfKind(tree.roots, .missing_node);
    try testing.expect(found_missing >= 1);
}

fn countNodesOfKind(roots: []const CaptureTree.Node, kind: CaptureTree.NodeKind) usize {
    var n: usize = 0;
    for (roots) |node| {
        if (node.kind == kind) n += 1;
        n += countNodesOfKind(node.children, kind);
    }
    return n;
}

test "rules_as_captures: multi-rule grammar with Definition+ produces balanced events" {
    // Regression test: a multi-rule grammar where the start rule has a
    // `+` repetition over a multi-rule sub-call. Each successful loop
    // iteration leaves stale .event frames on top of the loop's choice
    // frame; if `.commit` blindly pops the top frame instead of finding
    // the matching .choice/.lcatch, the choice frame becomes stale and
    // subsequent execution may produce an unbalanced events log.
    const PegScanner = @import("../peg/Scanner.zig").Scanner;
    const PegParser = @import("../peg/Parser.zig").Parser;

    const grammar =
        \\Top  <- Item+
        \\Item <- 'a' 'b'
    ;
    var scanner = PegScanner.init(grammar);
    const tokens = scanner.scanTokens();
    var parser = PegParser.init(tokens, grammar);
    const rules = try parser.parse();

    var c = try Compiler.compileOpts(rules, .{ .rules_as_captures = true });
    var vm = EventVm.initEvents(
        testing.allocator,
        c.getCode(),
        c.getCharsets(),
        c.getStringData(),
        "ababab",
    );
    defer vm.deinit();
    try testing.expectEqual(@as(?usize, 6), try vm.execute());

    // The events log MUST be balanced after a successful match;
    // buildCaptureTree returning an error here indicates a bug in
    // commit/fail_twice frame popping.
    var tree = try vm.buildCaptureTree(testing.allocator);
    defer tree.deinit();

    try testing.expectEqual(@as(usize, 1), tree.roots.len);
    const top = tree.roots[0];
    try testing.expectEqual(@as(usize, 3), top.children.len);
}

test "recovery: uncaught throw fails the whole match" {
    // Single rule, throw not wrapped in any lcatch:
    //   stmt = throw_label L
    // unwindThrow walks off the bottom of the stack and returns null
    // from execute(). The events log is left with whatever was emitted
    // before the throw (here: open(stmt) without a matching close);
    // buildCaptureTree on a failed match is undefined behavior, so the
    // test only asserts the failure.
    const throw_node: Ast.Node = .{ .throw_label = "L" };
    const rules = [_]Ast.Rule{
        .{ .name = "stmt", .node = throw_node, .incremental = false },
    };

    var compiler = try Compiler.compileOpts(&rules, .{ .rules_as_captures = true });
    var vm = EventVm.initEvents(
        testing.allocator,
        compiler.getCode(),
        compiler.getCharsets(),
        compiler.getStringData(),
        "",
    );
    defer vm.deinit();
    try testing.expectEqual(@as(?usize, null), try vm.execute());
}
