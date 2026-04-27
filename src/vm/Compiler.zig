/// Compiles AST nodes into bytecode for the grammar VM.
const std = @import("std");
const Ast = @import("../Ast.zig");
const I = @import("Instruction.zig");

const Optimizer = @import("Optimizer.zig");
const LookaheadAnalysis = @import("LookaheadAnalysis.zig");

pub const Config = struct {
    max_code: u32 = 4096,
    max_charsets: u16 = 256,
    max_patches: u32 = 512,
    max_string_data: u16 = 4096,
    max_captures: u16 = 64,
};

pub const Compiler = CompilerWith(.{});

/// Compile-time errors caused by grammar input exceeding a fixed
/// resource bound. These are recoverable: the caller can surface a
/// diagnostic and reject the grammar. Programmer-bug invariants in
/// the compiler still trip `unreachable` / `assert`, not these.
pub const Error = error{
    TooManyRules,
    TooManyLabels,
    TooManyCaptures,
    CodeBufferExhausted,
    PatchBufferExhausted,
    CharsetBufferExhausted,
};

pub fn CompilerWith(comptime config: Config) type {
    return struct {
        const Self = @This();

        code: [config.max_code]I.Inst = undefined,
        code_len: u32 = 0,

        charsets: [config.max_charsets]I.Charset = undefined,
        charset_len: u16 = 0,

        string_data: [config.max_string_data]u8 = undefined,
        string_data_len: u16 = 0,

        patches: [config.max_patches]Patch = undefined,
        patch_len: u32 = 0,

        capture_count: u16 = 0,
        /// AST capture nodes that have already been assigned a slot. A
        /// capture node can be compiled multiple times (e.g. when its
        /// parent is a repetition lowered into several visits); pointer
        /// identity here keeps its group id stable across visits, so
        /// `(a)+` is one group, not N.
        capture_node_ptrs: [config.max_captures / 2]?*const Ast.Node =
            [_]?*const Ast.Node{null} ** (config.max_captures / 2),
        memo_rule_count: u16 = 0,

        /// Per-rule name slices into `string_data`. Populated for every
        /// rule in `compileOpts`; `getRuleName` decodes them. Storing
        /// names here (rather than via the AST `rules` slice) keeps the
        /// Compiler self-contained - the AST can be freed after compile.
        rule_names: [256]I.Inst.StringRef = [_]I.Inst.StringRef{.{ .offset = 0, .len = 0 }} ** 256,
        rule_count: u16 = 0,

        /// Label-id table for recovery. Populated lazily by
        /// `labelIdFor` on the first reference to a label from a
        /// `throw_label`, `lcatch`, or `missing_label` AST node. Indexed
        /// by label id; `getLabelName` decodes them. Empty for grammars
        /// without recovery.
        label_names: [256]I.Inst.StringRef = [_]I.Inst.StringRef{.{ .offset = 0, .len = 0 }} ** 256,
        label_count: u16 = 0,

        optimize_enabled: bool = true,

        rules: []const Ast.Rule = &.{},

        /// Token-event emission state, populated by `compileOpts` from
        /// `Options.token_events` / `Options.tagged_tokens`. Read by
        /// `shouldEmitToken` during literal compilation.
        token_events: TokenEvents = .off,
        tagged_tokens: []const []const u8 = &.{},

        /// Field-event emission flag, populated from
        /// `Options.field_events`. When false, `.field` AST nodes
        /// compile to just their body (no `event_field` emission).
        field_events: bool = false,

        /// Field-name table. Populated lazily by `fieldIdFor` when a
        /// `.field` node is compiled with `field_events = true`. The
        /// `getFieldName` accessor maps an id back for renderers and
        /// query-side resolution. Empty for grammars that don't use
        /// fields (or compile with `field_events = false`).
        field_names: [256]I.Inst.StringRef = [_]I.Inst.StringRef{.{ .offset = 0, .len = 0 }} ** 256,
        field_count: u16 = 0,

        /// Per-rule bytecode bounds (`[rule_addrs[i], rule_ends[i])`),
        /// retained for post-compile analysis (`LookaheadAnalysis`).
        /// Indexed by rule index `i < rule_count`. Zero outside that
        /// range.
        rule_addrs: [256]u32 = [_]u32{0} ** 256,
        rule_ends: [256]u32 = [_]u32{0} ** 256,

        /// Map from rule index to memo rule id. `null` means the rule
        /// isn't memoized (either because `memoize` is off or because
        /// it carries captures and `memoize_captures` is off). Set by
        /// `rewriteMemoCalls`.
        memo_id_for_rule: [256]?u16 = [_]?u16{null} ** 256,

        /// Per-memo-rule upper bound on the byte offset (relative to
        /// rule entry pos) the rule's body can read. Indexed by memo
        /// rule id, valid for `id < memo_rule_count`. Computed by
        /// `LookaheadAnalysis` when `opts.memoize` is true; zero (and
        /// unused) otherwise. `unbounded_value` flags rules whose
        /// bound couldn't be statically established (recursive cycles,
        /// `*` / `+` loops). Read by `RuntimeState.applyEdit` to
        /// decide whether a cached `(rule, p)` memo entry can survive
        /// a byte-range edit.
        examined_max: [256]u32 = [_]u32{0} ** 256,

        const Patch = struct {
            addr: u32,
            name: []const u8,
        };

        pub fn compile(rules: []const Ast.Rule) Error!Self {
            return compileOpts(rules, .{});
        }

        pub const Options = struct {
            optimize: bool = true,
            memoize: bool = false,
            /// When true, capture-bearing rules are eligible for
            /// packrat memoization. Requires the VM/JIT to run with
            /// `capture_events = true`: the memo entry caches the
            /// rule's open/close events and replays them on a hit so
            /// the capture slots are restored. With events off there
            /// is no cached state to replay from, so captures would
            /// silently be wrong on a hit-after-backtrack - hence the
            /// default exclusion.
            memoize_captures: bool = false,
            /// When true, every multi-rule grammar rule emits an
            /// `event_open` at body entry and `event_close` just
            /// before `ret`, keyed by rule_id. The resulting event
            /// log mirrors the rule call hierarchy as a parse tree
            /// (each node typed by rule name). Requires the VM/JIT
            /// to run with `capture_events = true`; the events are
            /// no-ops otherwise. Single-rule grammars are unchanged.
            rules_as_captures: bool = false,
            /// Anonymous-token event emission. Tri-state:
            /// - `.off` (default): literal-matching opcodes emit no
            ///   events; trees contain only rule nodes.
            /// - `.all`: every literal (`char`, `string` after
            ///   optimization) is followed by an `event_token` so all
            ///   anonymous tokens appear in the tree. Mirrors
            ///   tree-sitter, but inflates the event log.
            /// - `.tagged`: only literals whose bytes appear in
            ///   `tagged_tokens` get the event. Lets grammars opt in
            ///   selectively (typically keywords/operators).
            /// Requires `capture_events = true` at runtime; the events
            /// are no-ops otherwise. Supported by the VM and both JIT
            /// backends; AOT inherits JIT support.
            token_events: TokenEvents = .off,
            /// Literal bytes to instrument under `token_events =
            /// .tagged`. Each entry is a literal as it appears in the
            /// grammar (e.g. `"function"`, `"+"`). Ignored in other
            /// modes. The slice must outlive the compile call.
            tagged_tokens: []const []const u8 = &.{},
            /// When true, `.field`-tagged AST nodes (produced by the
            /// PEG front-end from `name:Expr` syntax) emit an
            /// `event_field` instruction before the body so the parse
            /// tree carries the field name on the corresponding node.
            /// When false (default), the field tag is dropped at
            /// compile time and the body compiles unchanged. Supported
            /// by the VM and both JIT backends; AOT inherits JIT
            /// support.
            field_events: bool = false,
        };

        pub const TokenEvents = enum { off, all, tagged };

        pub fn compileOpts(rules: []const Ast.Rule, opts: Options) Error!Self {
            var c = Self{};
            if (rules.len > c.rule_names.len) return Error.TooManyRules;

            c.rules = rules;
            c.optimize_enabled = opts.optimize;
            c.token_events = opts.token_events;
            c.tagged_tokens = opts.tagged_tokens;
            c.field_events = opts.field_events;
            c.rule_count = @intCast(rules.len);
            for (rules, 0..) |rule, i| c.rule_names[i] = c.internRuleName(rule.name);

            if (rules.len == 0) {
                try c.emit(.{ .op = .match });
            } else if (rules.len == 1) {
                if (opts.rules_as_captures) try c.emit(.{ .op = .event_open, .data = .{ .slot = 0 } });
                try c.compileNode(&rules[0].node);
                if (opts.rules_as_captures) try c.emit(.{ .op = .event_close, .data = .{ .slot = 0 } });
                try c.emit(.{ .op = .match });
                c.patchCalls(&.{0}, rules);
            } else {
                const entry_call = try c.emitPlaceholder();
                try c.emit(.{ .op = .match });

                for (rules, 0..) |rule, i| {
                    c.rule_addrs[i] = c.code_len;
                    if (opts.rules_as_captures)
                        try c.emit(.{ .op = .event_open, .data = .{ .slot = @intCast(i) } });
                    try c.compileNode(&rule.node);
                    if (opts.rules_as_captures)
                        try c.emit(.{ .op = .event_close, .data = .{ .slot = @intCast(i) } });
                    try c.emit(.{ .op = .ret });
                    c.rule_ends[i] = c.code_len;
                }

                c.code[entry_call] = .{ .op = .call, .data = .{ .offset = c.rule_addrs[0] } };
                c.patchCalls(c.rule_addrs[0..rules.len], rules);

                if (opts.memoize) {
                    c.rewriteMemoCalls(
                        c.rule_addrs[0..rules.len],
                        c.rule_ends[0..rules.len],
                        opts.memoize_captures,
                    );
                    c.runLookaheadAnalysis(rules.len);
                }
            }

            if (c.optimize_enabled) {
                Optimizer.optimize(&c);
            }
            return c;
        }

        /// Run `LookaheadAnalysis` over the just-compiled bytecode and
        /// populate `examined_max` for each memoized rule. Called after
        /// `rewriteMemoCalls` (so `memo_call` opcodes carry their rule
        /// ids) and before `Optimizer.optimize` (the optimizer rewrites
        /// the bytecode but preserves examined-byte semantics, so the
        /// pre-optimization analysis is still a valid upper bound).
        fn runLookaheadAnalysis(self: *Self, rule_count_arg: usize) void {
            // Stack-allocate the analysis scratch and per-rule summary
            // buffer. Sized for the Compiler's `max_code` configuration
            // since a single rule body cannot exceed the total code
            // budget.
            var scratch: LookaheadAnalysis.Scratch(config.max_code) = .{};
            var summaries: [256]LookaheadAnalysis.RuleSummary = undefined;
            const slice = summaries[0..rule_count_arg];
            LookaheadAnalysis.analyze(
                self.code[0..self.code_len],
                self.rule_addrs[0..rule_count_arg],
                self.rule_ends[0..rule_count_arg],
                slice,
                &scratch,
            );
            for (0..rule_count_arg) |i| {
                if (self.memo_id_for_rule[i]) |mid| {
                    self.examined_max[mid] = slice[i].examined_max;
                }
            }
        }

        /// Copy the rule name into `string_data` so it outlives the AST.
        /// Truncates names longer than 255 bytes (the StringRef.len limit).
        fn internRuleName(self: *Self, name: []const u8) I.Inst.StringRef {
            const len: u8 = @intCast(@min(name.len, 255));
            const offset: u16 = self.string_data_len;
            if (@as(usize, offset) + len > self.string_data.len) return .{ .offset = 0, .len = 0 };
            @memcpy(self.string_data[offset..][0..len], name[0..len]);
            self.string_data_len += len;
            return .{ .offset = offset, .len = len };
        }

        /// Look up a rule's name by id. Valid for ids < `rule_count`.
        pub fn getRuleName(self: *const Self, rule_id: u16) []const u8 {
            const ref = self.rule_names[rule_id];
            return self.string_data[ref.offset..][0..ref.len];
        }

        /// Look up a label's name by id. Valid for ids < `label_count`.
        /// Used by the tree CLI to render ERROR / MISSING node labels.
        pub fn getLabelName(self: *const Self, label_id: u16) []const u8 {
            const ref = self.label_names[label_id];
            return self.string_data[ref.offset..][0..ref.len];
        }

        /// Look up a field's name by id. Valid for ids < `field_count`.
        pub fn getFieldName(self: *const Self, field_id: u16) []const u8 {
            const ref = self.field_names[field_id];
            return self.string_data[ref.offset..][0..ref.len];
        }

        /// Resolve a label name to an id, allocating a fresh id on first
        /// use. Names are interned into `string_data` so they outlive
        /// the AST.
        fn labelIdFor(self: *Self, name: []const u8) Error!u16 {
            for (0..self.label_count) |i| {
                const ref = self.label_names[i];
                const stored = self.string_data[ref.offset..][0..ref.len];
                if (std.mem.eql(u8, stored, name)) return @intCast(i);
            }
            if (self.label_count >= self.label_names.len) return Error.TooManyLabels;
            self.label_names[self.label_count] = self.internRuleName(name);
            const id: u16 = self.label_count;
            self.label_count += 1;
            return id;
        }

        /// Resolve a field name to an id, allocating a fresh id on first
        /// use. Reuses `internRuleName` for string storage. Reuses the
        /// `TooManyLabels` error for capacity overflow since fields and
        /// labels share the same `[256]StringRef` shape.
        fn fieldIdFor(self: *Self, name: []const u8) Error!u16 {
            for (0..self.field_count) |i| {
                const ref = self.field_names[i];
                const stored = self.string_data[ref.offset..][0..ref.len];
                if (std.mem.eql(u8, stored, name)) return @intCast(i);
            }
            if (self.field_count >= self.field_names.len) return Error.TooManyLabels;
            self.field_names[self.field_count] = self.internRuleName(name);
            const id: u16 = self.field_count;
            self.field_count += 1;
            return id;
        }

        fn rewriteMemoCalls(
            self: *Self,
            rule_addrs: []const u32,
            rule_ends: []const u32,
            memoize_captures: bool,
        ) void {
            for (rule_addrs, rule_ends, 0..) |start, end, i| {
                if (!memoize_captures) {
                    var has_save = false;
                    for (self.code[start..end]) |inst| {
                        if (inst.op == .save) {
                            has_save = true;
                            break;
                        }
                    }
                    if (has_save) continue;
                }
                self.memo_id_for_rule[i] = self.memo_rule_count;
                self.memo_rule_count += 1;
            }

            for (self.code[0..self.code_len]) |*inst| {
                if (inst.op != .call) continue;
                const target = inst.data.offset;
                for (rule_addrs, 0..) |addr, i| {
                    if (addr == target) {
                        if (self.memo_id_for_rule[i]) |id| {
                            inst.* = .{
                                .op = .memo_call,
                                .data = .{ .memo = .{ .rule_id = id, .offset = target } },
                            };
                        }
                        break;
                    }
                }
            }
        }

        fn patchCalls(self: *Self, rule_addrs: []const u32, rules: []const Ast.Rule) void {
            for (self.patches[0..self.patch_len]) |patch| {
                for (rules, 0..) |rule, i| {
                    if (std.mem.eql(u8, rule.name, patch.name)) {
                        self.code[patch.addr] = .{
                            .op = .call,
                            .data = .{ .offset = rule_addrs[i] },
                        };
                        break;
                    }
                }
            }
        }

        fn compileNode(self: *Self, node: *const Ast.Node) Error!void {
            switch (node.*) {
                .char_val => |cv| {
                    for (cv.value) |b| {
                        try self.emit(.{ .op = .char, .data = .{ .byte = b } });
                    }
                    try self.maybeEmitTokenEvent(cv.value);
                },
                .any => try self.emit(.{ .op = .any }),
                .concatenation => |nodes| {
                    for (nodes) |*n| try self.compileNode(n);
                },
                .alternation => |alts| try self.compileAlternation(alts),
                .repetition => |rep| try self.compileRepetition(rep),
                .char_class => |ranges| {
                    const idx = try self.addCharset(ranges);
                    try self.emit(.{ .op = .set, .data = .{ .charset = idx } });
                },
                .neg_char_class => |ranges| {
                    const idx = try self.addCharset(ranges);
                    try self.emit(.{ .op = .neg_set, .data = .{ .charset = idx } });
                },
                .num_val => |nv| try self.compileNumVal(nv),
                .rulename => |name| {
                    if (self.patch_len >= config.max_patches) return Error.PatchBufferExhausted;
                    self.patches[self.patch_len] = .{
                        .addr = self.code_len,
                        .name = name,
                    };
                    self.patch_len += 1;
                    try self.emit(.{ .op = .fail });
                },
                .and_predicate => |inner| {
                    const outer = try self.emitPlaceholder();
                    const inner_choice = try self.emitPlaceholder();
                    try self.compileNode(inner);
                    const commit_addr = try self.emitPlaceholder();
                    try self.emit(.{ .op = .fail_twice });
                    self.code[inner_choice] = .{ .op = .choice, .data = .{ .offset = self.code_len - 1 } };
                    self.code[commit_addr] = .{ .op = .commit, .data = .{ .offset = self.code_len - 1 } };
                    self.code[outer] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
                    try self.emit(.{ .op = .fail_twice });
                },
                .not_predicate => |inner| {
                    const choice_addr = try self.emitPlaceholder();
                    try self.compileNode(inner);
                    try self.emit(.{ .op = .fail_twice });
                    self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
                },
                .capture => |inner| {
                    const slot = try self.captureSlotFor(node);
                    try self.emit(.{ .op = .save, .data = .{ .slot = slot } });
                    try self.compileNode(inner);
                    try self.emit(.{ .op = .save, .data = .{ .slot = slot + 1 } });
                },
                .throw_label => |name| {
                    const label_id = try self.labelIdFor(name);
                    try self.emit(.{ .op = .throw, .data = .{ .slot = label_id } });
                },
                .missing_label => |name| {
                    const label_id = try self.labelIdFor(name);
                    try self.emit(.{ .op = .event_missing, .data = .{ .slot = label_id } });
                },
                .field => |f| {
                    if (self.field_events) {
                        const fid = try self.fieldIdFor(f.name);
                        try self.emit(.{ .op = .event_field, .data = .{ .slot = fid } });
                    }
                    try self.compileNode(f.body);
                },
                .lcatch => |c| {
                    // Lowers to:
                    //   lcatch L -> handler_pc
                    //   <body>
                    //   commit -> end
                    // handler_pc:
                    //   event_error_open L            ; (only if not recover_missing)
                    //   <handler>
                    //   event_error_close L           ; (only if not recover_missing)
                    // end:
                    //
                    // When `handler.* == .missing_label`, the handler is the
                    // PEG `recover_missing` builtin: emit only the missing
                    // marker, no ERROR-node wrapping.
                    const label_id = try self.labelIdFor(c.label);
                    const lcatch_addr = try self.emitPlaceholder();
                    try self.compileNode(c.body);
                    const commit_addr = try self.emitPlaceholder();
                    const handler_pc = self.code_len;
                    const wrap_in_error_node = c.handler.* != .missing_label;
                    if (wrap_in_error_node) {
                        try self.emit(.{ .op = .event_error_open, .data = .{ .slot = label_id } });
                    }
                    try self.compileNode(c.handler);
                    if (wrap_in_error_node) {
                        try self.emit(.{ .op = .event_error_close, .data = .{ .slot = label_id } });
                    }
                    const end = self.code_len;
                    self.code[lcatch_addr] = .{
                        .op = .lcatch,
                        .data = .{ .catch_handler = .{ .label = label_id, .handler_pc = handler_pc } },
                    };
                    self.code[commit_addr] = .{ .op = .commit, .data = .{ .offset = end } };
                },
                .anchor_start, .anchor_end, .prose_val => {},
            }
        }

        /// Return the start-of-capture slot for this AST capture node,
        /// assigning a fresh group id on first visit. Subsequent visits
        /// of the same node (e.g. via a parent repetition compiled
        /// multiple times) reuse the same slot so `(a)+` is one group.
        fn captureSlotFor(self: *Self, node_ptr: *const Ast.Node) Error!u16 {
            for (self.capture_node_ptrs[0..self.capture_count], 0..) |stored, i| {
                if (stored == node_ptr) return @as(u16, @intCast(i)) * 2;
            }
            if (self.capture_count >= config.max_captures / 2) return Error.TooManyCaptures;
            const slot = self.capture_count * 2;
            self.capture_node_ptrs[self.capture_count] = node_ptr;
            self.capture_count += 1;
            return slot;
        }

        fn compileAlternation(self: *Self, alts: []const Ast.Node) Error!void {
            if (alts.len == 0) return;
            if (alts.len == 1) {
                try self.compileNode(&alts[0]);
                return;
            }

            var commits: [256]u32 = undefined;
            var commit_count: usize = 0;

            for (alts[0 .. alts.len - 1]) |*alt| {
                const choice_addr = try self.emitPlaceholder();
                try self.compileNode(alt);
                commits[commit_count] = try self.emitPlaceholder();
                commit_count += 1;
                self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
            }

            try self.compileNode(&alts[alts.len - 1]);

            const end = self.code_len;
            for (commits[0..commit_count]) |addr| {
                self.code[addr] = .{ .op = .commit, .data = .{ .offset = end } };
            }
        }

        fn compileRepetition(self: *Self, rep: Ast.Repetition) Error!void {
            for (0..rep.min) |_| {
                try self.compileNode(rep.element);
            }

            if (rep.max) |max| {
                const optional = max - rep.min;
                for (0..optional) |_| {
                    const choice_addr = try self.emitPlaceholder();
                    try self.compileNode(rep.element);
                    const commit_addr = try self.emitPlaceholder();
                    self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
                    self.code[commit_addr] = .{ .op = .commit, .data = .{ .offset = self.code_len } };
                }
            } else {
                const loop_start = self.code_len;
                const choice_addr = try self.emitPlaceholder();
                try self.compileNode(rep.element);
                try self.emit(.{ .op = .commit, .data = .{ .offset = loop_start } });
                self.code[choice_addr] = .{ .op = .choice, .data = .{ .offset = self.code_len } };
            }
        }

        fn compileNumVal(self: *Self, nv: Ast.NumVal) Error!void {
            switch (nv) {
                .single => |b| {
                    try self.emit(.{ .op = .char, .data = .{ .byte = b } });
                    const literal = [_]u8{b};
                    try self.maybeEmitTokenEvent(&literal);
                },
                .range => |r| {
                    const ranges = [_][2]u8{.{ r.lo, r.hi }};
                    const idx = try self.addCharsetFromRaw(&ranges);
                    try self.emit(.{ .op = .set, .data = .{ .charset = idx } });
                },
                .concat => |bytes| {
                    for (bytes) |b| {
                        try self.emit(.{ .op = .char, .data = .{ .byte = b } });
                    }
                    try self.maybeEmitTokenEvent(bytes);
                },
            }
        }

        /// Emit an `event_token` instruction immediately after a literal-
        /// matching opcode if the current `token_events` mode opts in for
        /// this literal. The `byte` operand encodes the literal length so
        /// the VM can compute `start = pos - len` after the literal
        /// succeeds. Literals longer than 255 bytes (or empty) skip
        /// emission since the operand is `u8`; in practice grammars
        /// don't write multi-hundred-byte literals.
        fn maybeEmitTokenEvent(self: *Self, literal: []const u8) Error!void {
            if (literal.len == 0 or literal.len > 255) return;
            const want = switch (self.token_events) {
                .off => false,
                .all => true,
                .tagged => blk: {
                    for (self.tagged_tokens) |t| {
                        if (std.mem.eql(u8, t, literal)) break :blk true;
                    }
                    break :blk false;
                },
            };
            if (!want) return;
            try self.emit(.{ .op = .event_token, .data = .{ .byte = @intCast(literal.len) } });
        }

        fn addCharset(self: *Self, ranges: []const Ast.ClassRange) Error!u16 {
            var cs = I.Charset{ 0, 0, 0, 0 };
            for (ranges) |r| {
                var b: u16 = r.lo;
                while (b <= r.hi) : (b += 1) {
                    const byte: u8 = @intCast(b);
                    const word = byte >> 6;
                    const bit: u6 = @truncate(byte);
                    cs[word] |= @as(u64, 1) << bit;
                }
            }
            return self.findOrAddCharset(cs);
        }

        fn addCharsetFromRaw(self: *Self, ranges: []const [2]u8) Error!u16 {
            return self.findOrAddCharset(I.charsetFromRanges(ranges));
        }

        fn findOrAddCharset(self: *Self, cs: I.Charset) Error!u16 {
            for (self.charsets[0..self.charset_len], 0..) |existing, i| {
                if (existing[0] == cs[0] and existing[1] == cs[1] and
                    existing[2] == cs[2] and existing[3] == cs[3])
                {
                    return @intCast(i);
                }
            }
            if (self.charset_len >= config.max_charsets) return Error.CharsetBufferExhausted;
            const idx = self.charset_len;
            self.charsets[idx] = cs;
            self.charset_len += 1;
            return idx;
        }

        fn emit(self: *Self, inst: I.Inst) Error!void {
            if (self.code_len >= config.max_code) return Error.CodeBufferExhausted;
            self.code[self.code_len] = inst;
            self.code_len += 1;
        }

        fn emitPlaceholder(self: *Self) Error!u32 {
            const addr = self.code_len;
            try self.emit(.{ .op = .jump, .data = .{ .offset = 0 } });
            return addr;
        }

        pub fn getCode(self: *const Self) []const I.Inst {
            return self.code[0..self.code_len];
        }

        pub fn getCharsets(self: *const Self) []const I.Charset {
            return self.charsets[0..self.charset_len];
        }

        pub fn getStringData(self: *const Self) []const u8 {
            return self.string_data[0..self.string_data_len];
        }

        pub fn getCaptureCount(self: *const Self) u16 {
            return self.capture_count;
        }

        pub fn getMemoRuleCount(self: *const Self) u16 {
            return self.memo_rule_count;
        }

        /// Per-memo-rule upper bound on read offset. Indexed by memo
        /// rule id, length `getMemoRuleCount()`. Empty when the
        /// grammar wasn't compiled with `memoize`.
        pub fn getExaminedMax(self: *const Self) []const u32 {
            return self.examined_max[0..self.memo_rule_count];
        }
    };
}
