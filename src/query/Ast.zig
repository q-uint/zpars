/// AST and compiled-query representation for tree-sitter-style queries.
///
/// A `Query` owns its AST through an arena. Rule names referenced by the
/// query are pre-resolved at compile time against a `Names` table and
/// stored as numeric ids; this matches how the VM/CaptureTree side
/// represents nodes (group_id + Names table) and avoids per-match string
/// comparisons during traversal.
const std = @import("std");
const vm_inst = @import("../vm/Instruction.zig");

/// One quantified, optionally captured pattern.
///
/// In tree-sitter syntax, a capture and quantifier are *postfix* on
/// whatever pattern they apply to (`(Foo)? @x`). We hoist them to this
/// outer wrapper so any pattern shape -- node, alternation -- can carry
/// them uniformly.
pub const Pattern = struct {
    body: Body,
    quantifier: Quantifier = .one,
    /// Capture index into `Query.capture_names`. `null` means "no
    /// `@binding` on this pattern".
    capture: ?u16 = null,

    pub const Body = union(enum) {
        /// `(Foo ...)`, `(_ ...)`, `(ERROR ...)`, `(MISSING ...)`.
        node: NodePattern,
        /// `[ p1 p2 ... ]` -- match any of the alternatives.
        alt: []const Pattern,
        /// `(p1 p2 ... (#pred? ...))` -- a "grouping" with no head
        /// identifier. Two semantic shapes, distinguished by the
        /// number of inner patterns and the presence of anchors:
        ///   - Exactly one pattern child (no anchors): the group
        ///     matches the visited node like a normal pattern; the
        ///     predicates scope to the captures bound on that match.
        ///     Common form: `((Rule) @cap (#pred? @cap ...))`.
        ///   - More than one pattern child OR any anchor: the group's
        ///     children sequence is matched against the visited node's
        ///     children list using the same gap-allowed positional
        ///     semantics as a node body.
        group: GroupPattern,
    };
};

pub const GroupPattern = struct {
    children: []const Child,
    predicates: []const Predicate,
};

pub const Quantifier = enum {
    /// No quantifier (must match exactly once).
    one,
    /// `?`
    optional,
    /// `*`
    zero_or_more,
    /// `+`
    one_or_more,
};

pub const NodePattern = struct {
    kind: NodeKindMatch,
    /// Three-valued partial filter:
    ///   `null` -- match either `.rule` or `.rule_partial` (default).
    ///   `true` -- match only `.rule_partial`.
    ///   `false` -- match only `.rule` (set by absence of `partial` modifier
    ///              in a future "strict" mode; unused for now).
    /// Only meaningful when `kind` is `.rule_named` or `.any`.
    partial: ?bool = null,
    children: []const Child = &.{},
    predicates: []const Predicate = &.{},
};

pub const NodeKindMatch = union(enum) {
    /// `(_ ...)` -- any node kind, including ERROR/MISSING/token.
    any,
    /// `(RuleName ...)` -- match `.rule`/`.rule_partial` with this rule id.
    rule_named: u16,
    /// `(ERROR ...)` -- match `.error_node`.
    error_kind,
    /// `(MISSING ...)` -- match `.missing_node`.
    missing_kind,
    /// `"literal"` (a bare string atom) -- match a `.token` node whose
    /// text equals these bytes. Set by the parser when an atom is a
    /// quoted string with no head identifier.
    token_text: []const u8,
};

/// A child slot inside a node pattern's body. Plain patterns match an
/// actual child; anchors `.` constrain adjacency between surrounding
/// patterns and never match a node themselves; field-tagged patterns
/// only match if the candidate child carries the named field id.
pub const Child = union(enum) {
    pattern: Pattern,
    anchor,
    field_pattern: FieldPattern,
};

pub const FieldPattern = struct {
    /// Field-id index resolved against the names table at compile time.
    field_id: u16,
    pattern: Pattern,
};

pub const Predicate = struct {
    /// Predicate name with the leading `#` and trailing `?`/`!` stripped
    /// (e.g. `"eq"`, `"match"`, `"not-eq"`).
    name: []const u8,
    /// Whether the predicate was written with `?` (a query) or `!` (a
    /// directive). Affects how a matcher should treat its return value.
    suffix: Suffix,
    args: []const Arg,
    /// For `#match?`/`#not-match?`: precompiled ERE bytecode for the
    /// regex string argument. Null for other predicate kinds.
    compiled_regex: ?CompiledRegex = null,

    pub const Suffix = enum { question, bang };
};

/// ERE regex compiled to VM bytecode at query-compile time. Lifetime:
/// owned by the enclosing `Query`'s arena.
pub const CompiledRegex = struct {
    code: []const vm_inst.Inst,
    charsets: []const vm_inst.Charset,
    string_data: []const u8,
};

pub const Arg = union(enum) {
    /// `@name` reference -- index into `Query.capture_names`.
    capture: u16,
    /// `"literal"` string (with escapes already decoded).
    string: []const u8,
};

/// A compiled query: a forest of top-level patterns plus the capture-name
/// table they reference. The arena owns `patterns` and `capture_names`;
/// the `Query` struct itself is allocated from the parent allocator so
/// it survives `arena.deinit()` long enough to free the arena.
pub const Query = struct {
    arena: *std.heap.ArenaAllocator,
    patterns: []const Pattern,
    capture_names: []const []const u8,

    pub fn deinit(self: *Query) void {
        const child_alloc = self.arena.child_allocator;
        const arena = self.arena;
        arena.deinit();
        child_alloc.destroy(arena);
        child_alloc.destroy(self);
    }

    pub fn captureName(self: *const Query, idx: u16) []const u8 {
        return self.capture_names[idx];
    }
};
