/// Shared AST node types for grammar parsers (ABNF, BNF, etc.).
///
/// Each node represents a construct in a grammar's syntax. The parser produces
/// a list of `Rule`s, each mapping a name to a `Node` tree.
const Ast = @This();

/// A named grammar rule: `rulename = elements`.
pub const Rule = struct {
    name: []const u8,
    node: Node,
    /// True when defined with `=/` (incremental alternation).
    incremental: bool,
};

/// A single node in the grammar syntax tree.
pub const Node = union(enum) {
    /// `a / b / c` - one of the alternatives.
    alternation: []const Node,
    /// `a b c` - all elements in sequence.
    concatenation: []const Node,
    /// `[min]*[max] element` - bounded repetition.
    repetition: Repetition,
    /// Quoted string literal (RFC 5234 + RFC 7405).
    char_val: CharVal,
    /// `%x41`, `%x41-5A`, or `%x41.42.43`.
    num_val: NumVal,
    /// `<prose description>` - free-form text.
    prose_val: []const u8,
    /// Reference to another rule by name.
    rulename: []const u8,
    /// `&e` - positive lookahead (match without consuming).
    and_predicate: *const Node,
    /// `!e` - negative lookahead (succeed if e fails, consume nothing).
    not_predicate: *const Node,
    /// `[a-zA-Z0-9]` - character class with ranges and singles.
    char_class: []const ClassRange,
    /// `[^a-zA-Z0-9]` - negated character class (matches any char NOT in ranges).
    neg_char_class: []const ClassRange,
    /// `^` - anchor: match only at the start of input.
    anchor_start,
    /// `$` - anchor: match only at the end of input.
    anchor_end,
    /// `.` - match any single character.
    any,
    /// Capture group: records start/end positions of the inner match.
    capture: *const Node,
    /// Throw a labeled failure. Always fails non-recoverably for `fail`-style
    /// backtracking; the runtime unwinds past `choice` frames until a matching
    /// `lcatch` is found. Produced by the PEG front-end from `#@ throw
    /// <label>` directives, or constructed directly by programmatic AST
    /// users. The compiler resolves `label` to a label id at lowering time.
    throw_label: []const u8,
    /// Wrap `body` in a labeled-failure catch keyed to `label`. On a matching
    /// throw inside `body`, control transfers to `handler` with the input
    /// position left at the throw site. `handler` runs as a normal expression;
    /// if `handler` itself fails normally, the outer scope sees the throw
    /// propagate further. The compiler emits an error_open / error_close pair
    /// around `handler` so the recovered region surfaces as an ERROR node.
    lcatch: Lcatch,
    /// Emit a zero-width MISSING(label) marker into the event log. Always
    /// succeeds; consumes no input. Typically appears as the body of a
    /// recovery handler (or via the `recover_missing` builtin handler form
    /// in PEG).
    missing_label: []const u8,
};

/// Payload for `Node.lcatch`.
pub const Lcatch = struct {
    label: []const u8,
    body: *const Node,
    handler: *const Node,
};

/// A character class range entry (e.g. `a-z` or a single `_`).
pub const ClassRange = struct {
    lo: u8,
    hi: u8, // lo == hi for single characters
};

/// A quoted string literal with case-sensitivity (RFC 5234 + RFC 7405).
/// - `"text"` and `%i"text"` are case-insensitive.
/// - `%s"text"` is case-sensitive.
pub const CharVal = struct {
    value: []const u8,
    case_sensitive: bool,
};

/// Repetition operator: `*element`, `3*5element`, `1*element`, etc.
pub const Repetition = struct {
    /// Minimum number of occurrences (default 0).
    min: usize,
    /// Maximum number of occurrences, or null for unbounded.
    max: ?usize,
    /// The repeated element.
    element: *const Node,
};

/// A numeric value literal in one of three forms.
pub const NumVal = union(enum) {
    /// Single value, e.g. `%x41`.
    single: u8,
    /// Inclusive range, e.g. `%x41-5A`.
    range: struct { lo: u8, hi: u8 },
    /// Concatenated values, e.g. `%x41.42.43`.
    concat: []const u8,
};
