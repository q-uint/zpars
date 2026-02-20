/// AST node types for ABNF grammars (RFC 5234).
///
/// Each node represents a construct in the ABNF syntax. The parser produces
/// a list of `Rule`s, each mapping a name to a `Node` tree.
const Ast = @This();

/// A named grammar rule: `rulename = elements`.
pub const Rule = struct {
    name: []const u8,
    node: Node,
};

/// A single node in the ABNF syntax tree.
pub const Node = union(enum) {
    /// `a / b / c` — one of the alternatives.
    alternation: []const Node,
    /// `a b c` — all elements in sequence.
    concatenation: []const Node,
    /// `[min]*[max] element` — bounded repetition.
    repetition: Repetition,
    /// `"quoted string"` — case-insensitive literal.
    char_val: []const u8,
    /// `%x41`, `%x41-5A`, or `%x41.42.43`.
    num_val: NumVal,
    /// `<prose description>` — free-form text.
    prose_val: []const u8,
    /// Reference to another rule by name.
    rulename: []const u8,
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
