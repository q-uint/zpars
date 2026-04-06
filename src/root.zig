// zpars - parser playground in Zig
pub const abnf = struct {
    pub const Compiler = @import("abnf/Compiler.zig");
    pub const Diagnostic = @import("abnf/Diagnostic.zig");
    pub const Formatter = @import("abnf/Formatter.zig");
    pub const Parser = @import("abnf/Parser.zig");
    const scanner_mod = @import("abnf/Scanner.zig");
    pub const Scanner = scanner_mod.Scanner;
    pub const ScannerWith = scanner_mod.ScannerWith;
    pub const ScannerConfig = scanner_mod.Config;
    pub const Tokenizer = @import("abnf/Tokenizer.zig");
    pub const Token = @import("abnf/Token.zig");
};
pub const bnf = struct {
    pub const Diagnostic = @import("bnf/Diagnostic.zig");
    pub const Formatter = @import("bnf/Formatter.zig");
    pub const Parser = @import("bnf/Parser.zig");
    pub const Scanner = @import("bnf/Scanner.zig").Scanner;
};
pub const peg = struct {
    pub const Diagnostic = @import("peg/Diagnostic.zig");
    pub const Formatter = @import("peg/Formatter.zig");
    pub const Parser = @import("peg/Parser.zig");
    const scanner_mod = @import("peg/Scanner.zig");
    pub const Scanner = scanner_mod.Scanner;
    pub const ScannerWith = scanner_mod.ScannerWith;
    pub const ScannerConfig = scanner_mod.Config;
    pub const Token = @import("peg/Token.zig");
};
pub const cfg = struct {
    // CnfBuilder is intentionally not re-exported: it is a comptime-only
    // helper used internally by Cfg.toCnf, and exposing it publicly
    // causes `refAllDeclsRecursive` (in our root test block) to analyze
    // its runtime surface, which breaks comptime-only helpers like
    // `std.fmt.comptimePrint` used inside its build steps.
    pub const Diagnostic = @import("cfg/Diagnostic.zig");
    pub const Parser = @import("cfg/Parser.zig");
    const scanner_mod = @import("cfg/Scanner.zig");
    pub const Scanner = scanner_mod.Scanner;
    pub const ScannerWith = scanner_mod.ScannerWith;
    pub const ScannerConfig = scanner_mod.Config;
    pub const Token = @import("cfg/Token.zig");
};
pub const ere = struct {
    pub const Diagnostic = @import("ere/Diagnostic.zig");
    pub const Formatter = @import("ere/Formatter.zig");
    pub const Parser = @import("ere/Parser.zig");
    const scanner_mod = @import("ere/Scanner.zig");
    pub const Scanner = scanner_mod.Scanner;
    pub const ScannerWith = scanner_mod.ScannerWith;
    pub const ScannerConfig = scanner_mod.Config;
    pub const Token = @import("ere/Token.zig");
};
pub const sexp = @import("sexp.zig");
pub const vm = struct {
    pub const Aot = @import("vm/Aot.zig");
    pub const AotRuntime = @import("vm/AotRuntime.zig");
    pub const Compiler = @import("vm/Compiler.zig");
    pub const Disassembler = @import("vm/Disassembler.zig");
    pub const Instruction = @import("vm/Instruction.zig");
    pub const Jit = @import("vm/Jit.zig");
    pub const Vm = @import("vm/Vm.zig");
};
pub const Ast = @import("Ast.zig");
pub const Cfg = @import("Cfg.zig");
pub const Matcher = @import("Matcher.zig");
pub const Validator = @import("Validator.zig");
pub const combinators = @import("combinators.zig");
pub const diagnostic = @import("diagnostic.zig");
pub const token = @import("token.zig");

test {
    // Recursively reference every public decl in this module so that
    // tests in files re-exported via `pub const X = @import(...)` are
    // actually compiled and run. Without this, zig's test runner only
    // pulls in tests transitively reachable from `_ = @import(...)`
    // statements, silently skipping many files.
    @import("std").testing.refAllDeclsRecursive(@This());
    // CnfBuilder is not exposed in the public API above (see comment on
    // `cfg` namespace). Pull its tests in explicitly so they still run.
    _ = @import("cfg/CnfBuilder.zig");
}
