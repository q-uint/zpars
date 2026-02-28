<p align="center">
  <img src="icon.png" width="128" height="128" alt="zpars logo">
  <h1 align="center">zpars</h1>
  <p align="center">
    A grammar parser toolkit written in Zig — ABNF, BNF, PEG, CFG, and S-expressions<br>
    with comptime parser combinators and zero-overhead compiled grammars.
  </p>
  <p align="center">
    <a href="https://ziglang.org/download/"><img src="https://img.shields.io/badge/zig-0.15.2+-f7a41d?logo=zig&logoColor=white" alt="Zig 0.15.2+"></a>
    <a href="https://open-vsx.org/extension/q-uint/zpars"><img src="https://img.shields.io/open-vsx/v/q-uint/zpars?color=c160ef" alt="Open VSX"></a>
    <a href="LICENSE"><img src="https://img.shields.io/badge/license-MPL--2.0-blue" alt="License: MPL-2.0"></a>
  </p>
</p>

## Features

- **Multi-format parsing** — ABNF, BNF, PEG, CFG, and S-expression grammars, each with tokenizer, parser, and error diagnostics.
- **Validation** — Detects duplicate rules, undefined references, unused rules, and unproductive cycles.
- **Runtime matching** — Match input strings against any rule in a dynamically loaded grammar.
- **Formatting** — Pretty-print grammars back to canonical form with aligned operators.
- **Comptime combinators** — Zero-overhead parser combinator library resolved entirely at comptime.
- **ABNF compiler** — Compile ABNF grammar strings into combinator types at comptime — define your grammar in standard ABNF and get a parser for free.

## Comptime ABNF Compiler

Define a grammar in ABNF and compile it to a parser type at comptime — zero runtime overhead:

```zig
const zpars = @import("zpars");

const HttpVersion = zpars.abnf.Compiler.Compile(
   \\version = "HTTP/" 1*DIGIT "." 1*DIGIT
, "version");

test "parse HTTP version" {
   const r = HttpVersion.parse("HTTP/1.1 OK").?;
   try std.testing.expectEqualStrings("HTTP/1.1", r.value);
   try std.testing.expectEqualStrings(" OK", r.rest);
}
```

Multi-rule grammars with cross-references work too:

```zig
const Pair = zpars.abnf.Compiler.Compile(
   \\number = 1*DIGIT
   \\pair   = number "," number
, "pair");

const r = Pair.parse("42,7!").?;
// r.value == "42,7", r.rest == "!"
```

All 16 RFC 5234 core rules (ALPHA, DIGIT, SP, CRLF, etc.) are available implicitly.

## Parser Combinators

The combinator library can also be used directly:

```zig
const c = zpars.combinators;

const Digit = c.CharRange('0', '9');
const Number = c.Capture(c.Many(Digit, .{ .min = 1 }));
const P = c.Sequence(Number, c.Sequence(c.Literal(","), Number));

const r = c.Capture(P).parse("42,7!").?;
// r.value == "42,7"
```

Available primitives: `Literal`, `Char`, `CharRange`, `ByteLiteral`, `CaseInsensitiveLiteral`, `Any`, `Eof`.

Available combinators: `Sequence`, `Choice`, `Many`, `Optional`, `Map`, `Capture`.

## Runtime Matcher

For grammars loaded at runtime, use the `Matcher`:

```zig
const zpars = @import("zpars");

var scanner = zpars.abnf.Scanner.init(grammar);
const tokens = scanner.scanTokens();
var parser = zpars.abnf.Parser.init(tokens, grammar);
const rules = try parser.parse();
var validator = zpars.Validator.init(allocator, rules);
const merged = try validator.validate();
const matcher = zpars.Matcher.init(allocator, merged);

const r = matcher.match("version", "HTTP/1.1 OK").?;
// r.value == "HTTP/1.1", r.rest == " OK"
```

## CLI

```
zpars check <file>                       # validate a grammar
zpars fmt   <file>                       # format a grammar
zpars match -r <rule> <file> <input>     # match input against a rule
```

### check

Validate an ABNF grammar, reporting syntax errors and semantic issues:

```
$ zpars check grammar.abnf
grammar.abnf:1:12: error: expected element, found ')'
   foo = (a / )
              ^
```

```
$ zpars check grammar.abnf
grammar.abnf: warning: rule 'helper' is defined but never referenced
grammar.abnf: error: rule 'start' references undefined rule 'missing'
```

### fmt

Parse and reformat a grammar with aligned `=` signs:

```
$ zpars fmt grammar.abnf
number = 1*DIGIT
pair   = number "," number
```

### match

Match an input string against a rule:

```
$ zpars match -r version grammar.abnf "HTTP/1.1 OK"
HTTP/1.1
```

## Editor Support

The [VSCode extension](https://open-vsx.org/extension/q-uint/zpars) provides semantic highlighting for ABNF, BNF, PEG, CFG, and S-expression grammars, powered by the zpars WASM build.

Install from [Open VSX](https://open-vsx.org/extension/q-uint/zpars).

## Building

Requires Zig 0.15.2+.

```
zig build                      # build the executable
zig build test                 # run all tests
zig build vsx                  # build the Open VSX extension (WASM + TypeScript)
```

## References

- [RFC 5234 — Augmented BNF for Syntax Specifications: ABNF](https://www.rfc-editor.org/rfc/rfc5234)
- [RFC 7405 — Case-Sensitive String Support in ABNF](https://www.rfc-editor.org/rfc/rfc7405)
- [Report on the Algorithmic Language ALGOL 60 (1960)](https://softwarepreservation.computerhistory.org/ALGOL/report/Algol60_report_CACM_1960_June.pdf) — original BNF definition (Section 1.1)
- [Parsing Expression Grammars: A Recognition-Based Syntactic Foundation (2004)](https://bford.info/pub/lang/peg.pdf) — Bryan Ford's PEG paper
- [RFC 9804 — S-Expressions](https://www.rfc-editor.org/rfc/rfc9804)
