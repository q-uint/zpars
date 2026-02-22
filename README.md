# zpars

A grammar parser toolkit written in Zig, supporting [ABNF](https://www.rfc-editor.org/rfc/rfc5234) (RFC 5234, RFC 7405) and [BNF](https://softwarepreservation.computerhistory.org/ALGOL/report/Algol60_report_CACM_1960_June.pdf) (ALGOL 60) with comptime parser combinators.

## Features

- **ABNF Parser** — Tokenizes and parses ABNF grammars into an AST, with structured error diagnostics and error recovery on malformed input.
- **BNF Parser** — Tokenizes and parses BNF grammars (ALGOL 60 variant) into the same shared AST.
- **Grammar Validator** — Detects duplicate rules, undefined references, unused rules, and unproductive cycles.
- **Runtime Matcher** — Match input strings against any rule in a dynamically loaded grammar.
- **Formatter** — Pretty-print parsed grammars back to canonical ABNF or BNF with aligned operators.
- **Comptime Combinators** — Zero-overhead parser combinator library resolved entirely at comptime.
- **ABNF-to-Combinator Compiler** — Compile ABNF grammar strings into combinator types at comptime. Define your grammar in standard ABNF and get a parser for free.

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
const matcher = zpars.Matcher.init(merged);

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

## Building

Requires Zig 0.15.2+.

```
zig build                      # build the executable
zig build test                 # run all tests
```

## References

- [RFC 5234 — Augmented BNF for Syntax Specifications: ABNF](https://www.rfc-editor.org/rfc/rfc5234)
- [RFC 7405 — Case-Sensitive String Support in ABNF](https://www.rfc-editor.org/rfc/rfc7405)
- [Report on the Algorithmic Language ALGOL 60 (1960)](https://softwarepreservation.computerhistory.org/ALGOL/report/Algol60_report_CACM_1960_June.pdf) — original BNF definition (Section 1.1)
