# zpars

An ABNF ([RFC 5234](https://www.rfc-editor.org/rfc/rfc5234)) parser and comptime parser combinator library written in Zig, with support for case-sensitive strings ([RFC 7405](https://www.rfc-editor.org/rfc/rfc7405)).

## Features

- **ABNF Parser** — Tokenizes and parses ABNF grammars into an AST, with structured error diagnostics and error recovery on malformed input.
- **Grammar Validator** — Detects duplicate rules, undefined references, unused rules, and unproductive cycles.
- **Comptime Combinators** — Zero-overhead parser combinator library resolved entirely at comptime.
- **ABNF-to-Combinator Compiler** — Compile ABNF grammar strings into combinator types at comptime. Define your grammar in standard ABNF and get a parser for free.

## Comptime ABNF Compiler

Define a grammar in ABNF and compile it to a parser type at comptime — zero runtime overhead:

```zig
const zpars = @import("zpars");

const HttpVersion = zpars.Abnf.Compile(
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
const Pair = zpars.Abnf.Compile(
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

## CLI

```
$ zpars grammar.abnf
greeting
hello
world
```

Syntax errors point to the exact location:

```
grammar.abnf:1:12: error: expected element, found ')'
   foo = (a / )
              ^
```

Semantic issues are reported as warnings or errors:

```
grammar.abnf: warning: rule 'helper' is defined but never referenced
grammar.abnf: error: rule 'start' references undefined rule 'missing'
grammar.abnf: error: rule 'a' is unproductive (circular with no terminal escape)
```

## Building

Requires Zig 0.15.2+.

```
zig build                      # build the executable
zig build run -- grammar.abnf  # run on a file
zig build test                 # run all tests
```
