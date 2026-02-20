# zpars

An ABNF ([RFC 5234](https://www.rfc-editor.org/rfc/rfc5234)) parser written in Zig.

Tokenizes and parses ABNF grammars into an AST, with structured error diagnostics on malformed input.

## Example

Given `grammar.abnf`:

```abnf
greeting = hello SP world
hello    = "Hello"
world    = "World"
```

```
$ zpars grammar.abnf
greeting
hello
world
```

On syntax errors:

```
grammar.abnf:1:12: error: expected element, found ')'
   foo = (a / )
              ^
```

## Building

Requires Zig 0.15.2+.

```
zig build                      # build the executable
zig build run -- grammar.abnf  # run on a file
zig build test                 # run all tests
```
