# zpars Open VSX Extension

Syntax highlighting for ABNF, BNF, PEG, and CFG grammars, powered by the
zpars scanners compiled to WebAssembly.

Install from [Open VSX](https://open-vsx.org/extension/q-uint/zpars), or via
the command line:

```bash
code --install-extension q-uint.zpars
```

## Building

From the repository root:

```bash
# Build the WASM module
nix develop --command zig build wasm

# Install dependencies and compile the extension
cd editors/vsx
nix develop ../../ --command npm install
nix develop ../../ --command npm run compile

# Or build everything in one step
nix develop --command zig build vsx
```

## Testing

1. Open the repository root in VSCode.
2. Open the **Run and Debug** panel (Cmd+Shift+D).
3. Select **"Run zpars Extension"** and press F5.
4. In the Extension Development Host window, open a grammar file
   (e.g. `examples/rfc5234_a.abnf` or `examples/grammar.peg`).

To verify semantic tokens are being applied, run **"Developer: Inspect
Editor Tokens and Scopes"** from the Command Palette (Cmd+Shift+P) and
click on any token.

## Supported File Types

| Extension | Language |
|-----------|----------|
| `.abnf`   | ABNF (RFC 5234 / 7405) |
| `.bnf`    | BNF (ALGOL 60 variant) |
| `.peg`    | PEG (Parsing Expression Grammar) |
| `.cfg`    | CFG (Context-Free Grammar) |
