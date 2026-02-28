# zpars LSP for Vim / Neovim

Language server providing diagnostics, semantic highlighting, and completion
for ABNF, BNF, PEG, and CFG grammar files.

## Build

```sh
zig build lsp
```

The binary is placed at `zig-out/bin/zpars-lsp`. Copy or symlink it to
somewhere on your `$PATH`.

## Vim (with vim-lsp)

```vim
if executable('zpars-lsp')
  au User lsp_setup call lsp#register_server(#{
    \ name: 'zpars',
    \ cmd: ['zpars-lsp'],
    \ allowlist: ['abnf', 'bnf', 'peg', 'cfg'],
    \ })
endif

au BufRead,BufNewFile *.abnf setfiletype abnf
au BufRead,BufNewFile *.bnf  setfiletype bnf
au BufRead,BufNewFile *.peg  setfiletype peg
```

## Features

- **Diagnostics**: parse errors shown inline as you type
- **Semantic tokens**: syntax highlighting via LSP semantic tokens
- **Completion**: rule name / identifier completion
