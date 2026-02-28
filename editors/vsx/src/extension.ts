import * as vscode from "vscode";
import * as path from "path";
import * as fs from "fs";

// Semantic token types used by this extension.
const tokenTypes = [
  "type", // rule names / identifiers
  "string", // string literals, prose values
  "number", // numeric values
  "comment", // comments
  "operator", // operators, definition symbols
  "regexp", // character classes
];

const tokenLegend = new vscode.SemanticTokensLegend(tokenTypes);

// Token tag → semantic type index mappings per grammar format.
// Values are indices into `tokenTypes` above, or -1 to skip.

// ABNF Tag enum order (from Token.zig):
//   0: left_paren, 1: right_paren, 2: left_bracket, 3: right_bracket,
//   4: slash, 5: star, 6: equals, 7: equals_slash,
//   8: rulename, 9: number, 10: char_val, 11: char_val_ci,
//   12: char_val_cs, 13: prose_val, 14: bin_val, 15: dec_val,
//   16: hex_val, 17: comment, 18: newline, 19: eof, 20: invalid
const abnfTagMap: number[] = [
  -1, // left_paren
  -1, // right_paren
  -1, // left_bracket
  -1, // right_bracket
  4, // slash → operator
  4, // star → operator
  4, // equals → operator
  4, // equals_slash → operator
  0, // rulename → type
  2, // number → number
  1, // char_val → string
  1, // char_val_ci → string
  1, // char_val_cs → string
  1, // prose_val → string
  2, // bin_val → number
  2, // dec_val → number
  2, // hex_val → number
  3, // comment → comment
  -1, // newline
  -1, // eof
  -1, // invalid
];

// BNF Tag enum order:
//   0: rulename, 1: definition, 2: pipe, 3: terminal,
//   4: newline, 5: eof, 6: invalid
const bnfTagMap: number[] = [
  0, // rulename → type
  4, // definition → operator
  4, // pipe → operator
  1, // terminal → string
  -1, // newline
  -1, // eof
  -1, // invalid
];

// PEG Tag enum order:
//   0: identifier, 1: left_arrow, 2: slash, 3: and, 4: not,
//   5: question, 6: star, 7: plus, 8: left_paren, 9: right_paren,
//   10: dot, 11: literal, 12: char_class, 13: comment,
//   14: newline, 15: eof, 16: invalid
const pegTagMap: number[] = [
  0, // identifier → type
  4, // left_arrow → operator
  4, // slash → operator
  4, // and → operator
  4, // not → operator
  4, // question → operator
  4, // star → operator
  4, // plus → operator
  -1, // left_paren
  -1, // right_paren
  4, // dot → operator
  1, // literal → string
  5, // char_class → regexp
  3, // comment → comment
  -1, // newline
  -1, // eof
  -1, // invalid
];

// CFG Tag enum order:
//   0: identifier, 1: string, 2: string_cs, 3: string_ci,
//   4: hex_byte, 5: hex_range, 6: arrow, 7: pipe,
//   8: newline, 9: eof, 10: invalid
const cfgTagMap: number[] = [
  0, // identifier → type
  1, // string → string
  1, // string_cs → string
  1, // string_ci → string
  2, // hex_byte → number
  2, // hex_range → number
  4, // arrow → operator
  4, // pipe → operator
  -1, // newline
  -1, // eof
  -1, // invalid
];

// Language tags matching the WASM Language enum.
const enum Language {
  abnf = 0,
  bnf = 1,
  peg = 2,
  cfg = 3,
}

interface WasmExports {
  memory: WebAssembly.Memory;
  alloc(len: number): number;
  free(ptr: number, len: number): void;
  analyze(lang: number, ptr: number, len: number): number;
}

let wasmExports: WasmExports | undefined;

async function loadWasm(
  context: vscode.ExtensionContext
): Promise<WasmExports> {
  const wasmPath = path.join(context.extensionPath, "wasm", "zpars.wasm");
  const wasmBytes = fs.readFileSync(wasmPath);
  const wasmModule = await WebAssembly.compile(wasmBytes);
  const instance = await WebAssembly.instantiate(wasmModule);
  return instance.exports as unknown as WasmExports;
}

interface ScannedToken {
  tag: number;
  start: number;
  len: number;
}

interface RawDiagnostic {
  start: number;
  len: number;
  message: string;
}

interface AnalyzeResult {
  tokens: ScannedToken[];
  diagnostics: RawDiagnostic[];
}

function analyzeText(text: string, lang: Language): AnalyzeResult {
  const encoder = new TextEncoder();
  const encoded = encoder.encode(text);
  if (encoded.length === 0) return { tokens: [], diagnostics: [] };

  const exports = wasmExports!;

  const srcPtr = exports.alloc(encoded.length);
  if (srcPtr === 0) return { tokens: [], diagnostics: [] };
  const mem = new Uint8Array(exports.memory.buffer);
  mem.set(encoded, srcPtr);

  const resultPtr = exports.analyze(lang, srcPtr, encoded.length);
  exports.free(srcPtr, encoded.length);

  if (resultPtr === 0) return { tokens: [], diagnostics: [] };

  const view = new DataView(exports.memory.buffer);
  const tokenCount = view.getUint32(resultPtr, true);
  const diagCount = view.getUint32(resultPtr + 4, true);

  const headerLen = 8;

  // Decode tokens.
  const tokens: ScannedToken[] = [];
  for (let i = 0; i < tokenCount; i++) {
    const off = resultPtr + headerLen + i * 12;
    tokens.push({
      tag: view.getUint32(off, true),
      start: view.getUint32(off + 4, true),
      len: view.getUint32(off + 8, true),
    });
  }

  // Decode diagnostics.
  const tokenDataLen = tokenCount * 12;
  const diagDataLen = diagCount * 16;
  const msgDataStart = resultPtr + headerLen + tokenDataLen + diagDataLen;
  const decoder = new TextDecoder();
  const diagnostics: RawDiagnostic[] = [];

  for (let i = 0; i < diagCount; i++) {
    const off = resultPtr + headerLen + tokenDataLen + i * 16;
    const start = view.getUint32(off, true);
    const len = view.getUint32(off + 4, true);
    const msgOff = view.getUint32(off + 8, true);
    const msgLen = view.getUint32(off + 12, true);

    const msgBytes = new Uint8Array(exports.memory.buffer, msgDataStart + msgOff, msgLen);
    const message = decoder.decode(msgBytes);
    diagnostics.push({ start, len, message });
  }

  // Free the result buffer.
  const resultLen = headerLen + tokenDataLen + diagDataLen +
    diagnostics.reduce((sum, d) => sum + new TextEncoder().encode(d.message).length, 0);
  exports.free(resultPtr as unknown as number, resultLen);

  return { tokens, diagnostics };
}

type BracketPair = [number, number]; // [openTag, closeTag]

class ZparsSemanticTokensProvider
  implements vscode.DocumentSemanticTokensProvider {
  constructor(
    private lang: Language,
    private tagMap: number[]
  ) { }

  provideDocumentSemanticTokens(
    document: vscode.TextDocument
  ): vscode.SemanticTokens {
    const { tokens } = analyzeText(document.getText(), this.lang);
    const builder = new vscode.SemanticTokensBuilder(tokenLegend);

    for (const tok of tokens) {
      const semanticType = tok.tag < this.tagMap.length ? this.tagMap[tok.tag] : -1;
      if (semanticType < 0) continue;

      const pos = document.positionAt(tok.start);
      builder.push(
        new vscode.Range(pos, document.positionAt(tok.start + tok.len)),
        tokenTypes[semanticType]
      );
    }

    return builder.build();
  }
}

class ZparsBracketHighlightProvider
  implements vscode.DocumentHighlightProvider {
  constructor(
    private lang: Language,
    private bracketPairs: BracketPair[]
  ) { }

  provideDocumentHighlights(
    document: vscode.TextDocument,
    position: vscode.Position
  ): vscode.DocumentHighlight[] | undefined {
    const { tokens } = analyzeText(document.getText(), this.lang);
    const offset = document.offsetAt(position);

    // Find the token at the cursor position.
    const idx = tokens.findIndex(
      (t) => offset >= t.start && offset < t.start + t.len
    );
    if (idx < 0) return undefined;

    const tok = tokens[idx];

    for (const [openTag, closeTag] of this.bracketPairs) {
      if (tok.tag === openTag) {
        // Search forward for matching close.
        let depth = 1;
        for (let i = idx + 1; i < tokens.length; i++) {
          if (tokens[i].tag === openTag) depth++;
          else if (tokens[i].tag === closeTag) depth--;
          if (depth === 0) {
            return [
              this.highlight(document, tok),
              this.highlight(document, tokens[i]),
            ];
          }
        }
        return undefined;
      }
      if (tok.tag === closeTag) {
        // Search backward for matching open.
        let depth = 1;
        for (let i = idx - 1; i >= 0; i--) {
          if (tokens[i].tag === closeTag) depth++;
          else if (tokens[i].tag === openTag) depth--;
          if (depth === 0) {
            return [
              this.highlight(document, tokens[i]),
              this.highlight(document, tok),
            ];
          }
        }
        return undefined;
      }
    }

    return undefined;
  }

  private highlight(
    document: vscode.TextDocument,
    tok: ScannedToken
  ): vscode.DocumentHighlight {
    return new vscode.DocumentHighlight(
      new vscode.Range(
        document.positionAt(tok.start),
        document.positionAt(tok.start + tok.len)
      ),
      vscode.DocumentHighlightKind.Text
    );
  }
}

class ZparsCompletionProvider
  implements vscode.CompletionItemProvider {
  constructor(
    private lang: Language,
    private ruleNameTag: number
  ) { }

  provideCompletionItems(
    document: vscode.TextDocument
  ): vscode.CompletionItem[] {
    const { tokens } = analyzeText(document.getText(), this.lang);
    const seen = new Set<string>();
    const text = document.getText();

    for (const tok of tokens) {
      if (tok.tag === this.ruleNameTag) {
        seen.add(text.slice(tok.start, tok.start + tok.len));
      }
    }

    return Array.from(seen).map((name) => {
      const item = new vscode.CompletionItem(name, vscode.CompletionItemKind.Variable);
      item.detail = "rule";
      return item;
    });
  }
}

export async function activate(
  context: vscode.ExtensionContext
): Promise<void> {
  try {
    wasmExports = await loadWasm(context);
  } catch (err) {
    vscode.window.showErrorMessage(
      `zpars: failed to load WASM module: ${err}`
    );
    return;
  }

  const languages: {
    id: string;
    lang: Language;
    tagMap: number[];
    ruleNameTag: number;
    bracketPairs: BracketPair[];
  }[] = [
      {
        id: "abnf",
        lang: Language.abnf,
        tagMap: abnfTagMap,
        ruleNameTag: 8,
        bracketPairs: [[0, 1], [2, 3]], // ()/[]
      },
      {
        id: "bnf",
        lang: Language.bnf,
        tagMap: bnfTagMap,
        ruleNameTag: 0,
        bracketPairs: [],
      },
      {
        id: "peg",
        lang: Language.peg,
        tagMap: pegTagMap,
        ruleNameTag: 0,
        bracketPairs: [[8, 9]], // ()
      },
      {
        id: "cfg",
        lang: Language.cfg,
        tagMap: cfgTagMap,
        ruleNameTag: 0,
        bracketPairs: [],
      },
    ];

  const diagnosticCollection = vscode.languages.createDiagnosticCollection("zpars");
  const langByDocType = new Map<string, Language>();

  for (const lang of languages) {
    langByDocType.set(lang.id, lang.lang);
    const selector = { language: lang.id };
    context.subscriptions.push(
      vscode.languages.registerDocumentSemanticTokensProvider(
        selector,
        new ZparsSemanticTokensProvider(lang.lang, lang.tagMap),
        tokenLegend
      ),
      vscode.languages.registerCompletionItemProvider(
        selector,
        new ZparsCompletionProvider(lang.lang, lang.ruleNameTag)
      )
    );
    if (lang.bracketPairs.length > 0) {
      context.subscriptions.push(
        vscode.languages.registerDocumentHighlightProvider(
          selector,
          new ZparsBracketHighlightProvider(lang.lang, lang.bracketPairs)
        )
      );
    }
  }

  function updateDiagnostics(document: vscode.TextDocument): void {
    const lang = langByDocType.get(document.languageId);
    if (lang === undefined) return;
    const { diagnostics } = analyzeText(document.getText(), lang);
    diagnosticCollection.set(
      document.uri,
      diagnostics.map((d) => {
        const startPos = document.positionAt(d.start);
        const endPos = document.positionAt(d.start + Math.max(d.len, 1));
        return new vscode.Diagnostic(
          new vscode.Range(startPos, endPos),
          d.message,
          vscode.DiagnosticSeverity.Error
        );
      })
    );
  }

  context.subscriptions.push(
    diagnosticCollection,
    vscode.workspace.onDidChangeTextDocument((e) => updateDiagnostics(e.document)),
    vscode.workspace.onDidOpenTextDocument(updateDiagnostics),
    vscode.workspace.onDidCloseTextDocument((doc) => diagnosticCollection.delete(doc.uri))
  );

  vscode.workspace.textDocuments.forEach(updateDiagnostics);
}

export function deactivate(): void { }
