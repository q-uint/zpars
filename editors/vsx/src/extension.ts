import * as vscode from "vscode";
import * as path from "path";
import * as fs from "fs";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

function platformBinaryName(): string {
  return process.platform === "win32" ? "zpars-lsp.exe" : "zpars-lsp";
}

function locateServerBinary(context: vscode.ExtensionContext): string | undefined {
  const cfg = vscode.workspace.getConfiguration("zpars");
  const configured = cfg.get<string>("serverPath");
  if (configured && configured.length > 0) {
    if (fs.existsSync(configured)) return configured;
    vscode.window.showWarningMessage(
      `zpars: configured serverPath does not exist: ${configured}`
    );
  }

  const bundled = path.join(context.extensionPath, "server", platformBinaryName());
  if (fs.existsSync(bundled)) return bundled;

  // PATH fallback: spawn `which` / `where` and let the OS resolve.
  // Returning the bare name lets Node resolve via PATH on launch.
  return platformBinaryName();
}

async function startClient(context: vscode.ExtensionContext): Promise<void> {
  const command = locateServerBinary(context);
  if (!command) {
    vscode.window.showErrorMessage("zpars: could not locate zpars-lsp binary.");
    return;
  }

  const serverOptions: ServerOptions = {
    run: { command, transport: TransportKind.stdio },
    debug: { command, transport: TransportKind.stdio },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "abnf" },
      { scheme: "file", language: "bnf" },
      { scheme: "file", language: "peg" },
      { scheme: "file", language: "cfg" },
      { scheme: "file", language: "sexp" },
      { scheme: "file", language: "ere" },
    ],
    outputChannelName: "zpars",
  };

  client = new LanguageClient("zpars", "zpars", serverOptions, clientOptions);
  await client.start();
}

interface MatchResult {
  matched: boolean;
  value?: string;
  rest?: string;
}

interface TreeResult {
  json: string;
}

async function pickRuleName(): Promise<string | undefined> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) return undefined;

  const doc = editor.document;
  const wordRange = doc.getWordRangeAtPosition(editor.selection.active);
  const initial = wordRange ? doc.getText(wordRange) : "";

  return await vscode.window.showInputBox({
    prompt: "Rule name to match",
    value: initial,
    ignoreFocusOut: true,
  });
}

async function commandMatch(): Promise<void> {
  if (!client) return;
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage("zpars: no active grammar file");
    return;
  }

  const rule = await pickRuleName();
  if (!rule) return;

  const input = await vscode.window.showInputBox({
    prompt: `Input to match against ${rule}`,
    ignoreFocusOut: true,
  });
  if (input === undefined) return;

  try {
    const result = await client.sendRequest<MatchResult>("zpars/match", {
      uri: editor.document.uri.toString(),
      rule,
      input,
    });

    const channel = client.outputChannel;
    channel.show(true);
    channel.appendLine(`--- match: ${rule} ---`);
    channel.appendLine(`input:   ${JSON.stringify(input)}`);
    if (result.matched) {
      channel.appendLine(`matched: ${JSON.stringify(result.value ?? "")}`);
      channel.appendLine(`rest:    ${JSON.stringify(result.rest ?? "")}`);
    } else {
      channel.appendLine(`no match`);
    }
  } catch (err) {
    vscode.window.showErrorMessage(`zpars: match failed: ${err}`);
  }
}

async function commandTree(): Promise<void> {
  if (!client) return;
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    vscode.window.showErrorMessage("zpars: no active grammar file");
    return;
  }
  if (editor.document.languageId !== "peg") {
    vscode.window.showErrorMessage("zpars: tree is PEG-only");
    return;
  }

  const input = await vscode.window.showInputBox({
    prompt: "Input to parse into a capture tree",
    ignoreFocusOut: true,
  });
  if (input === undefined) return;

  try {
    const result = await client.sendRequest<TreeResult>("zpars/tree", {
      uri: editor.document.uri.toString(),
      input,
    });

    const json = result.json && result.json.length > 0 ? result.json : "{}";
    let pretty = json;
    try {
      pretty = JSON.stringify(JSON.parse(json), null, 2);
    } catch {
      // leave as-is
    }

    const doc = await vscode.workspace.openTextDocument({
      language: "json",
      content: pretty,
    });
    await vscode.window.showTextDocument(doc, { preview: true });
  } catch (err) {
    vscode.window.showErrorMessage(`zpars: tree failed: ${err}`);
  }
}

async function commandRestart(context: vscode.ExtensionContext): Promise<void> {
  if (client) {
    await client.stop();
    client = undefined;
  }
  await startClient(context);
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  context.subscriptions.push(
    vscode.commands.registerCommand("zpars.match", commandMatch),
    vscode.commands.registerCommand("zpars.tree", commandTree),
    vscode.commands.registerCommand("zpars.restartServer", () =>
      commandRestart(context)
    )
  );

  await startClient(context);
}

export async function deactivate(): Promise<void> {
  if (client) {
    await client.stop();
    client = undefined;
  }
}
