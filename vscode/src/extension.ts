'use strict';

import * as vscode from 'vscode';
import * as lc from 'vscode-languageclient/node';
import { Config } from './config';
import { activateTaskProvider } from './tasks';

let client: lc.LanguageClient;

// 拡張機能が有効になったときに呼ばれる
export async function activate(context: vscode.ExtensionContext) {
    await tryActivate(context).catch((err) => {
        void vscode.window.showErrorMessage(`Cannot activate Atla: ${err.message}`);
        throw err;
    });
}

async function tryActivate(context: vscode.ExtensionContext) {
    const config = new Config(context);

    // コマンド登録
    const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
    if (workspaceFolder === undefined) {
        throw new Error('no folder is opened');
    }
    context.subscriptions.push(activateTaskProvider(workspaceFolder, config));

    // サーバーのパスを取得
    const serverPath = config.serverPath;

    // サーバーの設定
    const run: lc.Executable = {
        command: serverPath,
        options: { env: process.env },
    };
    const serverOptions: lc.ServerOptions = {
        run,
        debug: run,
    };
    // Atla ファイルで `'` 入力時に補完 UI を開くフックを登録する
    context.subscriptions.push(registerApostropheCompletionTrigger());

    // LSPとの通信に使うリクエストを定義
    const clientOptions: lc.LanguageClientOptions = {
        // 対象とするファイルの種類や拡張子
        documentSelector: [{ scheme: 'file', language: 'atla' }],
        // 警告パネルでの表示名
        diagnosticCollectionName: 'Atla',
        revealOutputChannelOn: lc.RevealOutputChannelOn.Never,
        initializationOptions: {},
        progressOnInitialization: true,
        // 旧仕様の `.` トリガー補完を抑止し、手動補完は維持する
        middleware: {
            provideCompletionItem: async (
                document,
                position,
                context,
                token,
                next,
            ) => {
                if (
                    context.triggerKind === vscode.CompletionTriggerKind.TriggerCharacter &&
                    context.triggerCharacter === '.'
                ) {
                    return [];
                }
                return next(document, position, context, token);
            },
        },
    };

    try {
        // LSPを起動
        client = new lc.LanguageClient('Atla LSP Server', serverOptions, clientOptions);
    } catch (err) {
        void vscode.window.showErrorMessage(
            'Failed to launch Atla LSP Server. See output for more details.',
        );
        return;
    }
    client.start().catch((error) => client.error(`Starting the server failed.`, error, 'force'));

    // 通知
    vscode.workspace.onDidChangeConfiguration(
        (_) => client.sendNotification('workspace/didChangeConfiguration', { settings: '' }),
        null,
        context.subscriptions,
    );
}

// Atla ドキュメントで `'` が入力されたら補完を開始する
function registerApostropheCompletionTrigger(): vscode.Disposable {
    return vscode.workspace.onDidChangeTextDocument(async (event) => {
        if (event.document.languageId !== 'atla' || event.contentChanges.length !== 1) {
            return;
        }

        const [change] = event.contentChanges;
        if (change.text !== '\'' || change.rangeLength > 0) {
            return;
        }

        const editor = vscode.window.activeTextEditor;
        if (editor === undefined || editor.document.uri.toString() !== event.document.uri.toString()) {
            return;
        }

        await vscode.commands.executeCommand('editor.action.triggerSuggest');
    });
}

export async function deactivate(): Promise<void> {
    if (client) {
        await client.stop();
    }
}
