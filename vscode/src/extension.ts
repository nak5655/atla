'use strict';

import * as vscode from 'vscode';
import * as lc from 'vscode-languageclient/node';
import { Config } from './config';
import { activateTaskProvider } from './tasks';

let client: lc.LanguageClient;

/**
 * 入力変更イベントから補完 UI を自動起動するべきかを判定する。
 * `.` 入力は補完トリガー対象から除外する。
 */
function shouldTriggerSuggestOnInput(event: vscode.TextDocumentChangeEvent): boolean {
    if (event.document.languageId !== 'atla') {
        return false;
    }
    if (event.contentChanges.length !== 1) {
        return false;
    }

    const change = event.contentChanges[0];
    if (change.text.length !== 1) {
        return false;
    }

    const ch = change.text;
    if (ch === '.') {
        return false;
    }

    return /^[A-Za-z0-9_']$/.test(ch);
}

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
    // LSPとの通信に使うリクエストを定義
    const clientOptions: lc.LanguageClientOptions = {
        // 対象とするファイルの種類や拡張子
        documentSelector: [{ scheme: 'file', language: 'atla' }],
        // 警告パネルでの表示名
        diagnosticCollectionName: 'Atla',
        revealOutputChannelOn: lc.RevealOutputChannelOn.Never,
        initializationOptions: {},
        progressOnInitialization: true,
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

    // 入力中に補完候補を表示する。
    // `.` 入力は対象外（ユーザー要件）として、識別子/`'` 入力時のみ起動する。
    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument((event) => {
            if (!shouldTriggerSuggestOnInput(event)) {
                return;
            }

            const activeEditor = vscode.window.activeTextEditor;
            if (activeEditor === undefined || activeEditor.document.uri.toString() !== event.document.uri.toString()) {
                return;
            }

            void vscode.commands.executeCommand('editor.action.triggerSuggest');
        }),
    );
}

export async function deactivate(): Promise<void> {
    if (client) {
        await client.stop();
    }
}
