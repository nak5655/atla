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
}

export async function deactivate(): Promise<void> {
    if (client) {
        await client.stop();
    }
}
