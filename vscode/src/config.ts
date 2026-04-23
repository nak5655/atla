'use strict';

import * as vscode from 'vscode';

// TODO 環境変数の置換の仕方が分からないのでとりあえず決め打ちで
function replacePathVar(path: string): string {
    return path.replace('~', process.env['USERPROFILE']!);
}

export class Config {
    readonly rootSection = 'atla';

    constructor(_: vscode.ExtensionContext) {}

    private get<T>(path: string): T {
        return this.cfg.get<T>(path)!;
    }

    private get cfg(): vscode.WorkspaceConfiguration {
        return vscode.workspace.getConfiguration(this.rootSection);
    }

    get serverPath() {
        return replacePathVar(this.get<string>('lsp-server.path'));
    }

    get buildSystemPath() {
        return replacePathVar(this.get<string>('build-system.path'));
    }
}
