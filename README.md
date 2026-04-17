# Atla

Atla は F# で実装されたコンパイラ/ビルドツールチェーンです。  
このリポジトリでは主に以下の構成で動作します。

- `Atla.Console` : CLI フロントエンド
- `Atla.Build` : `atla.yaml` 読み取り + 依存解決
- `Atla.Core` : コンパイル本体（AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL）

## CLI インターフェイス

### build コマンド

```bash
dotnet run --project src/Atla.Console -- build <projectRoot> [-o <outDir>] [--name <assemblyName>]
# publish 後:
atla.exe build <projectRoot> [-o <outDir>] [--name <assemblyName>]
```

- `projectRoot` には Atla プロジェクトルートを指定します。
- `atla.yaml` と `src/main.atla` が必要です。
- `-o` 省略時の出力先は `<projectRoot>/out` です。
- `--name` 省略時のアセンブリ名は `atla.yaml` の `package.name` です。
- 終了コードは成功 `0` / 失敗 `1` です。

### `atla.yaml` 例

最小構成:

```yaml
package:
  name: "hello"
  version: "0.1.0"
```

依存を含む構成:

```yaml
package:
  name: "app"
  version: "0.1.0"
dependencies:
  corelib:
    path: "../corelib"
  Newtonsoft.Json:
    version: "13.0.3"
```

## 環境変数

### `NUGET_PACKAGES`

NuGet パッケージキャッシュの参照先を上書きします。  
未設定時は `~/.nuget/packages`（ユーザープロファイル配下）を使用します。

### NuGet 自動取得

NuGet 依存がキャッシュに存在しない場合は、NuGet.Client API 経由で取得を自動試行します。
キャッシュ配置を明示したい場合は `NUGET_PACKAGES` を設定してください。


## 参考ドキュメント

- CLI 詳細: `doc/cli-interface.md`
- Build フェーズ設計: `doc/build-system-phase1.md`
