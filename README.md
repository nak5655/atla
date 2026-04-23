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

## 依存DLLの2系統解決（compile / runtime）

- Atla は依存DLLを次の2用途で別々に扱います。
  - `compileReferencePaths`: 意味解析・型解決の参照向け（優先順: `ref > lib`）
  - `runtimeLoadPaths`: 実際の `AssemblyLoadContext` ロード向け（優先順: `lib > ref`、`lib` 不在時は `ref` フォールバック）
- この分離により、参照専用DLL（`ref`）を実ロードして失敗するケースを避けつつ、コンパイル時の参照解決精度を維持します。

## 言語メモ（型注釈）

- 空白区切りの型適用をサポートします（例: `Array String`, `String Int`）。
- `Array T` はランタイム配列型へ解決されます（例: `Array String` -> `System.String[]`）。
- プリミティブ型名は `String` / `Int` / `Bool` / `Float` / `Unit` のように **大文字始まり**を使用してください。
- 型引数付き呼び出しは `target[typeArgs]` 記法を使用します（例: `AppBuilder.Configure[Application] ()`）。
- インデックスアクセスは `expr !! index` 記法を使用します（例: `values !! i`）。
- `a[b]` のインデックス記法は廃止されました。既存コードは `a !! b` へ移行してください。

## クロージャー lowering 方針（2026-04-21 時点）

- 非捕捉ラムダは lambda lifting で module-level method へ変換し、static delegate として扱います。
- 捕捉ラムダは将来的に `env-instance + lifted invoke` へ変換する方針です。
- 捕捉順序（env フィールド順、初期化順、補助メソッド引数順）は `SymbolId` 昇順で固定します。
- mutable 捕捉は C# 互換寄りに「変数単位（共有セル）」を目標とします。
- 現在は env-class 本実装前の段階であり、捕捉ラムダは明示診断で失敗させます（非捕捉のみ成功パス）。


## ソリューション一括 Publish（Windows）

`src/Atla.slnx` に含まれる publish 対象プロジェクトを一括で publish するには、
リポジトリルートで次を実行してください。

```bat
publish-atla.bat
```

このバッチは内部で `dotnet publish src\Atla.slnx -c Release -p:PublishProfile=FolderProfile` を実行します。

`FolderProfile` は各プロジェクトの `Properties/PublishProfiles/FolderProfile.pubxml` を参照します。
出力先を変更したい場合は、対象プロジェクトの `FolderProfile.pubxml` の `PublishDir` を編集してください。

## 参考ドキュメント

- CLI 詳細: `doc/cli-interface.md`
- Build フェーズ設計: `doc/build-system-phase1.md`
