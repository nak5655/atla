# `.atlalib` 仕様案（v1）

## 1. 目的

`atla.yaml` の `package.type` がライブラリ配布用途の場合に、Atlaのimport解決で利用できるコンパイル成果物を単一ファイルとして出力する。

本仕様は、コンパイラの段階境界（`AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL`）を保ちつつ、配布用成果物フォーマットを定義する。

---

## 2. `atla.yaml` の出力種別

`package.type` は以下を受理する。

- `exe`: 実行可能成果物を生成（既存仕様）。
- `lib`: **`.atlalib` のみ**生成する。
- `dll`: **`.dll` のみ**生成する。

### 2.1 `lib` の動作

- 中間的にCIL生成を行ってもよいが、ユーザー可視の最終成果物としては `.atlalib` だけを出力する。
- ビルド出力メッセージは `.atlalib` のみを `Generated:` として表示する。

### 2.2 `dll` の動作

- `Generated: <name>.dll` のみを出力する。
- `.atlalib` は生成しない。

---

## 3. `.atlalib` コンテナ形式

拡張子 `.atlalib` の実体はZIPコンテナとする。

理由:

- 実装コストが低い。
- ストリーム読取で先頭メタ情報を検査しやすい。
- 将来の拡張（署名、IR追加、差分配布）に対応しやすい。

### 3.1 コンテナレイアウト（v1）

```text
/
  atlalib.json
  assemblies/
    <package-name>.dll
    <package-name>.pdb                (任意)
  symbols/
    public.api.json
  deps/
    manifest.lock.json
  hashes/
    sha256sums.txt
  signature/
    package.sig                       (任意)
```

---

## 4. 必須ファイル定義

### 4.1 `atlalib.json`

フォーマット識別と互換性判定に使うメタデータ。

必須項目:

- `formatVersion`: `.atlalib` ファイル構造のバージョン。
- `package.name`, `package.version`
- `compiler.name`, `compiler.version`, `compiler.targetFramework`
- `artifacts.assembly`, `artifacts.publicApi`, `artifacts.dependencyLock`
- `compat.languageAbi`

例:

```json
{
  "formatVersion": "1.0",
  "package": {
    "name": "mylib",
    "version": "1.2.3"
  },
  "compiler": {
    "name": "atla",
    "version": "0.1.0",
    "targetFramework": "net10.0"
  },
  "artifacts": {
    "assembly": "assemblies/mylib.dll",
    "publicApi": "symbols/public.api.json",
    "dependencyLock": "deps/manifest.lock.json"
  },
  "compat": {
    "languageAbi": "atla-abi-1"
  }
}
```

### 4.2 `symbols/public.api.json`

import時のシンボル解決に必要な公開API情報（Semantic Analysis完了後の解決済み情報）を格納。

最低限の項目:

- 公開モジュール名
- 公開関数シグネチャ（名前・引数型・戻り型）
- 公開型定義（DU/record/enum等）
- 型パラメータ制約
- 属性（必要に応じて）

制約:

- 未解決識別子を含めない。
- AST由来の糖衣構文表現を残さない。

### 4.3 `deps/manifest.lock.json`

ビルド時に解決された依存関係を決定的に記録する。

最低限:

- 依存パッケージ識別子
- バージョン
- 取得元
- 内容ハッシュ

### 4.4 `hashes/sha256sums.txt`

コンテナ内エントリの整合性検証用。

- 形式: `<sha256>  <path>`
- 少なくとも必須ファイル群は全件含める。

---

## 5. 互換性ポリシー

- `formatVersion` のメジャー不一致: ロード拒否。
- `compat.languageAbi` 不一致: ロード拒否。
- `public.api.json` の破壊的変更: パッケージメジャーバージョン更新必須。
- `manifest.lock.json` と実際の参照不一致: エラー。

---

## 6. ビルド時フロー（`type: lib`）

1. 通常コンパイルでCILを生成。
2. Semantic Analysis結果を基に公開APIを抽出。
3. 依存lockを生成。
4. `.atlalib` をパッケージ。
5. ハッシュファイルを生成し整合性を検証。
6. `Generated: <name>.atlalib` を表示。

`type: dll` の場合は既存のDLL出力のみ実施する。

---

## 7. import側推奨動作

1. `.atlalib` を優先探索。
2. `atlalib.json` を読み取り、`formatVersion`/`languageAbi` を検証。
3. `symbols/public.api.json` を読み込んでシンボル解決。
4. 実行時/リンク時に `assemblies/*.dll` を利用。

---

## 8. 今後の拡張候補（非v1）

- `ir/hir.bin`, `ir/mir.bin` の任意格納（増分コンパイル最適化向け）。
- 署名検証の厳格化（証明書チェーン、失効確認）。
- ネイティブターゲット用複数アセンブリ同梱。

