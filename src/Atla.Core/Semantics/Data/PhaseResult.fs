namespace Atla.Core.Semantics.Data

type PhaseResult<'T> =
    { succeeded: bool
      value: 'T option
      diagnostics: Diagnostic list }

module PhaseResult =
    /// フェーズ成功の結果を構築する。
    let succeeded (value: 'T) (diagnostics: Diagnostic list) : PhaseResult<'T> =
        { succeeded = true
          value = Some value
          diagnostics = diagnostics }

    /// 値を伴わない失敗結果を構築する。
    let failed (diagnostics: Diagnostic list) : PhaseResult<'T> =
        { succeeded = false
          value = None
          diagnostics = diagnostics }

    /// 部分結果（例: エラーを含む中間IR）を伴う失敗結果を構築する。
    let failedWithValue (value: 'T) (diagnostics: Diagnostic list) : PhaseResult<'T> =
        { succeeded = false
          value = Some value
          diagnostics = diagnostics }
