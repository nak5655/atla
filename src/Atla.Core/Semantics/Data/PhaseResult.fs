namespace Atla.Core.Semantics.Data

type PhaseResult<'T> =
    { succeeded: bool
      value: 'T option
      diagnostics: Diagnostic list }

module PhaseResult =
    let succeeded (value: 'T) (diagnostics: Diagnostic list) : PhaseResult<'T> =
        { succeeded = true
          value = Some value
          diagnostics = diagnostics }

    let failed (diagnostics: Diagnostic list) : PhaseResult<'T> =
        { succeeded = false
          value = None
          diagnostics = diagnostics }
