namespace Atla.Core.Semantics.Data

open Atla.Core.Data

type DiagnosticSeverity =
    | Error
    | Warning
    | Info

type Diagnostic =
    | Error of message: string * span: Span
    | Warning of message: string * span: Span
    | Info of message: string * span: Span

    member this.message =
        match this with
        | Error (message, _)
        | Warning (message, _)
        | Info (message, _) -> message

    member this.span =
        match this with
        | Error (_, span)
        | Warning (_, span)
        | Info (_, span) -> span

    member this.severity =
        match this with
        | Error _ -> DiagnosticSeverity.Error
        | Warning _ -> DiagnosticSeverity.Warning
        | Info _ -> DiagnosticSeverity.Info

    member this.isError =
        match this with
        | Error _ -> true
        | Warning _
        | Info _ -> false

    member this.WithSource (source: string) =
        match this with
        | Error (message, span) -> Error($"{source}: {message}", span)
        | Warning (message, span) -> Warning($"{source}: {message}", span)
        | Info (message, span) -> Info($"{source}: {message}", span)

    member this.toDisplayText() = $"{this.span.left}: {this.message}"
