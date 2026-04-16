namespace Atla.LanguageServer.Tests

open Newtonsoft.Json
open Xunit
open Atla.LanguageServer.Server
open Atla.LanguageServer.LSPTypes

module DiagnosticsTests =
    [<Fact>]
    let ``semantic unresolved identifier diagnostics snapshot`` () =
        let published = ResizeArray<Diagnostic list>()
        let server = Server(fun _ diagnostics -> published.Add(diagnostics))
        server.IsAvailablePublishDiagnostics <- true

        server.OpenDocument("file:///tmp/semantic-unresolved.atla", "fn main: Int = missing")

        let diagnostics = published |> Seq.last
        let json = JsonConvert.SerializeObject(diagnostics, Formatting.None)

        let _ = Assert.Single(diagnostics)
        Assert.Contains("\"source\":\"atla-compiler\"", json)
        Assert.Contains("\"severity\":1", json)

    [<Fact>]
    let ``semantic type mismatch diagnostics snapshot`` () =
        let published = ResizeArray<Diagnostic list>()
        let server = Server(fun _ diagnostics -> published.Add(diagnostics))
        server.IsAvailablePublishDiagnostics <- true

        server.OpenDocument("file:///tmp/semantic-type-mismatch.atla", "fn main: Int = \"hello\"")

        let diagnostics = published |> Seq.last
        let json = JsonConvert.SerializeObject(diagnostics, Formatting.None)

        let _ = Assert.Single(diagnostics)
        Assert.Contains("\"source\":\"atla-compiler\"", json)
        Assert.Contains("\"severity\":1", json)

    [<Fact>]
    let ``syntax error diagnostics snapshot`` () =
        let published = ResizeArray<Diagnostic list>()
        let server = Server(fun _ diagnostics -> published.Add(diagnostics))
        server.IsAvailablePublishDiagnostics <- true

        server.OpenDocument("file:///tmp/syntax-error.atla", "fn main: Int =")

        let diagnostics = published |> Seq.last
        let json = JsonConvert.SerializeObject(diagnostics, Formatting.None)

        let _ = Assert.Single(diagnostics)
        Assert.Contains("\"source\":\"atla-compiler\"", json)
        Assert.Contains("\"severity\":1", json)

        let first = diagnostics.Head
        Assert.True(first.range.start.line >= 0)
        Assert.True(first.range.start.character >= 0)
