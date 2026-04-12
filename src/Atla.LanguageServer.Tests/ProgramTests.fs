namespace Atla.LanguageServer.Tests

open System
open System.Diagnostics
open System.Text
open Newtonsoft.Json.Linq
open Xunit
open Atla.LanguageServer.Program

module ProgramTests =
    [<Fact>]
    let ``shutdown and exit follow lsp exit code rule`` () =
        let state0 = false
        let state1 = nextShutdownState state0 "initialize"
        let state2 = nextShutdownState state1 "shutdown"

        Assert.False(state1)
        Assert.True(state2)
        Assert.Equal(0, exitCodeFromShutdownState state2)
        Assert.Equal(1, exitCodeFromShutdownState false)

    let private frame (json: string) =
        let bytes = Encoding.UTF8.GetBytes(json)
        sprintf "Content-Length: %d\r\n\r\n%s" bytes.Length json

    let private runServerWithRawInput (input: string) =
        let serverAssemblyPath = typeof<Atla.LanguageServer.Server.Server>.Assembly.Location
        let psi =
            ProcessStartInfo(
                FileName = "dotnet",
                Arguments = sprintf "\"%s\"" serverAssemblyPath,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            )

        use proc = Process.Start(psi)
        proc.StandardInput.Write(input)
        proc.StandardInput.Close()

        let stdout = proc.StandardOutput.ReadToEnd()
        let stderr = proc.StandardError.ReadToEnd()
        let exited = proc.WaitForExit(10000)
        if not exited then
            proc.Kill(true)
            failwith "language server process did not exit in time"

        proc.ExitCode, stdout, stderr

    let private parseFramedBodies (stdout: string) : string list =
        let rec loop (offset: int) (acc: string list) =
            if offset >= stdout.Length then
                List.rev acc
            else
                let marker = "Content-Length: "
                let markerIndex = stdout.IndexOf(marker, offset, StringComparison.Ordinal)
                if markerIndex < 0 then
                    List.rev acc
                else
                    let lineEndCrLf = stdout.IndexOf("\r\n", markerIndex, StringComparison.Ordinal)
                    let lineEndLf = stdout.IndexOf("\n", markerIndex, StringComparison.Ordinal)
                    let lineEnd =
                        if lineEndCrLf >= 0 then lineEndCrLf
                        elif lineEndLf >= 0 then lineEndLf
                        else -1

                    if lineEnd < 0 then List.rev acc
                    else
                        let lengthText = stdout.Substring(markerIndex + marker.Length, lineEnd - (markerIndex + marker.Length)).Trim()
                        let ok, length = Int32.TryParse(lengthText)
                        if not ok then List.rev acc
                        else
                            let bodyStartCrLf = stdout.IndexOf("\r\n\r\n", lineEnd, StringComparison.Ordinal)
                            let bodyStartLf = stdout.IndexOf("\n\n", lineEnd, StringComparison.Ordinal)
                            let bodyStart =
                                if bodyStartCrLf >= 0 then bodyStartCrLf + 4
                                elif bodyStartLf >= 0 then bodyStartLf + 2
                                else -1

                            if bodyStart < 0 || bodyStart + length > stdout.Length then List.rev acc
                            else
                                let body = stdout.Substring(bodyStart, length)
                                loop (bodyStart + length) (body :: acc)

        loop 0 []

    [<Fact>]
    let ``e2e normal flow uses content length framing`` () =
        let initialize =
            """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{"textDocument":{"semanticTokens":{"tokenTypes":["keyword","type","variable","number","string"]}}}}}"""

        let didOpen =
            """{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/e2e.atla","text":"fn main: Int = 0"}}}"""

        let semanticTokens =
            """{"jsonrpc":"2.0","id":2,"method":"textDocument/semanticTokens/full","params":{"textDocument":{"uri":"file:///tmp/e2e.atla"}}}"""

        let shutdown = """{"jsonrpc":"2.0","id":3,"method":"shutdown","params":{}}"""
        let exit = """{"jsonrpc":"2.0","method":"exit"}"""

        let input = String.Concat(frame initialize, frame didOpen, frame semanticTokens, frame shutdown, frame exit)
        let code, stdout, stderr = runServerWithRawInput input

        Assert.Equal(0, code)
        Assert.True(String.IsNullOrWhiteSpace(stderr), stderr)

        let bodies = parseFramedBodies stdout
        Assert.True(bodies.Length >= 3)

        let tokenResponse = bodies |> List.find (fun x -> x.Contains("\"id\":2")) |> JObject.Parse
        Assert.Equal("", tokenResponse.["result"].["resultId"].ToString())
        Assert.True((tokenResponse.["result"].["data"] :?> JArray).Count >= 5)

    [<Theory>]
    [<InlineData("Content-Len: 2\r\n\r\n{}")>]
    [<InlineData("X-Test: 1\r\n\r\n{}")>]
    [<InlineData("Content-Length: 4\r\n\r\n{bad")>]
    [<InlineData("Content-Length: 0\r\n\r\n")>]
    let ``e2e invalid framing inputs exit with code 1 and no output`` (raw: string) =
        let code, stdout, stderr = runServerWithRawInput raw
        Assert.Equal(1, code)
        Assert.True(String.IsNullOrWhiteSpace(stdout), stdout)
        Assert.True(String.IsNullOrWhiteSpace(stderr), stderr)

    [<Fact>]
    let ``e2e unknown request returns method not found error`` () =
        let unknown = """{"jsonrpc":"2.0","id":7,"method":"$/unknown","params":{}}"""
        let shutdown = """{"jsonrpc":"2.0","id":8,"method":"shutdown","params":{}}"""
        let exit = """{"jsonrpc":"2.0","method":"exit"}"""

        let input = String.Concat(frame unknown, frame shutdown, frame exit)
        let code, stdout, _ = runServerWithRawInput input

        Assert.Equal(0, code)
        let bodies = parseFramedBodies stdout
        let unknownResponse = bodies |> List.find (fun x -> x.Contains("\"id\":7")) |> JObject.Parse
        Assert.Equal(-32601, unknownResponse.["error"].["code"].Value<int>())
