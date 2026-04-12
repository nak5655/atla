/// Entry point: read LSP messages from stdin in a loop and dispatch them.
module Atla.LanguageServer.Program

open Newtonsoft.Json.Linq
open Atla.LanguageServer.LSPTypes
open Atla.LanguageServer.LSPMessage
open Atla.LanguageServer.Server

[<EntryPoint>]
let main _ =
    let server = Server()
    let mutable isShutdown = false

    while true do
        match waitMessage () with

        // ---- Requests (client expects a response with matching id) ----------

        | RequestMsg(_, content) ->
            let id     = requestId content
            let method = messageMethod content

            match method with
            | "initialize" ->
                let result = server.Initialize content
                sendResponse id result

            | "textDocument/semanticTokens/full" ->
                let uri  = (messageParams content).["textDocument"].["uri"].ToString()
                let data = server.Tokenize uri
                sendResponse id (SemanticTokens("", data))

            | "shutdown" ->
                // Acknowledge the shutdown request and mark as ready to exit.
                isShutdown <- true
                sendResponse id (box null)

            | _ -> ()   // ignore unhandled request methods

        // ---- Notifications (no response expected) ---------------------------

        | NotificationMsg(_, content) ->
            let method = messageMethod content

            match method with
            | "initialized" ->
                windowLogMessage "initialized" MessageType.Info

                if not server.IsAvailablePublishDiagnostics then
                    windowLogMessage "PublishDiagnostics is not available." MessageType.Info

            | "textDocument/didOpen" ->
                let p    = messageParams content
                let uri  = p.["textDocument"].["uri"].ToString()
                let text = p.["textDocument"].["text"].ToString()
                server.Compile(uri, text)
                windowLogMessage (sprintf "textDocument/didOpen %s" uri) MessageType.Info

            | "textDocument/didChange" ->
                let p       = messageParams content
                let uri     = p.["textDocument"].["uri"].ToString()
                let changes = p.["contentChanges"] :?> Newtonsoft.Json.Linq.JArray |> Seq.toArray
                if changes.Length > 0 then
                    let text = changes.[changes.Length - 1].["text"].ToString()
                    server.Compile(uri, text)
                windowLogMessage (sprintf "textDocument/didChange %s" uri) MessageType.Info

            | "textDocument/didClose" ->
                let uri = (messageParams content).["textDocument"].["uri"].ToString()
                windowLogMessage (sprintf "textDocument/didClose %s" uri) MessageType.Info

            | "exit" ->
                // The LSP spec requires the process to exit with code 0 after
                // a preceding shutdown request, or code 1 otherwise.
                System.Environment.Exit(if isShutdown then 0 else 1)

            | _ -> ()   // ignore unhandled notification methods

    0
