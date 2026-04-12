/// Entry point: read LSP messages from stdin in a loop and dispatch them.
module Atla.LanguageServer.Program

open Newtonsoft.Json.Linq
open Atla.LanguageServer.LSPTypes
open Atla.LanguageServer.LSPMessage
open Atla.LanguageServer.Server

let private methodNotFound = -32601
let private invalidRequest = -32600

/// Pure shutdown state transition for request methods.
let nextShutdownState (isShutdown: bool) (methodName: string) : bool =
    if methodName = "shutdown" then true else isShutdown

/// Compute process exit code per LSP requirement.
let exitCodeFromShutdownState (isShutdown: bool) : int =
    if isShutdown then 0 else 1

[<EntryPoint>]
let main _ =
    let server = Server()
    let mutable isShutdown = false

    while true do
        match waitMessage () with

        // EOF or invalid-framed message: terminate per shutdown state.
        | None ->
            System.Environment.Exit(exitCodeFromShutdownState isShutdown)

        // ---- Requests (client expects a response with matching id) ----------

        | Some(RequestMsg(_, content)) ->
            match tryRequestId content, messageMethod content with
            | Some id, Some methodName ->
                match methodName with
                | "initialize" ->
                    let result = server.Initialize content
                    sendResponse id result

                | "textDocument/semanticTokens/full" ->
                    match messageParams content with
                    | Some p when p.["textDocument"] <> null && p.["textDocument"].["uri"] <> null ->
                        let uri = p.["textDocument"].["uri"].ToString()
                        let data = server.Tokenize uri
                        sendResponse id (SemanticTokens("", data))
                    | _ ->
                        sendErrorResponse id invalidRequest "Missing textDocument.uri"

                | "shutdown" ->
                    // Acknowledge the shutdown request and mark as ready to exit.
                    isShutdown <- nextShutdownState isShutdown methodName
                    sendResponse id (box null)

                | _ ->
                    sendErrorResponse id methodNotFound (sprintf "Method not found: %s" methodName)

            | Some id, None ->
                sendErrorResponse id invalidRequest "Missing method"
            | None, _ ->
                ()

        // ---- Notifications (no response expected) ---------------------------

        | Some(NotificationMsg(_, content)) ->
            match messageMethod content with
            | Some "initialized" ->
                windowLogMessage "initialized" MessageType.Info

                if not server.IsAvailablePublishDiagnostics then
                    windowLogMessage "PublishDiagnostics is not available." MessageType.Info

            | Some "textDocument/didOpen" ->
                match messageParams content with
                | Some p when p.["textDocument"] <> null && p.["textDocument"].["uri"] <> null && p.["textDocument"].["text"] <> null ->
                    let uri = p.["textDocument"].["uri"].ToString()
                    let text = p.["textDocument"].["text"].ToString()
                    server.OpenDocument(uri, text)
                    windowLogMessage (sprintf "textDocument/didOpen %s" uri) MessageType.Info
                | _ ->
                    windowLogMessage "textDocument/didOpen has invalid params" MessageType.Warning

            | Some "textDocument/didChange" ->
                match messageParams content with
                | Some p when p.["textDocument"] <> null && p.["textDocument"].["uri"] <> null && p.["contentChanges"] <> null ->
                    let uri = p.["textDocument"].["uri"].ToString()
                    let changes = p.["contentChanges"] :?> JArray |> Seq.toArray
                    if changes.Length > 0 && changes.[changes.Length - 1].["text"] <> null then
                        let text = changes.[changes.Length - 1].["text"].ToString()
                        server.ChangeDocument(uri, text)
                    windowLogMessage (sprintf "textDocument/didChange %s" uri) MessageType.Info
                | _ ->
                    windowLogMessage "textDocument/didChange has invalid params" MessageType.Warning

            | Some "textDocument/didClose" ->
                match messageParams content with
                | Some p when p.["textDocument"] <> null && p.["textDocument"].["uri"] <> null ->
                    let uri = p.["textDocument"].["uri"].ToString()
                    server.CloseDocument(uri)
                    windowLogMessage (sprintf "textDocument/didClose %s" uri) MessageType.Info
                | _ ->
                    windowLogMessage "textDocument/didClose has invalid params" MessageType.Warning

            | Some "exit" ->
                // The LSP spec requires the process to exit with code 0 after
                // a preceding shutdown request, or code 1 otherwise.
                System.Environment.Exit(exitCodeFromShutdownState isShutdown)

            | Some _
            | None ->
                ()   // ignore unhandled notification methods

    0
