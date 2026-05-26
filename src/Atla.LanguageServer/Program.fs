/// Entry point: read LSP messages from stdin in a loop and dispatch them.
module Atla.LanguageServer.Program

open Newtonsoft.Json.Linq
open Atla.LanguageServer.LSPTypes
open Atla.LanguageServer.LSPMessage
open Atla.LanguageServer.Server

let private methodNotFound = -32601
let private invalidRequest = -32600
let private internalError = -32603

/// Pure shutdown state transition for request methods.
let nextShutdownState (isShutdown: bool) (methodName: string) : bool =
    if methodName = "shutdown" then true else isShutdown

/// Compute process exit code per LSP requirement.
let exitCodeFromShutdownState (isShutdown: bool) : int =
    if isShutdown then 0 else 1

/// 受信メッセージの簡潔な記述（method + uri + position）。クラッシュ直前の
/// 「どの操作だったか」を残すための trace 用。
let private describeMessage (content: JObject) : string =
    let methodName = messageMethod content |> Option.defaultValue "<no-method>"
    let detail =
        match messageParams content with
        | Some p ->
            let uri =
                let td = p.["textDocument"]
                if not (isNull td) && not (isNull td.["uri"]) then " uri=" + td.["uri"].ToString() else ""
            let pos =
                let ps = p.["position"]
                if not (isNull ps) then sprintf " pos=%s:%s" (string ps.["line"]) (string ps.["character"]) else ""
            uri + pos
        | None -> ""
    methodName + detail

[<EntryPoint>]
let main _ =
    Log.init ()
    let server = Server()
    let mutable isShutdown = false

    /// 単一メッセージを処理する。例外はメッセージループ側で捕捉・記録する。
    let handleMessage (message: LSPMessage) =
        match message with

        // ---- Requests (client expects a response with matching id) ----------

        | RequestMsg(_, content) ->
            Log.info ("recv request " + describeMessage content)
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
                        let data =
                            if server.TokenTypes.Length = 0 then
                                match server.SemanticTokensFallbackReason with
                                | Some reason -> windowLogMessage reason MessageType.Warning
                                | None -> ()

                                []
                            else
                                server.Tokenize uri
                        sendResponse id (SemanticTokens("", data))
                    | _ ->
                        sendErrorResponse id invalidRequest "Missing textDocument.uri"

                | "textDocument/completion" ->
                    match messageParams content with
                    | Some p when p.["textDocument"] <> null && p.["textDocument"].["uri"] <> null ->
                        let uri = p.["textDocument"].["uri"].ToString()
                        // position が省略または不正な場合は先頭位置（0, 0）にフォールバックする。
                        let line      = try p.["position"].["line"].Value<int>()      with _ -> 0
                        let character = try p.["position"].["character"].Value<int>() with _ -> 0
                        let completions = server.GetCompletions(uri, line, character)
                        sendResponse id completions
                    | _ ->
                        sendErrorResponse id invalidRequest "Missing textDocument.uri"

                | "textDocument/hover" ->
                    match messageParams content with
                    | Some p when p.["textDocument"] <> null && p.["textDocument"].["uri"] <> null ->
                        let uri = p.["textDocument"].["uri"].ToString()
                        // position が省略または不正な場合は先頭位置（0, 0）にフォールバックする。
                        let line      = try p.["position"].["line"].Value<int>()      with _ -> 0
                        let character = try p.["position"].["character"].Value<int>() with _ -> 0
                        let hover = server.GetHover(uri, line, character)
                        sendResponse id (hover |> Option.map box |> Option.defaultValue (box null))
                    | _ ->
                        sendErrorResponse id invalidRequest "Missing textDocument.uri"

                | "textDocument/definition" ->
                    match messageParams content with
                    | Some p when p.["textDocument"] <> null && p.["textDocument"].["uri"] <> null ->
                        let uri = p.["textDocument"].["uri"].ToString()
                        // position が省略または不正な場合は先頭位置（0, 0）にフォールバックする。
                        let line      = try p.["position"].["line"].Value<int>()      with _ -> 0
                        let character = try p.["position"].["character"].Value<int>() with _ -> 0
                        let location = server.GetDefinition(uri, line, character)
                        sendResponse id (location |> Option.map box |> Option.defaultValue (box null))
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

        | NotificationMsg(_, content) ->
            Log.info ("recv notification " + describeMessage content)
            match messageMethod content with
            | Some "initialized" ->
                windowLogMessage "initialized" MessageType.Info
                windowLogMessage (sprintf "atla-lsp log file: %s" Log.logPath) MessageType.Info

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
                    let changes = p.["contentChanges"] :?> JArray |> Seq.cast<JToken>
                    server.ApplyIncrementalChanges(uri, changes)
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
                Log.info "recv exit notification"
                System.Environment.Exit(exitCodeFromShutdownState isShutdown)

            | Some _
            | None ->
                ()   // ignore unhandled notification methods

    while true do
        match waitMessage () with

        // EOF or invalid-framed message: terminate per shutdown state.
        | None ->
            Log.info "stdin closed or invalid frame; exiting"
            System.Environment.Exit(exitCodeFromShutdownState isShutdown)

        | Some message ->
            try
                handleMessage message
            with ex ->
                // キャッチ可能例外はここで記録し、サーバを落とさずループを継続する。
                Log.logException ("message loop: " + describeMessage (messageContent message)) ex
                match message with
                | RequestMsg(_, content) ->
                    match tryRequestId content with
                    | Some id -> sendErrorResponse id internalError (sprintf "Internal error: %s" ex.Message)
                    | None -> ()
                | NotificationMsg _ -> ()

    0
