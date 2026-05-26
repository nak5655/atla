/// File-based diagnostic logging for the language server, including crash capture.
/// Logging must never throw: all IO is guarded so a logging failure can never
/// take down the server it is meant to diagnose.
module Atla.LanguageServer.Log

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open Atla.Build

/// アクティブログの最大サイズ（100KB）。超過したら 1 世代ローテーションする。
[<Literal>]
let private maxBytes = 102400

let private writeLock = obj ()

/// 解決済みログファイルパス（既定 ~/.atla/logs/atla-lsp.log、ATLA_HOME を尊重）。
let logPath : string =
    Path.Join(InstallSystem.resolveAtlaHome (), "logs", "atla-lsp.log")

let private rotatedPath = logPath + ".1"

let private pid =
    try Process.GetCurrentProcess().Id with _ -> 0

/// アクティブファイルが maxBytes 以上なら `.1` へ退避し、新規ファイルから書き直す。
let private rotateIfNeeded () =
    try
        let info = FileInfo(logPath)
        if info.Exists && info.Length >= int64 maxBytes then
            if File.Exists rotatedPath then File.Delete rotatedPath
            File.Move(logPath, rotatedPath)
    with _ -> ()

let private writeRaw (level: string) (message: string) =
    lock writeLock (fun () ->
        try
            let dir = Path.GetDirectoryName(logPath)
            if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
                Directory.CreateDirectory dir |> ignore
            rotateIfNeeded ()
            let ts = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff")
            let tid = Thread.CurrentThread.ManagedThreadId
            File.AppendAllText(logPath, sprintf "[%s][%d][%d][%s] %s%s" ts pid tid level message Environment.NewLine)
        with _ -> ())

let info (message: string) = writeRaw "INFO" message
let warn (message: string) = writeRaw "WARN" message
let error (message: string) = writeRaw "ERROR" message

/// 例外をフルスタックトレース付きで記録する。
let logException (context: string) (ex: exn) =
    let detail = if isNull (box ex) then "<null exception>" else ex.ToString()
    writeRaw "ERROR" (sprintf "%s: %s" context detail)

let mutable private initialized = false

/// グローバル例外ハンドラを登録し、起動バナーを出力する。プロセスにつき一度だけ。
let init () =
    lock writeLock (fun () ->
        if not initialized then
            initialized <- true
            AppDomain.CurrentDomain.UnhandledException.Add(fun args ->
                match args.ExceptionObject with
                | :? exn as ex ->
                    logException (sprintf "UnhandledException (terminating=%b)" args.IsTerminating) ex
                | other ->
                    error (sprintf "UnhandledException (terminating=%b): %A" args.IsTerminating other))
            TaskScheduler.UnobservedTaskException.Add(fun args ->
                logException "UnobservedTaskException" args.Exception
                args.SetObserved())
            AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> info "==== atla-lsp process exit ===="))

    let version =
        try string (Reflection.Assembly.GetExecutingAssembly().GetName().Version)
        with _ -> "unknown"
    info (sprintf "==== atla-lsp started (pid=%d, version=%s, log=%s) ====" pid version logPath)
