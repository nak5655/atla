namespace Atla.Build

open System
open System.IO
open Atla.Compiler
open Atla.Core.Data
open Atla.Core.Semantics.Data

module InstallSystem =
    /// install 処理用の Diagnostic.Error を作成する。
    let private error (message: string) : Diagnostic =
        Diagnostic.Error(message, Span.Empty)

    /// `ATLA_HOME` 環境変数、または `~/.atla` をインストール先ルートとして解決する。
    let resolveAtlaHome () : string =
        match Environment.GetEnvironmentVariable("ATLA_HOME") with
        | null
        | "" ->
            let userHome = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
            Path.Join(userHome, ".atla")
        | value ->
            Path.GetFullPath(value)

    /// ディレクトリを再帰コピーする（既存宛先は上書きする）。
    let private copyDirectoryRecursive (srcDir: string) (dstDir: string) : unit =
        Directory.CreateDirectory(dstDir) |> ignore

        Directory.GetDirectories(srcDir, "*", SearchOption.AllDirectories)
        |> Array.iter (fun srcSubDir ->
            let relative = Path.GetRelativePath(srcDir, srcSubDir)
            let dstSubDir = Path.Join(dstDir, relative)
            Directory.CreateDirectory(dstSubDir) |> ignore)

        Directory.GetFiles(srcDir, "*", SearchOption.AllDirectories)
        |> Array.iter (fun srcFile ->
            let relative = Path.GetRelativePath(srcDir, srcFile)
            let dstFile = Path.Join(dstDir, relative)
            let parent = Path.GetDirectoryName(dstFile)
            if not (String.IsNullOrWhiteSpace(parent)) then
                Directory.CreateDirectory(parent) |> ignore
            File.Copy(srcFile, dstFile, overwrite = true))

    /// Unix ランチャースクリプトへ実行権限を付与する。
    let private trySetUnixExecutable (path: string) : unit =
        if not (OperatingSystem.IsWindows()) then
            try
                let mode =
                    UnixFileMode.UserRead
                    ||| UnixFileMode.UserWrite
                    ||| UnixFileMode.UserExecute
                    ||| UnixFileMode.GroupRead
                    ||| UnixFileMode.GroupExecute
                    ||| UnixFileMode.OtherRead
                    ||| UnixFileMode.OtherExecute

                File.SetUnixFileMode(path, mode)
            with
            | :? IOException
            | :? UnauthorizedAccessException
            | :? PlatformNotSupportedException ->
                ()

    /// `.atlalib` を `~/.atla/packages/<name>/<version>/` 配下へインストールする。
    let installLib
        (atlaHome: string)
        (projectName: string)
        (projectVersion: string)
        (atlaLibPath: string)
        : Result<string, Diagnostic list> =
        try
            if not (File.Exists(atlaLibPath)) then
                Result.Error [ error $"atlalib not found: `{atlaLibPath}`" ]
            else
                let installDir = Path.Join(atlaHome, "packages", projectName, projectVersion)
                let installedPath = Path.Join(installDir, $"{projectName}.atlalib")
                Directory.CreateDirectory(installDir) |> ignore
                File.Copy(atlaLibPath, installedPath, overwrite = true)
                Ok installedPath
        with ex ->
            Result.Error [ error $"failed to install atlalib: {ex.Message}" ]

    /// exe 成果物を `~/.atla/bin/<name>/` へ配置し、Unix/Windows ランチャーを作成する。
    let installExe
        (atlaHome: string)
        (projectName: string)
        (outDir: string)
        : Result<string list, Diagnostic list> =
        try
            if not (Directory.Exists(outDir)) then
                Result.Error [ error $"build output directory not found: `{outDir}`" ]
            else
                let binRoot = Path.Join(atlaHome, "bin")
                let payloadDir = Path.Join(binRoot, projectName)
                let unixLauncherPath = Path.Join(binRoot, $"{projectName}.sh")
                let windowsLauncherPath = Path.Join(binRoot, $"{projectName}.bat")
                let installedDllPath = Path.Join(payloadDir, $"{projectName}.dll")

                Directory.CreateDirectory(binRoot) |> ignore

                if Directory.Exists(payloadDir) then
                    Directory.Delete(payloadDir, recursive = true)

                copyDirectoryRecursive outDir payloadDir

                if not (File.Exists(installedDllPath)) then
                    Result.Error [ error $"installed exe assembly not found: `{installedDllPath}`" ]
                else
                    let unixLauncherContent =
                        String.concat Environment.NewLine [
                            "#!/bin/sh"
                            "SCRIPT_DIR=$(CDPATH= cd -- \"$(dirname -- \"$0\")\" && pwd)"
                            $"DLL_PATH=\"$SCRIPT_DIR/{projectName}/{projectName}.dll\""
                            "if [ ! -f \"$DLL_PATH\" ]; then"
                            "  echo 'atla launcher error: executable payload not found:' \"$DLL_PATH\" >&2"
                            "  exit 1"
                            "fi"
                            "exec dotnet \"$DLL_PATH\" \"$@\""
                            ""
                        ]

                    File.WriteAllText(unixLauncherPath, unixLauncherContent)
                    trySetUnixExecutable unixLauncherPath

                    let windowsLauncherContent =
                        String.concat Environment.NewLine [
                            "@echo off"
                            "set SCRIPT_DIR=%~dp0"
                            $"set DLL_PATH=%%SCRIPT_DIR%%{projectName}\\{projectName}.dll"
                            "if not exist \"%DLL_PATH%\" ("
                            "  >&2 echo atla launcher error: executable payload not found: \"%DLL_PATH%\""
                            "  exit /b 1"
                            ")"
                            "dotnet \"%DLL_PATH%\" %*"
                            ""
                        ]

                    File.WriteAllText(windowsLauncherPath, windowsLauncherContent)
                    Ok [ payloadDir; unixLauncherPath; windowsLauncherPath ]
        with ex ->
            Result.Error [ error $"failed to install executable package: {ex.Message}" ]

    /// dll パッケージを一時的に `.atlalib` 化して `installLib` へ委譲する。
    let installDll
        (atlaHome: string)
        (buildPlan: BuildPlan)
        (asmName: string)
        (compileOutDir: string)
        (packageOutDir: string)
        (compileResult: Compiler.CompileResult)
        : Result<string, Diagnostic list> =
        match BuildSystem.createAtlaLib
            buildPlan.projectName
            buildPlan.projectVersion
            asmName
            compileOutDir
            packageOutDir
            buildPlan.dependencies
            compileResult with
        | Result.Error diagnostics ->
            Result.Error diagnostics
        | Ok atlaLibPath ->
            installLib atlaHome buildPlan.projectName buildPlan.projectVersion atlaLibPath
