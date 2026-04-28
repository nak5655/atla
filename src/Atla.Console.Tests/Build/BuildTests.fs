namespace Atla.Console.Build.Tests

open System
open System.IO
open Xunit
open Atla.Console

module BuildTests =
    /// テスト用の一時プロジェクトディレクトリを作成する。
    let private createTempProjectDir () =
        let projectRoot = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(Path.Join(projectRoot, "src")) |> ignore
        projectRoot

    /// 最小の atla.yaml を書き込む。
    let private writeManifest (projectRoot: string) (name: string) =
        File.WriteAllText(
            Path.Join(projectRoot, "atla.yaml"),
            $"""
package:
  name: "{name}"
  version: "0.1.0"
""".Trim())

    /// カレントディレクトリから上位へ辿ってリポジトリルートを見つける。
    let private tryFindRepositoryRoot () : string option =
        let rec loop (dir: DirectoryInfo) =
            let marker = Path.Join(dir.FullName, "examples", "gui_hello", "atla.yaml")
            if File.Exists(marker) then
                Some dir.FullName
            else
                match dir.Parent with
                | null -> None
                | parent -> loop parent

        loop (DirectoryInfo(Directory.GetCurrentDirectory()))

    [<Fact>]
    let ``help should return zero`` () =
        let code = Console.run [| "--help" |]
        Assert.Equal(0, code)

    [<Fact>]
    let ``no args should return one`` () =
        let code = Console.run [||]
        Assert.Equal(1, code)

    [<Fact>]
    let ``build should fail when project root does not exist`` () =
        let projectRoot = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))

        let code = Console.run [| "build"; projectRoot |]
        Assert.Equal(1, code)

    [<Fact>]
    let ``build should fail when src main is missing`` () =
        let projectRoot = createTempProjectDir ()
        writeManifest projectRoot "hello"

        let code = Console.run [| "build"; projectRoot |]
        Assert.Equal(1, code)

    [<Fact>]
    let ``build should emit dll for valid project root`` () =
        let projectRoot = createTempProjectDir ()
        writeManifest projectRoot "hello"

        File.WriteAllText(
            Path.Join(projectRoot, "src", "main.atla"),
            """
import System'Console

fn main: () = do
    "Hello, World!" Console'WriteLine.
""".Trim())

        let outDir = Path.Join(projectRoot, "artifacts")
        let code = Console.run [| "build"; projectRoot; "-o"; outDir; "--name"; "HelloConsole" |]

        Assert.Equal(0, code)
        Assert.True(File.Exists(Path.Join(outDir, "HelloConsole.dll")))

    [<Fact>]
    let ``build should succeed for examples gui_hello`` () =
        match tryFindRepositoryRoot () with
        | Some repositoryRoot ->
            let projectRoot = Path.Join(repositoryRoot, "examples", "gui_hello")
            let outDir = Path.Join(projectRoot, "out-regression")

            if Directory.Exists(outDir) then
                Directory.Delete(outDir, recursive = true)

            let code = Console.run [| "build"; projectRoot; "-o"; outDir; "--name"; "GuiExampleRegression" |]
            Assert.Equal(0, code)
            Assert.True(File.Exists(Path.Join(outDir, "GuiExampleRegression.dll")))
        | None ->
            Assert.True(false, "Repository root was not found from current working directory.")

    [<Fact>]
    let ``build should succeed for examples data`` () =
        match tryFindRepositoryRoot () with
        | Some repositoryRoot ->
            let projectRoot = Path.Join(repositoryRoot, "examples", "data")
            let outDir = Path.Join(projectRoot, "out-regression")

            if Directory.Exists(outDir) then
                Directory.Delete(outDir, recursive = true)

            let code = Console.run [| "build"; projectRoot; "-o"; outDir; "--name"; "DataExampleRegression" |]
            Assert.Equal(0, code)
            Assert.True(File.Exists(Path.Join(outDir, "DataExampleRegression.dll")))
        | None ->
            Assert.True(false, "Repository root was not found from current working directory.")

    [<Fact>]
    let ``build should succeed for examples hello_module`` () =
        match tryFindRepositoryRoot () with
        | Some repositoryRoot ->
            let projectRoot = Path.Join(repositoryRoot, "examples", "hello_module")
            let outDir = Path.Join(projectRoot, "out-regression")

            if Directory.Exists(outDir) then
                Directory.Delete(outDir, recursive = true)

            let code = Console.run [| "build"; projectRoot; "-o"; outDir; "--name"; "HelloModuleRegression" |]
            Assert.Equal(0, code)
            Assert.True(File.Exists(Path.Join(outDir, "HelloModuleRegression.dll")))
        | None ->
            Assert.True(false, "Repository root was not found from current working directory.")
