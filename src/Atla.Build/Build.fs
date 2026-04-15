namespace Atla.Build

open System
open System.IO
open Atla.Core.Data
open Atla.Core.Semantics.Data
open Tomlyn
open Tomlyn.Model

type ResolvedDependency =
    { name: string
      version: string
      source: string }

type BuildRequest =
    { projectRoot: string }

type BuildPlan =
    { projectName: string
      projectVersion: string
      projectRoot: string
      dependencies: ResolvedDependency list }

type BuildResult =
    { succeeded: bool
      plan: BuildPlan option
      diagnostics: Diagnostic list }

module BuildSystem =
    let private manifestFileName = "atla.toml"

    let private error (message: string) : Diagnostic =
        Diagnostic.Error(message, Span.Empty)

    let private failed (diagnostics: Diagnostic list) : BuildResult =
        { succeeded = false
          plan = None
          diagnostics = diagnostics }

    let private succeeded (plan: BuildPlan) : BuildResult =
        { succeeded = true
          plan = Some plan
          diagnostics = [] }

    let private tryGetRequiredString (table: TomlTable) (fieldName: string) : Result<string, Diagnostic list> =
        match table.TryGetValue(fieldName) with
        | true, (:? string as value) when not (String.IsNullOrWhiteSpace value) ->
            Ok value
        | true, (:? string) ->
            Result.Error [ error $"`package.{fieldName}` must not be empty" ]
        | true, _ ->
            Result.Error [ error $"`package.{fieldName}` must be a string" ]
        | false, _ ->
            Result.Error [ error $"missing required field `package.{fieldName}`" ]

    let private parseManifestToPlan (projectRoot: string) (manifestText: string) : BuildResult =
        let document = Toml.Parse(manifestText)

        if document.HasErrors then
            let diagnostics =
                document.Diagnostics
                |> Seq.map (fun diag -> error $"atla.toml parse error: {diag.ToString()}")
                |> Seq.toList
            failed diagnostics
        else
            let root = document.ToModel()

            match root.TryGetValue("package") with
            | true, (:? TomlTable as packageTable) ->
                match tryGetRequiredString packageTable "name", tryGetRequiredString packageTable "version" with
                | Ok packageName, Ok packageVersion ->
                    succeeded {
                        projectName = packageName
                        projectVersion = packageVersion
                        projectRoot = projectRoot
                        dependencies = []
                    }
                | Result.Error nameErrors, Ok _ -> failed nameErrors
                | Ok _, Result.Error versionErrors -> failed versionErrors
                | Result.Error nameErrors, Result.Error versionErrors -> failed (nameErrors @ versionErrors)
            | true, _ ->
                failed [ error "`package` must be a table" ]
            | false, _ ->
                failed [ error "missing required table `[package]`" ]

    let createEmptyPlan (request: BuildRequest) : BuildPlan =
        { projectName = ""
          projectVersion = ""
          projectRoot = request.projectRoot
          dependencies = [] }

    let buildProject (request: BuildRequest) : BuildResult =
        let manifestPath = Path.Join(request.projectRoot, manifestFileName)

        if not (File.Exists manifestPath) then
            failed [ error $"atla.toml not found: {manifestPath}" ]
        else
            let manifestText = File.ReadAllText(manifestPath)
            parseManifestToPlan request.projectRoot manifestText
