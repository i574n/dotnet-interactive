// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.DotNet.Interactive.Spiral.ScriptHelpers

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Threading
open FSharp.Compiler
open FSharp.Compiler.Interactive.Shell
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices

open Polyglot
open Polyglot.Common
open Polyglot.FileSystem

[<RequireQualifiedAccess>]
type LangVersion =
    | V47
    | V50
    | Preview

type SpiralScript(?additionalArgs: string[], ?quiet: bool, ?langVersion: LangVersion) as self =

    let additionalArgs = defaultArg additionalArgs [||]
    let quiet = defaultArg quiet true
    let langVersion = defaultArg langVersion LangVersion.Preview
    let config = FsiEvaluationSession.GetDefaultConfiguration()

    let computedProfile =
        // If we are being executed on the desktop framework (we can tell because the assembly containing int is mscorlib) then profile must be mscorlib otherwise use netcore
        if typeof<int>.Assembly.GetName().Name = "mscorlib" then "mscorlib"
        else "netcore"

    let baseArgs = [|
        typeof<SpiralScript>.Assembly.Location;
        "--noninteractive";
        "--targetprofile:" + computedProfile
        if quiet then "--quiet"
        match langVersion with
        | LangVersion.V47 -> "--langversion:4.7"
        | LangVersion.V50 -> "--langversion:5.0"
        | LangVersion.Preview -> "--langversion:preview"
        |]

    let argv = Array.append baseArgs additionalArgs

    let fsi = FsiEvaluationSession.Create (config, argv, stdin, stdout, stderr)

    let log (text : string) =
        try
            let tmpPath = Path.GetTempPath ()
            let logDir = Path.Combine (tmpPath, "_log_spiral_kernel")
            Directory.CreateDirectory logDir |> ignore
            let logFile = Path.Combine (logDir, "log.txt")
            let dateTimeStr = DateTime.Now.ToString "yyyy-MM-dd HH:mm:ss.fff"
            let fileName = "SpiralScriptHelpers"
            File.AppendAllText (logFile, $"{dateTimeStr} {fileName} {text}{Environment.NewLine}") |> ignore
        with ex ->
            trace Debug (fun () -> $"SpiralScriptHelpers.log / ex: {ex |> printException}") getLocals

    do
        log $"SpiralScript () / argv: %A{argv}"

    let tmpSpiralPath = Path.GetTempPath () </> "!dotnet-interactive-spiral"
    let tmpCodePath = tmpSpiralPath </> "code"
    let tmpTokensPath = tmpSpiralPath </> "tokens"

    do
        [tmpSpiralPath; tmpCodePath; tmpTokensPath]
        |> List.iter (fun dir -> if Directory.Exists dir |> not then Directory.CreateDirectory dir |> ignore)

    let stream, disposable = watchDirectory true tmpCodePath

    do
        let streamAsyncChild =
            stream
            |> FSharp.Control.AsyncSeq.iterAsyncParallel (fun (ticks, event) -> async {
                try
                    let getLocals () = $"ticks: {ticks} / event: {event} / {getLocals ()}"
                    match event with
                    | FileSystemChange.Created (path, Some code) ->
                        let! tokens = code |> Supervisor.getCodeTokenRange 5000 None
                        match tokens with
                        | Some tokens ->
                            do! tokens |> FSharp.Json.Json.serialize |> writeAllTextAsync (tmpTokensPath </> path)
                        | None ->
                            log $"SpiralScriptHelpers.watchDirectory / iterAsyncParallel / tokens: None / {getLocals ()}"
                    | _ -> ()
                with ex ->
                    log $"SpiralScriptHelpers.watchDirectory / iterAsyncParallel / ex: {ex |> printException} / {getLocals ()}"
            })
            |> Async.StartChild

        let existingFilesChild =
            tmpCodePath
            |> System.IO.Directory.GetFiles
            |> Array.map (fun codePath -> async {
                try
                    let tokensPath = tmpTokensPath </> (codePath |> System.IO.Path.GetFileName)
                    if File.Exists tokensPath |> not then
                        let! code = codePath |> readAllTextAsync
                        let! tokens = code |> Supervisor.getCodeTokenRange 5000 None
                        match tokens with
                        | Some tokens ->
                            do! tokens |> FSharp.Json.Json.serialize |> writeAllTextAsync tokensPath
                        | None ->
                            log $"SpiralScriptHelpers.watchDirectory / GetFiles / tokens: None / {getLocals ()}"
                with ex ->
                    log $"SpiralScriptHelpers.watchDirectory / GetFiles / ex: {ex |> printException} / {getLocals ()}"
            })
            |> Async.Sequential
            |> Async.Ignore
            |> Async.StartChild

        async {
            do! Async.Sleep 3000
            let! _ = streamAsyncChild
            let! _ = existingFilesChild
            ()
        }
        |> Async.StartImmediate

    let mutable allCode = ""


    member _.ValueBound = fsi.ValueBound

    member _.Fsi = fsi

    member _.mapErrors (severity, errors, lastTopLevelIndex) =
        let allCodeLineLength =
            allCode |> String.split [| '\n' |] |> Array.length

        errors
        |> List.map (fun (_, error) ->
            match error with
            | Supervisor.FatalError message ->
                FSharpDiagnostic.Create (
                    severity, message, 0, Text.range.Zero
                )
                |> List.singleton
            | Supervisor.TracedError data ->
                data.trace
                |> List.truncate 5
                |> List.append [ data.message ]
                |> List.map (fun message ->
                    FSharpDiagnostic.Create (
                        severity, message, 0, Text.range.Zero
                    )
                )
            | Supervisor.PackageErrors data
            | Supervisor.TokenizerErrors data
            | Supervisor.ParserErrors data
            | Supervisor.TypeErrors data ->
                data.errors
                |> List.filter (fun ((rangeStart, _), _) ->
                    trace Debug (fun () -> $"SpiralScriptHelpers.mapErrors / rangeStart.line: {rangeStart.line} / lastTopLevelIndex: {lastTopLevelIndex} / allCodeLineLength: {allCodeLineLength} / filtered: {rangeStart.line > allCodeLineLength}") getLocals
                    rangeStart.line > allCodeLineLength
                )
                |> List.map (fun ((rangeStart, rangeEnd), message) ->
                    FSharpDiagnostic.Create (
                        severity,
                        message,
                        0,
                        Text.Range.mkRange
                            (data.uri |> System.IO.Path.GetFileName)
                            (Text.Position.mkPos
                                (match lastTopLevelIndex with
                                | Some i when rangeStart.line >= i + allCodeLineLength + 3 ->
                                    rangeStart.line - allCodeLineLength - 2
                                | _ -> rangeStart.line - allCodeLineLength)
                                (match lastTopLevelIndex with
                                | Some i when rangeStart.line >= i + allCodeLineLength + 3 ->
                                    rangeStart.character - 4
                                | _ -> rangeStart.character)
                            )
                            (Text.Position.mkPos
                                (match lastTopLevelIndex with
                                | Some i when rangeStart.line >= i + allCodeLineLength + 3 ->
                                    rangeEnd.line - allCodeLineLength - 2
                                | _ -> rangeEnd.line - allCodeLineLength)
                                (match lastTopLevelIndex with
                                | Some i when rangeStart.line >= i + allCodeLineLength + 3 ->
                                    rangeEnd.character - 4
                                | _ -> rangeEnd.character)
                            )
                    )
                )
        )
        |> List.collect id
        |> List.toArray

    member _.Eval(code: string, ?cancellationToken: CancellationToken) =
        log $"Eval / code: %A{code}"

        if code = "//// trace" then
            if traceLevel = Info
            then traceLevel <- Verbose
            else traceLevel <- Info

        let cellCode = code |> String.replace "\r\n" "\n"

        let lines = cellCode |> String.split [| '\n' |]

        let lastBlock =
            lines
            |> Array.tryFindBack (fun line ->
                line |> String.length > 0
                && line.[0] <> ' '
            )

        let hasMain =
            lastBlock
            |> Option.exists (fun line ->
                line |> String.startsWith "inl main "
                || line |> String.startsWith "let main "
            )

        let cellCode, lastTopLevelIndex =
            if hasMain
            then cellCode, None
            else
                let lastTopLevelIndex, _ =
                    (lines |> Array.indexed, (None, false))
                    ||> Array.foldBack (fun (i, line) (lastTopLevelIndex, finished) ->
                        trace Debug (fun () -> $"i: {i} / line: '{line}' / lastTopLevelIndex: {lastTopLevelIndex} / finished: {finished}") getLocals
                        match line with
                        | _ when finished -> lastTopLevelIndex, true
                        | "" when lastTopLevelIndex |> Option.isSome -> Some i, false
                        | "" -> lastTopLevelIndex, false
                        | line when line |> String.startsWith " " -> lastTopLevelIndex, false
                        | line when
                            line |> String.startsWith "open "
                            || line |> String.startsWith "prototype "
                            || line |> String.startsWith "instance "
                            || line |> String.startsWith "type "
                            || line |> String.startsWith "union "
                            || line |> String.startsWith "inl "
                            || line |> String.startsWith "let "
                            || line |> String.startsWith "nominal " -> lastTopLevelIndex, true
                        | _ -> Some i, false
                    )
                let code =
                    match lastTopLevelIndex with
                    | Some lastTopLevelIndex ->
                        lines
                        |> Array.mapi (fun i line ->
                            match i with
                            | i when i < lastTopLevelIndex -> line
                            | i when i = lastTopLevelIndex -> $"\ninl main () =\n    {line}"
                            | _ when line |> String.trim = "" -> ""
                            | _ -> $"    {line}"
                        )
                        |> String.concat "\n"
                    | None -> $"{cellCode}\n\ninl main () = ()\n"
                code, lastTopLevelIndex

        let newAllCode = $"{allCode}\n\n{cellCode}"

        let timeout = 5000
        let codeAsync =
            newAllCode
            |> Supervisor.buildCode timeout cancellationToken
            |> Async.runWithTimeoutAsync timeout
        let cancellationToken = defaultArg cancellationToken CancellationToken.None
        let code =
            Async.RunSynchronously (codeAsync, -1, cancellationToken)
        match code with
        | Some (Some code, spiralErrors) ->
            let spiralErrors = self.mapErrors (FSharpDiagnosticSeverity.Warning, spiralErrors, lastTopLevelIndex)
            trace Info (fun () -> $"SpiralScriptHelpers.Eval / code:\n{code}") getLocals

            let ch, errors = fsi.EvalInteractionNonThrowing(code, cancellationToken)
            let errors =
                errors
                |> Array.map (fun error ->
                    FSharpDiagnostic.Create (error.Severity, error.Message, error.ErrorNumber, Text.range.Zero)
                )
                |> Array.append spiralErrors
            match ch with
            | Choice1Of2 v ->
                allCode <- newAllCode
                Ok(v), errors
            | Choice2Of2 ex -> Result.Error(ex), errors
        | Some (None, errors) when errors |> List.isEmpty |> not ->
            errors.[0] |> fst |> Exception |> Result.Error,
            self.mapErrors (FSharpDiagnosticSeverity.Error, errors, lastTopLevelIndex)
        | _ ->
            Result.Error (Exception "Spiral error or timeout"),
            [|
                FSharpDiagnostic.Create (
                    FSharpDiagnosticSeverity.Error, "Diag: Spiral error or timeout", 0, Text.range.Zero
                )
            |]


    /// Get the available completion items from the code at the specified location.
    ///
    /// <param name="text">The input text on which completions will be calculated</param>
    /// <param name="line">The 1-based line index</param>
    /// <param name="column">The 0-based column index</param>
    member _.GetCompletionItems(text: string, line: int, column: int) =
        log $"GetCompletionItems / text: %A{text} / line: %A{line} / column: %A{column}"

        task {
            let parseResults, checkResults, _projectResults = fsi.ParseAndCheckInteraction(text)
            let lineText = text.Split('\n').[line - 1]
            let partialName = QuickParse.GetPartialLongNameEx(lineText, column - 1)
            let declarationListInfos = checkResults.GetDeclarationListInfo(Some parseResults, line, lineText, partialName)
            return declarationListInfos.Items
        }

    interface IDisposable with
        member _.Dispose() =
            log $"Dispose"
            (fsi :> IDisposable).Dispose()
            disposable.Dispose ()
