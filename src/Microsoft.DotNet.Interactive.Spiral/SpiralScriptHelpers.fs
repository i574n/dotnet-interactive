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

    let log2 (text : string) =
        try
            let tmpPath = Path.GetTempPath ()
            let logDir = Path.Combine (tmpPath, "_log_spiral_kernel")
            Directory.CreateDirectory logDir |> ignore
            let dateTimeStr = DateTime.Now.ToString "yyyy-MM-dd HH-mm-ss-fff"
            let logFile = Path.Combine (logDir, $"log_{dateTimeStr}_{Random().Next()}.txt")
            let dateTimeStr = DateTime.Now.ToString "yyyy-MM-dd HH:mm:ss.fff"
            let fileName = "SpiralScriptHelpers"
            File.AppendAllText (logFile, $"{dateTimeStr} {fileName} {text}{Environment.NewLine}") |> ignore
        with ex ->
            Polyglot.Common.trace Polyglot.Common.Debug (fun () -> $"V.log / ex: {ex |> Polyglot.Common.printException}") Polyglot.Common.getLocals

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
            Polyglot.Common.trace Polyglot.Common.Debug (fun () -> $"SpiralScriptHelpers.log / ex: {ex |> Polyglot.Common.printException}") Polyglot.Common.getLocals
            log2 text

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
        try
            let streamAsyncChild =
                stream
                |> FSharp.Control.AsyncSeq.iterAsyncParallel (fun (ticks, event) -> async {
                    try
                        let getLocals () = $"ticks: {ticks} / event: {event} / {getLocals ()}"
                        match event with
                        | FileSystemChange.Created (path, Some code) ->
                            let! tokens = code |> Supervisor.getCodeTokenRange 10000 None
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
                            let! tokens = code |> Supervisor.getCodeTokenRange 10000 None
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
        with ex ->
            log $"SpiralScriptHelpers / ex: {ex |> printException}"

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

        let rawCellCode =
            if code <> "//// trace"
            then code |> String.replace "\r\n" "\n"
            else
                if traceLevel = Info
                then traceLevel <- Verbose
                else traceLevel <- Info
                "inl main () = ()"

        let isRust = rawCellCode |> String.startsWith "// // rust"

        let lines = rawCellCode |> String.split [| '\n' |]

        if lines |> Array.exists (fun line -> line |> String.startsWith "#r " && line |> String.endsWith "\"") then
            let cancellationToken = defaultArg cancellationToken CancellationToken.None
            let ch, errors = fsi.EvalInteractionNonThrowing(code, cancellationToken)
            match ch with
            | Choice1Of2 v -> Ok(v), errors
            | Choice2Of2 ex -> Result.Error(ex), errors
        else
            try
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
                    then rawCellCode, None
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
                                    || line |> String.startsWith "nominal " -> lastTopLevelIndex, true
                                | line when
                                    line |> String.startsWith "inl "
                                    || line |> String.startsWith "let " ->
                                    let m =
                                        System.Text.RegularExpressions.Regex.Match (
                                            line,
                                            @"^(inl|let) +([\w\d]+) +(:|=)"
                                        )
                                    trace Debug (fun () -> $"m: '{m}' / m.Groups.Count: {m.Groups.Count}") getLocals
                                    if m.Groups.Count = 4
                                    then Some i, false
                                    else lastTopLevelIndex, true
                                | _ -> Some i, false
                            )
                        let code =
                            match lastTopLevelIndex with
                            | Some lastTopLevelIndex ->
                                lines
                                |> Array.mapi (fun i line ->
                                    match i with
                                    | i when i < lastTopLevelIndex -> line
                                    | i when i = lastTopLevelIndex -> $"\nlet main () =\n    {line}"
                                    | _ when line |> String.trim = "" -> ""
                                    | _ -> $"    {line}"
                                )
                                |> String.concat "\n"
                            | None -> $"{rawCellCode}\n\ninl main () = ()\n"
                        code, lastTopLevelIndex

                let newAllCode = $"{allCode}\n\n{cellCode}"

                let timeout = 60000 * 3
                async {
                    try
                        let! mainPath, disposable =
                            newAllCode
                            |> Supervisor.persistCode timeout cancellationToken
                        use _ = disposable
                        let! code =
                            mainPath
                            |> Supervisor.buildFile timeout cancellationToken
                            |> Async.runWithTimeoutAsync timeout
                        match code with
                        | Some (Some code, spiralErrors) ->
                            let spiralErrors = self.mapErrors (FSharpDiagnosticSeverity.Warning, spiralErrors, lastTopLevelIndex)
                            let inline _trace (fn : unit -> string) =
                                if traceLevel = Info
                                then fn () |> System.Console.WriteLine
                                else trace Info (fun () -> $"SpiralScriptHelpers.Eval / {fn ()}") getLocals

                            _trace (fun () -> if isRust then $".fsx:\n{code}" else code)

                            let! rustResult =
                                if not isRust || lastTopLevelIndex = None
                                then None |> Async.init
                                else
                                    async {
                                        let repositoryRoot = FileSystem.getSourceDirectory () |> FileSystem.findParent ".paket" false
                                        let projectDir = repositoryRoot </> "target/!fs-rs"
                                        let hash = $"repl_{code |> Crypto.hashText}"
                                        let! fsprojPath = Builder.persistCodeProject ["Fable.Core"] [] projectDir hash code
                                        let outPath = projectDir </> "target/rs"
                                        let! exitCode, result =
                                            Runtime.executeWithOptionsAsync
                                                {
                                                    Command = $@"dotnet fable {fsprojPath} --optimize --lang rs --extension .rs --outDir {outPath} --noCache"
                                                    CancellationToken = cancellationToken
                                                    WorkingDirectory = None
                                                    OnLine = None
                                                }

                                        if exitCode <> 0
                                        then return Some (Result.Error result)
                                        else
                                            let rsPath = outPath </> $"{hash}.rs"
                                            let! rsCode = rsPath |> FileSystem.readAllTextAsync
                                            let rsCode = rsCode |> String.replace "),);" "));"

                                            _trace (fun () -> $"\n.rs:\n{rsCode}")

                                            do!
                                                $"{rsCode}\n\npub fn main() -> Result<(), String> {{ Ok(()) }}\n"
                                                |> FileSystem.writeAllTextAsync rsPath


                                            let cargoTomlPath = outPath </> $"Cargo.toml"
                                            let cargoTomlContent = $"""[package]
    name = "{hash}"
    version = "0.0.1"
    edition = "2021"

    [workspace]

    [dependencies]
    fable_library_rust = {{ path = "fable_modules/fable-library-rust", optional = true, default-features = false }}

    [features]
    default = ["fable_library_rust/default", "fable_library_rust/static_do_bindings"]

    [[bin]]
    name = "{hash}"
    path = "{hash}.rs"
    """
                                            do! cargoTomlContent |> FileSystem.writeAllTextExists cargoTomlPath

                                            let! exitCode, result =
                                                Runtime.executeWithOptionsAsync
                                                    {
                                                        Command = $@"cargo run --release --manifest-path {cargoTomlPath}"
                                                        CancellationToken = cancellationToken
                                                        WorkingDirectory = None
                                                        OnLine = None
                                                    }

                                            if exitCode = 0 then
                                                let result =
                                                    result
                                                    |> String.split [| '\n' |]
                                                    |> Array.skipWhile (fun line ->
                                                        line |> String.contains @"Finished release [optimized] target" |> not
                                                    )
                                                    |> Array.skip 2
                                                    |> String.concat "\n"
                                                return Some (Ok result)
                                            else
                                                return Some (Result.Error result)
                                    }

                            let cancellationToken = defaultArg cancellationToken CancellationToken.None

                            let fsxResult =
                                if isRust
                                then None
                                else
                                    try
                                        let ch, errors = fsi.EvalInteractionNonThrowing(code, cancellationToken)
                                        let errors =
                                            errors
                                            |> Array.map (fun error ->
                                                FSharpDiagnostic.Create (error.Severity, error.Message, error.ErrorNumber, Text.range.Zero)
                                            )
                                        Some (ch, errors)
                                    with ex ->
                                        if ex.Message |> String.contains "Could not load type 'FSI_" |> not
                                        then raise ex
                                        else
                                            trace Error (fun () -> $"SpiralScriptHelpers.Eval / ex: {ex |> printException}") getLocals
                                            None

                            match fsxResult, rustResult with
                            | Some (ch, errors), None ->
                                let errors = errors |> Array.append spiralErrors
                                match ch with
                                | Choice1Of2 v ->
                                    allCode <- newAllCode
                                    return Ok(v), errors
                                | Choice2Of2 ex -> return Result.Error(ex), errors
                            | _, Some result ->
                                let result, errors =
                                    match result with
                                    | Ok result -> result, [||]
                                    | Result.Error error ->
                                        "",
                                        [|
                                            FSharpDiagnostic.Create (
                                                FSharpDiagnosticSeverity.Error, error, 0, Text.range.Zero
                                            )
                                        |]

                                let ch, errors2 = fsi.EvalInteractionNonThrowing($"\"\"\".rs output:\n{result}\n\"\"\"", cancellationToken)
                                let errors =
                                    errors
                                    |> Array.append spiralErrors
                                    |> Array.append errors2
                                match ch with
                                | Choice1Of2 v ->
                                    allCode <- newAllCode
                                    return Ok(v), errors
                                | Choice2Of2 ex ->
                                    return Result.Error(ex), errors
                            | _ ->
                                let ch, errors = fsi.EvalInteractionNonThrowing("()", cancellationToken)
                                match ch with
                                | Choice1Of2 v ->
                                    allCode <- newAllCode
                                    return Ok(v), errors
                                | Choice2Of2 ex ->
                                    return Result.Error(ex), errors
                        | Some (None, errors) when errors |> List.isEmpty |> not ->
                            return errors.[0] |> fst |> Exception |> Result.Error,
                            self.mapErrors (FSharpDiagnosticSeverity.Error, errors, lastTopLevelIndex)
                        | _ ->
                            return Result.Error (Exception "Spiral error or timeout"),
                            [|
                                FSharpDiagnostic.Create (
                                    FSharpDiagnosticSeverity.Error, "Diag: Spiral error or timeout", 0, Text.range.Zero
                                )
                            |]
                    with ex ->
                        log $"Eval / ex: {ex |> printException}"
                        return Result.Error (Exception "Spiral error or timeout (4)"),
                        [|
                            FSharpDiagnostic.Create (
                                FSharpDiagnosticSeverity.Error, "Diag: Spiral error or timeout (4)", 0, Text.range.Zero
                            )
                        |]
                }
                |> Async.runWithTimeoutStrict timeout
                |> Option.defaultValue (
                    Result.Error (Exception "Spiral error or timeout (2)"),
                    [|
                        FSharpDiagnostic.Create (
                            FSharpDiagnosticSeverity.Error, "Diag: Spiral error or timeout (2)", 0, Text.range.Zero
                        )
                    |]
                )
            with ex ->
                log $"Eval / ex: {ex |> printException}"
                Result.Error (Exception "Spiral error or timeout (3)"),
                [|
                    FSharpDiagnostic.Create (
                        FSharpDiagnosticSeverity.Error, "Diag: Spiral error or timeout (3)", 0, Text.range.Zero
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
