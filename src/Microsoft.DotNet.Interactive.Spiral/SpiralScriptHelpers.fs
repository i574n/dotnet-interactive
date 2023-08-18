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

[<RequireQualifiedAccess>]
type LangVersion =
    | V47
    | V50
    | Preview

type SpiralScript(?additionalArgs: string[], ?quiet: bool, ?langVersion: LangVersion) =

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

    member _.ValueBound = fsi.ValueBound

    member _.Fsi = fsi

    member _.Eval(code: string, ?cancellationToken: CancellationToken) =
        let code = code |> String.replace "\r\n" "\n"

        let lines = code |> String.split [| '\n' |]

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

        let code =
            if hasMain
            then code
            else code + "\ninl main () = ()\n"

        let timeout = 5000
        let codeAsync =
            code
            |> Supervisor.compileCode timeout cancellationToken
            |> Async.runWithTimeoutAsync timeout
        let cancellationToken = defaultArg cancellationToken CancellationToken.None
        let code =
            Async.RunSynchronously (codeAsync, -1, cancellationToken)
            |> Option.flatten
        match code with
        | Some code ->
            trace Info (fun () -> $"SpiralScriptHelpers.Eval / code:\n{code}") getLocals

            let ch, errors = fsi.EvalInteractionNonThrowing(code, cancellationToken)
            match ch with
            | Choice1Of2 v -> Ok(v), errors
            | Choice2Of2 ex -> Result.Error(ex), errors
        | None ->
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
        task {
            let parseResults, checkResults, _projectResults = fsi.ParseAndCheckInteraction(text)
            let lineText = text.Split('\n').[line - 1]
            let partialName = QuickParse.GetPartialLongNameEx(lineText, column - 1)
            let declarationListInfos = checkResults.GetDeclarationListInfo(Some parseResults, line, lineText, partialName)
            return declarationListInfos.Items
        }

    interface IDisposable with
        member _.Dispose() =
            (fsi :> IDisposable).Dispose()
