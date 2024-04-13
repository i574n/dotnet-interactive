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
        let fsi_eval code cancellationToken =
            let ch, errors = fsi.EvalInteractionNonThrowing (code, cancellationToken)
            let errors =
                errors
                |> Array.map (fun x ->
                    let a = x.Severity
                    let b = x.Message
                    let c = x.ErrorNumber
                    let d = x.Range
                    let a =
                        match a with
                        | FSharpDiagnosticSeverity.Error -> Polyglot.Common.Critical
                        | FSharpDiagnosticSeverity.Warning -> Polyglot.Common.Warning
                        | FSharpDiagnosticSeverity.Info -> Polyglot.Common.Info
                        | FSharpDiagnosticSeverity.Hidden -> Polyglot.Common.Debug
                    let d =
                        d.FileName, (d.StartColumn, d.EndColumn), (d.StartLine, d.EndLine)
                    a, b, c, d
                )
            ch, errors

        let ch, errors = code |> Polyglot.Eval.eval fsi_eval cancellationToken
        let errors =
            errors
            |> Array.map (fun (severity, message, error, (file, (a, b), (c, d))) ->
                let severity =
                    match severity with
                    | Polyglot.Common.Critical -> FSharpDiagnosticSeverity.Error
                    | Polyglot.Common.Warning -> FSharpDiagnosticSeverity.Warning
                    | Polyglot.Common.Info -> FSharpDiagnosticSeverity.Info
                    | _ -> FSharpDiagnosticSeverity.Hidden
                let range =
                    Text.Range.mkRange
                        file
                        (Text.Position.mkPos a b)
                        (Text.Position.mkPos c d)
                FSharpDiagnostic.Create (severity, message, error, range)
            )

        ch, errors

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
