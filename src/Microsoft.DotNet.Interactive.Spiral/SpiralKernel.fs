// Copyright (c) .NET Foundation and contributors. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Microsoft.DotNet.Interactive.Spiral

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Collections.Concurrent
open System.IO
open System.Runtime.InteropServices
open System.Xml
open System.Text
open System.Threading
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Tags
open Microsoft.DotNet.Interactive
open Microsoft.DotNet.Interactive.Formatting
open Microsoft.DotNet.Interactive.Commands
open Microsoft.DotNet.Interactive.Events
open Microsoft.DotNet.Interactive.Spiral.ScriptHelpers
open Microsoft.DotNet.Interactive.ValueSharing

open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Interactive.Shell
open FSharp.Compiler.Text.Position

open FsAutoComplete
open FSharp.Compiler.Symbols

type SpiralKernel () as this =

    inherit Kernel("spiral")

    do this.KernelInfo.LanguageName <- "Spiral"
    do this.KernelInfo.LanguageVersion <- "2.3"
    do this.KernelInfo.DisplayName <- $"{this.KernelInfo.LocalName} - Spiral Script"

    do Polyglot.Common.traceLevel <- Polyglot.Common.TraceLevel.Info

    static let lockObj = Object();

    let createScript () =
        let additionalArgs = [|
            "/langversion:preview"
            "/usesdkrefs-" // work around ref/impl type resolution; see https://github.com/dotnet/fsharp/issues/10496
            "--multiemit-" // work around inability to reference types defined in this assembly; see https://github.com/dotnet/fsharp/issues/13197
            |]
        lock lockObj (fun () -> new SpiralScript(additionalArgs=additionalArgs))

    let script = lazy createScript ()

    let mutable cancellationTokenSource = new CancellationTokenSource()

    let xmlDocuments = ConcurrentDictionary<string, XmlDocument>(StringComparer.OrdinalIgnoreCase)

    [<DefaultValue>] val mutable workingDirectory : string

    let log (text : string) =
        try
            let tmpPath = Path.GetTempPath ()
            let logDir = Path.Combine (tmpPath, "_log_spiral_kernel")
            Directory.CreateDirectory logDir |> ignore
            let logFile = Path.Combine (logDir, "log.txt")
            let dateTimeStr = DateTime.Now.ToString "yyyy-MM-dd HH:mm:ss.fff"
            let fileName = "SpiralKernel"
            File.AppendAllText (logFile, $"{dateTimeStr} {fileName} {text}{Environment.NewLine}") |> ignore
        with ex ->
            Polyglot.Common.trace Polyglot.Common.Debug (fun () -> $"SpiralKernel.log / ex: {ex |> Polyglot.Common.printException}") Polyglot.Common.getLocals

    let serialize1 obj =
        try
            obj |> FSharp.Json.Json.serializeEx (FSharp.Json.JsonConfig.create false)
        with ex ->
            log $"SpiralKernel.serialize1 / ex: {ex |> Polyglot.Common.printException}"
            "Serialize error"

    let serialize2 obj =
        try
            Json.JsonSerializer.Serialize (
                    obj,
                    Json.JsonSerializerOptions (
                        ReferenceHandler = Json.Serialization.ReferenceHandler.IgnoreCycles,
                        WriteIndented = true
                    )
            )
        with ex ->
            log $"SpiralKernel.serialize2 / ex: {ex |> Polyglot.Common.printException}"
            "Serialize error"

    let serialize obj =
        let result = serialize1 obj
        if result = "Serialize error"
        then serialize2 obj
        else $"%A{obj}"

    let getKindString (glyph: FSharpGlyph) =
        log $"getKindString / glyph: %A{glyph}"

        match glyph with
        | FSharpGlyph.Class -> WellKnownTags.Class
        | FSharpGlyph.Constant -> WellKnownTags.Constant
        | FSharpGlyph.Delegate -> WellKnownTags.Delegate
        | FSharpGlyph.Enum -> WellKnownTags.Enum
        | FSharpGlyph.EnumMember -> WellKnownTags.EnumMember
        | FSharpGlyph.Event -> WellKnownTags.Event
        | FSharpGlyph.Exception -> WellKnownTags.Class
        | FSharpGlyph.Field -> WellKnownTags.Field
        | FSharpGlyph.Interface -> WellKnownTags.Interface
        | FSharpGlyph.Method -> WellKnownTags.Method
        | FSharpGlyph.OverridenMethod -> WellKnownTags.Method
        | FSharpGlyph.Module -> WellKnownTags.Module
        | FSharpGlyph.NameSpace -> WellKnownTags.Namespace
        | FSharpGlyph.Property -> WellKnownTags.Property
        | FSharpGlyph.Struct -> WellKnownTags.Structure
        | FSharpGlyph.Typedef -> WellKnownTags.Class
        | FSharpGlyph.Type -> WellKnownTags.Class
        | FSharpGlyph.TypeParameter -> WellKnownTags.TypeParameter
        | FSharpGlyph.Union -> WellKnownTags.Enum
        | FSharpGlyph.Variable -> WellKnownTags.Local
        | FSharpGlyph.ExtensionMethod -> WellKnownTags.ExtensionMethod
        | FSharpGlyph.Error -> WellKnownTags.Error

    let getFilterText (declarationItem: DeclarationListItem) =
        log $"getFilterText / declarationItem: %A{declarationItem}"

        match declarationItem.NamespaceToOpen, declarationItem.NameInList.Split '.' with
        // There is no namespace to open and the item name does not contain dots, so we don't need to pass special FilterText to Roslyn.
        | None, [|_|] -> null
        // Either we have a namespace to open ("DateTime (open System)") or item name contains dots ("Array.map"), or both.
        // We are passing last part of long ident as FilterText.
        | _, idents -> Array.last idents

    let tryGetXmlDocument xmlFile =
        log $"tryGetXmlDocument / xmlFile: %A{xmlFile}"

        match xmlDocuments.TryGetValue(xmlFile) with
        | true, doc -> Some doc
        | _ ->
            if not (File.Exists(xmlFile)) ||
               not (String.Equals(Path.GetExtension(xmlFile), ".xml", StringComparison.OrdinalIgnoreCase)) then
                None
            else
                try
                    let doc = System.Xml.XmlDocument()
                    use xmlStream = File.OpenRead(xmlFile)
                    doc.Load(xmlStream)
                    xmlDocuments.[xmlFile] <- doc
                    Some doc
                with
                | _ ->
                    None

    let tryGetDocumentationByXmlFileAndKey xmlFile key =
        log $"tryGetDocumentationByXmlFileAndKey / xmlFile: %A{xmlFile}, key: %A{key}"

        tryGetXmlDocument xmlFile
        |> Option.bind (fun doc ->
            match doc.SelectSingleNode(sprintf "doc/members/member[@name='%s']" key) with
            | null -> None
            | node ->
                match node.SelectSingleNode("summary") with
                | null -> None
                | summaryNode -> Some summaryNode.InnerText)

    let tryGetDocumentationByToolTipElementData (dataList: ToolTipElementData list) =
        log $"tryGetDocumentationByToolTipElementData / dataList: %A{dataList}"

        let text =
            let xmlData =
                dataList
                |> List.map (fun data ->
                    match data.XmlDoc with
                    | FSharpXmlDoc.FromXmlText xmlDoc when xmlDoc.UnprocessedLines.Length > 0 ->
                        sprintf "%s" (String.concat "" xmlDoc.UnprocessedLines)
                    | FSharpXmlDoc.FromXmlFile (file, key) ->
                        let xmlFile = Path.ChangeExtension(file, "xml")
                        match tryGetDocumentationByXmlFileAndKey xmlFile key with
                        | Some docText -> sprintf "%s" docText
                        | _ -> String.Empty
                    | _ ->
                        String.Empty
                )
            if xmlData.IsEmpty then String.Empty
            else
                xmlData
                |> String.concat ""
        if String.IsNullOrWhiteSpace(text) then None
        else Some text

    let getDocumentation (declarationItem: DeclarationListItem) =
        log $"getDocumentation / declarationItem: %A{declarationItem}"

        task {
            match declarationItem.Description with
            | ToolTipText(elements) ->
                return
                    elements
                    |> List.choose (fun element ->
                        match element with
                        | ToolTipElement.Group(dataList) ->
                            tryGetDocumentationByToolTipElementData dataList
                        | _ ->
                            None
                    )
                    |> String.concat ""
        }

    let getCompletionItem (declarationItem: DeclarationListItem) =
        log $"getCompletionItem / declarationItem: %A{declarationItem}"

        task {
            let kind = getKindString declarationItem.Glyph
            let filterText = getFilterText declarationItem
            let! documentation = getDocumentation declarationItem
            let isMethod =
                match declarationItem.Kind with
                | CompletionItemKind.Method _ -> true
                | _ -> false
            let insertTextSuffix = if isMethod then "($1)" else ""
            let insertTextFormat = if isMethod then System.Nullable(InsertTextFormat.Snippet) else System.Nullable<InsertTextFormat>()
            return CompletionItem(declarationItem.NameInList, kind, filterText=filterText, documentation=documentation, insertText=declarationItem.NameInCode + insertTextSuffix, insertTextFormat=insertTextFormat)
        }

    let getDiagnostic (error: FSharpDiagnostic) =
        log $"getDiagnostic / error: %A{error}"

        // F# errors are 1-based but should be 0-based for diagnostics, however, 0-based errors are still valid to report
        let diagLineDelta = if error.Start.Line = 0 then 0 else -1
        let startPos = LinePosition(error.Start.Line + diagLineDelta, error.Start.Column)
        let endPos = LinePosition(error.End.Line + diagLineDelta, error.End.Column)
        let linePositionSpan = LinePositionSpan(startPos, endPos)
        let severity =
            match error.Severity with
            | FSharpDiagnosticSeverity.Error -> DiagnosticSeverity.Error
            | FSharpDiagnosticSeverity.Warning -> DiagnosticSeverity.Warning
            | FSharpDiagnosticSeverity.Hidden -> DiagnosticSeverity.Hidden
            | FSharpDiagnosticSeverity.Info -> DiagnosticSeverity.Info
        let errorId = sprintf "FS%04i" error.ErrorNumber
        Diagnostic(linePositionSpan, severity, errorId, error.Message)

    let handleChangeWorkingDirectory (changeDirectory: ChangeWorkingDirectory) (context: KernelInvocationContext) =
        log $"handleChangeWorkingDirectory / changeDirectory: %A{changeDirectory |> serialize2}"

        task {
            this.workingDirectory <- changeDirectory.WorkingDirectory;
            return Task.CompletedTask;
        }

    let handleSubmitCode (codeSubmission: SubmitCode) (context: KernelInvocationContext) =
        log $"handleSubmitCode / codeSubmission: %A{codeSubmission |> serialize2}"

        task {
            let codeSubmissionReceived = CodeSubmissionReceived(codeSubmission)
            context.Publish(codeSubmissionReceived)
            log $"handleSubmitCode / Publish(CodeSubmissionReceived): %A{codeSubmissionReceived |> serialize2}"

            let tokenSource =
                CancellationTokenSource.CreateLinkedTokenSource
                    [|
                        cancellationTokenSource.Token
                        context.CancellationToken
                    |]
            let result, fsiDiagnostics =
                try
                    script.Value.Eval(codeSubmission.Code, tokenSource.Token)
                with
                | ex -> Error(ex), [||]

            // script.Eval can succeed with error diagnostics, see https://github.com/dotnet/interactive/issues/691
            let isError = fsiDiagnostics |> Array.exists (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

            log $"handleSubmitCode / fsiDiagnostics:\n{fsiDiagnostics |> Array.map (fun x -> x.ToString ()) |> serialize}"

            if fsiDiagnostics.Length > 0 then
                let diagnostics = fsiDiagnostics |> Array.map getDiagnostic |> fun x -> x.ToImmutableArray()

                let formattedDiagnostics =
                    fsiDiagnostics
                    |> Array.map (fun d -> d.ToString())
                    |> Array.map (fun text -> new FormattedValue(PlainTextFormatter.MimeType, text))

                context.Publish(DiagnosticsProduced(diagnostics, codeSubmission, formattedDiagnostics))
                log $"handleSubmitCode / Publish(DiagnosticsProduced): %A{DiagnosticsProduced(diagnostics, codeSubmission, formattedDiagnostics) |> serialize2}"

            match result with
            | Ok(result) when not isError ->
                match result with
                | Some(value) when value.ReflectionType <> typeof<unit> ->
                    let value = value.ReflectionValue
                    let formattedValues = FormattedValue.CreateManyFromObject(value)
                    context.Publish(ReturnValueProduced(value, codeSubmission, formattedValues))
                    log $"handleSubmitCode / Publish(ReturnValueProduced): %A{ReturnValueProduced(value, codeSubmission, formattedValues) |> serialize2}"


                | Some _
                | None -> ()
            | _ ->
                if not (tokenSource.IsCancellationRequested) then
                    let aggregateError = String.Join("\n", fsiDiagnostics)
                    match result with
                    | Error (:? FsiCompilationException)
                    | Ok _ ->
                        let ex = CodeSubmissionCompilationErrorException(Exception(aggregateError))
                        context.Fail(codeSubmission, ex, aggregateError)
                        log $"handleSubmitCode / Fail / codeSubmission: %A{codeSubmission} / ex: %A{ex} / aggregateError: %A{aggregateError}"
                    | Error ex ->
                        context.Fail(codeSubmission, ex, null)
                        log $"handleSubmitCode / Fail / codeSubmission: %A{codeSubmission} / ex: %A{ex}"
                else
                    context.Fail(codeSubmission, null, "Command cancelled")
                    log $"handleSubmitCode / Fail / codeSubmission: %A{codeSubmission} / Command cancelled"
        }

    let handleRequestCompletions (requestCompletions: RequestCompletions) (context: KernelInvocationContext) =
        // log $"handleRequestCompletions / requestCompletions: %A{requestCompletions |> serialize}"

        task {
            ()
            // let! declarationItems = script.Value.GetCompletionItems(requestCompletions.Code, requestCompletions.LinePosition.Line + 1, requestCompletions.LinePosition.Character)
            // let! completionItems =
            //     declarationItems
            //     |> Array.map getCompletionItem
            //     |> Task.WhenAll
            // context.Publish(CompletionsProduced(completionItems, requestCompletions))
            // log $"handleRequestCompletions / Publish(CompletionsProduced): %A{CompletionsProduced(completionItems, requestCompletions)}"
        }

    let handleRequestHoverText (requestHoverText: RequestHoverText) (context: KernelInvocationContext) =
        log $"handleRequestHoverText / requestHoverText: %A{requestHoverText |> serialize2}"

        task {
            let fsiModuleRx = System.Text.RegularExpressions.Regex @"FSI_[0-9]+\."
            let stdinRx = System.Text.RegularExpressions.Regex @"Stdin\."
            let parse, check, _ctx = script.Value.Fsi.ParseAndCheckInteraction(requestHoverText.Code)

            let res = FsAutoComplete.ParseAndCheckResults(parse, check, EntityCache())
            let text = FSharp.Compiler.Text.SourceText.ofString requestHoverText.Code

            // seem to be off by one
            let line = requestHoverText.LinePosition.Line + 1
            let col = requestHoverText.LinePosition.Character + 1

            let fsiAssemblyRx = System.Text.RegularExpressions.Regex @"^\s*Assembly:\s+FSI-ASSEMBLY\s*$"

            let lineContent = text.GetLineString(line - 1)
            let! value =
                async {
                    match res.TryGetSymbolUse (mkPos line col) lineContent with
                    | Some symbolUse ->
                        let fullName =
                            match symbolUse with
                            | FsAutoComplete.Patterns.SymbolUse.Val sym ->
                                match sym.DeclaringEntity with
                                | Some ent when ent.IsFSharpModule ->
                                    match ent.TryFullName with
                                    | Some _ -> Some sym.FullName
                                    | None -> None
                                | _ -> None
                            | _ -> None
                        match fullName with
                        | Some name ->
                            let expr = name
                            let expr = stdinRx.Replace(expr, "")
                            let expr = fsiModuleRx.Replace(expr, "")
                            try
                                return script.Value.Fsi.EvalExpression(expr) |> Some
                            with e ->
                                return None
                        | None -> return None
                    | None -> return None
                }

            log $"handleRequestHoverText / requestHoverText: %A{serialize2 requestHoverText} / parse: %A{parse} / check: %A{check} / res: %A{serialize2 res} / text: %A{text} / line: %A{line} / col: %A{col} / lineContent: %A{lineContent} / value: %A{value}"

            match res.TryGetToolTipEnhanced (mkPos line col) lineContent with
            | Result.Ok (Some (tip, signature, footer, typeDoc)) ->
                let results =
                    FsAutoComplete.TipFormatter.formatTipEnhanced
                        tip signature footer typeDoc
                        FsAutoComplete.TipFormatter.FormatCommentStyle.Legacy
                    |> Seq.concat
                    |> Seq.map (fun (signature, comment, footer) ->
                        // make footer look like in Ionide
                        let newFooter =
                            footer.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
                            |> Seq.map (fun line -> line.TrimEnd('\r'))
                            |> Seq.filter (fsiAssemblyRx.IsMatch >> not)
                            |> Seq.map (sprintf "*%s*")
                            |> String.concat "\n\n----\n"

                        let markdown =
                            String.concat "\n\n----\n" [
                                if not (String.IsNullOrWhiteSpace signature) then
                                    let code =
                                        match value with
                                        // don't show function-values
                                        | Some (Some value) when not (Reflection.FSharpType.IsFunction value.ReflectionType) ->
                                            let valueString = sprintf "%0A" value.ReflectionValue
                                            let lines = valueString.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList

                                            match lines with
                                            | [] ->
                                                signature
                                            | [line] ->
                                                sprintf "%s // %s" signature line
                                            | first :: rest ->
                                                String.concat "\n" [
                                                    let prefix = sprintf "%s " signature
                                                    yield sprintf "%s// %s" prefix first

                                                    let ws = String(' ', prefix.Length)

                                                    for line in rest do
                                                        yield sprintf "%s// %s" ws line
                                                ]
                                        | Some None ->
                                            sprintf "%s // null" signature
                                        | _ ->
                                            signature

                                    sprintf "```spiral\n%s\n```" code

                                if not (String.IsNullOrWhiteSpace comment) then
                                    comment

                                if not (String.IsNullOrWhiteSpace newFooter) then
                                    newFooter
                            ]

                        FormattedValue("text/markdown", stdinRx.Replace(fsiModuleRx.Replace(markdown, "").Replace("\r\n", "\n"), ""))
                    )
                    |> Seq.toArray

                let sp = LinePosition(requestHoverText.LinePosition.Line, col)
                let ep = LinePosition(requestHoverText.LinePosition.Line, col)
                let lps = LinePositionSpan(sp, ep)
                context.Publish(HoverTextProduced(requestHoverText, results, lps))
                log $"handleRequestHoverText / Publish(HoverTextProduced): %A{HoverTextProduced(requestHoverText, results, lps) |> serialize2}"

            | _ ->
                let sp = LinePosition(requestHoverText.LinePosition.Line, col)
                let ep = LinePosition(requestHoverText.LinePosition.Line, col)
                let lps = LinePositionSpan(sp, ep)
                let reply = [| FormattedValue("text/markdown", "") |]
                context.Publish(HoverTextProduced(requestHoverText, reply, lps))
                log $"handleRequestHoverText / Publish(HoverTextProduced) ERROR: %A{HoverTextProduced(requestHoverText, reply, lps) |> serialize2}"
                ()
        }

    let handleRequestDiagnostics (requestDiagnostics: RequestDiagnostics) (context: KernelInvocationContext) =
        // log $"handleRequestDiagnostics / requestDiagnostics: %A{serialize requestDiagnostics}"

        task {
            ()
            // let _parseResults, checkFileResults, _checkProjectResults = script.Value.Fsi.ParseAndCheckInteraction(requestDiagnostics.Code)
            // let errors = checkFileResults.Diagnostics

            // if errors.Length > 0 then
            //     let diagnostics = errors |> Array.map getDiagnostic |> fun x -> x.ToImmutableArray()
            //     context.Publish(DiagnosticsProduced(diagnostics, requestDiagnostics))
            //     log $"handleRequestDiagnostics / Publish(DiagnosticsProduced): %A{DiagnosticsProduced(diagnostics, requestDiagnostics) |> serialize}"
        }

    let handleRequestValueValueInfos (requestValueInfos: RequestValueInfos) (context: KernelInvocationContext) =
        task {
            ()
            // let valueInfos =
            //     script.Value.Fsi.GetBoundValues()
            //     |> List.filter (fun x -> x.Name <> "it") // don't report special variable `it`
            //     |> List.map (fun x -> new KernelValueInfo(x.Name, FormattedValue.CreateSingleFromObject(x.Value.ReflectionValue, requestValueInfos.MimeType), this.getValueType(x.Name)))
            //     :> IReadOnlyCollection<KernelValueInfo>
            // context.Publish(new ValueInfosProduced(valueInfos, requestValueInfos))
            // log $"handleRequestValueValueInfos / Publish(ValueInfosProduced): %A{ValueInfosProduced(valueInfos, requestValueInfos) |> serialize}"
        }

    let handleRequestValue (requestValue: RequestValue) (context: KernelInvocationContext) =
        log $"handleRequestValue / requestValue: %A{requestValue |> serialize2}"

        task {
            ()
            // match script.Value.Fsi.TryFindBoundValue(requestValue.Name) with
            // | Some cv ->
            //     context.PublishValueProduced(requestValue, cv.Value.ReflectionValue)
            //     log $"handleRequestValue / PublishValueProduced: %A{requestValue} / cv.Value.ReflectionValue: %A{cv.Value.ReflectionValue}"

            // | _ ->
            //     context.Fail(requestValue, message=(sprintf "Value '%s' not found in kernel %s" requestValue.Name this.Name))
            //     log $"handleRequestValue / Fail: %A{requestValue}"
        }

    let createPackageRestoreContext (useResultsCache:bool) (registerForDisposal) =
        log $"createPackageRestoreContext"

        let packageRestoreContext = new PackageRestoreContext(useResultsCache)
        do registerForDisposal(fun () -> packageRestoreContext.Dispose())
        packageRestoreContext

    let mutable _packageRestoreContext = lazy createPackageRestoreContext true this.RegisterForDisposal

    member this.GetValues() =
        log $"GetValues"

        []
        // script.Value.Fsi.GetBoundValues()
        // |> List.filter (fun x -> x.Name <> "it") // don't report special variable `it`
        // |> List.map (fun x -> KernelValue( new KernelValueInfo(x.Name, new FormattedValue(PlainTextFormatter.MimeType, x.Value.ToDisplayString(PlainTextFormatter.MimeType)) , x.Value.ReflectionType), x.Value.ReflectionValue, this.Name))

    member this.getValueType(name:string) =
        log $"getValueType / name: %A{name}"
        typeof<obj>
        // let result =
        //     match script.Value.Fsi.TryFindBoundValue(name) with
        //     | Some cv ->
        //         cv.Value.ReflectionValue.GetType()
        //     | _ ->
        //         null
        // log $"getValueType / name: %A{name} / result: %A{result}"
        // result

    member this.handleTryGetValue<'a>(name: string, [<Out>] value: 'a byref) =
        log $"handleTryGetValue / name: %A{name}"
        true

        // match script.Value.Fsi.TryFindBoundValue(name) with
        // | Some cv ->
        //     value <- cv.Value.ReflectionValue :?> 'a
        //     true
        // | _ ->
        //     false

    member _.RestoreSources with get () = _packageRestoreContext.Value.RestoreSources

    member _.RequestedPackageReferences with get () = _packageRestoreContext.Value.RequestedPackageReferences;

    member _.ResolvedPackageReferences with get () = _packageRestoreContext.Value.ResolvedPackageReferences;

    member _.PackageRestoreContext with get () = _packageRestoreContext.Value

    interface IKernelCommandHandler<RequestCompletions> with
        member this.HandleAsync(command: RequestCompletions, context: KernelInvocationContext) =
            // log $"IKernelCommandHandler<RequestCompletions>.HandleAsync / command: %A{command |> serialize}"
            handleRequestCompletions command context

    interface IKernelCommandHandler<RequestDiagnostics> with
        member this.HandleAsync(command: RequestDiagnostics, context: KernelInvocationContext) =
            // log $"IKernelCommandHandler<RequestDiagnostics>.HandleAsync / command: %A{command |> serialize}"
            handleRequestDiagnostics command context

    interface IKernelCommandHandler<RequestHoverText> with
        member this.HandleAsync(command: RequestHoverText, context: KernelInvocationContext) =
            // log $"IKernelCommandHandler<RequestHoverText>.HandleAsync / command: %A{command |> serialize2}"
            handleRequestHoverText command context

    interface IKernelCommandHandler<RequestValueInfos> with
        member this.HandleAsync(command: RequestValueInfos, context: KernelInvocationContext) =
            log $"IKernelCommandHandler<RequestValueInfos>.HandleAsync / command: %A{command |> serialize2}"
            handleRequestValueValueInfos command context

    interface IKernelCommandHandler<RequestValue> with
        member this.HandleAsync(command: RequestValue, context: KernelInvocationContext) =
            // log $"IKernelCommandHandler<RequestValue>.HandleAsync / command: %A{command |> serialize2}"
            handleRequestValue command context

    interface IKernelCommandHandler<SendValue> with
        member this.HandleAsync(command: SendValue, context: KernelInvocationContext) =
            log $"IKernelCommandHandler<SendValue>.HandleAsync / command: %A{command |> serialize2}"
            let handle (name : string) (value : obj) (declaredType : Type) : Task =
                script.Value.Fsi.AddBoundValue(name, value)
                Task.CompletedTask
            base.SetValueAsync(command, context, handle)

    interface IKernelCommandHandler<SubmitCode> with
        member this.HandleAsync(command: SubmitCode, context: KernelInvocationContext) =
            // log $"IKernelCommandHandler<SubmitCode>.HandleAsync / command: %A{command |> serialize}"
            handleSubmitCode command context

    interface IKernelCommandHandler<ChangeWorkingDirectory> with
        member this.HandleAsync(command: ChangeWorkingDirectory, context: KernelInvocationContext) =
            // log $"IKernelCommandHandler<ChangeWorkingDirectory>.HandleAsync / command: %A{command |> serialize}"
            handleChangeWorkingDirectory command context

    interface ISupportNuget with
        member _.TryAddRestoreSource(source: string) =
            log $"ISupportNuget.TryAddRestoreSource / source: %A{source}"
            this.PackageRestoreContext.TryAddRestoreSource source

        member _.GetOrAddPackageReference(packageName: string, packageVersion: string) =
            log $"ISupportNuget.GetOrAddPackageReference / packageName: %A{packageName}, packageVersion: %A{packageVersion}"
            this.PackageRestoreContext.GetOrAddPackageReference (packageName, packageVersion)

        member _.Configure(useResultsCache:bool) =
             _packageRestoreContext <- lazy createPackageRestoreContext useResultsCache this.RegisterForDisposal
        member _.RestoreAsync() =
            log $"ISupportNuget.RestoreAsync"
            this.PackageRestoreContext.RestoreAsync()

        member _.RestoreSources =
            this.PackageRestoreContext.RestoreSources

        member _.RequestedPackageReferences =
            this.PackageRestoreContext.RequestedPackageReferences

        member _.ResolvedPackageReferences =
            this.PackageRestoreContext.ResolvedPackageReferences

        member _.RegisterResolvedPackageReferences (packageReferences: IReadOnlyList<ResolvedPackageReference>) =
            log $"ISupportNuget.RegisterResolvedPackageReferences / packageReferences: %A{packageReferences |> serialize}"
            // Generate #r and #I from packageReferences
            let sb = StringBuilder()
            let hashset = HashSet()

            for reference in packageReferences do
                for assembly in reference.AssemblyPaths do
                    if hashset.Add(assembly) then
                        if File.Exists assembly then
                            sb.AppendFormat("#r @\"{0}\"", assembly) |> ignore
                            sb.Append(Environment.NewLine) |> ignore

                match reference.PackageRoot with
                | null -> ()
                | root ->
                    if hashset.Add(root) then
                        if File.Exists root then
                            sb.AppendFormat("#I @\"{0}\"", root) |> ignore
                            sb.Append(Environment.NewLine) |> ignore
            let command = new SubmitCode(sb.ToString(), "spiral")
            this.DeferCommand(command)
