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

open Polyglot.Common
open Polyglot
open Lib

type SpiralKernel () as this =

    inherit Kernel("spiral")

    do this.KernelInfo.LanguageName <- "Spiral"
    do this.KernelInfo.LanguageVersion <- "2.3"
    do this.KernelInfo.DisplayName <- $"{this.KernelInfo.LocalName} - Spiral Script"

    do Lib.SpiralTrace.TraceLevel.US0_2 |> Lib.set_trace_level

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

    let serialize1 obj =
        try
            obj |> FSharp.Json.Json.serializeEx (FSharp.Json.JsonConfig.create false)
        with ex ->
            trace Critical (fun () -> $"SpiralKernel.serialize1 / ex: {ex |> Sm.format_exception}") _locals
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
            trace Critical (fun () -> $"SpiralKernel.serialize2 1 / ex: {ex |> Sm.format_exception}") _locals
            try
                Json.JsonSerializer.Serialize (
                        obj,
                        Json.JsonSerializerOptions (
                            ReferenceHandler = Json.Serialization.ReferenceHandler.Preserve,
                            WriteIndented = true
                        )
                )
            with ex ->
                trace Critical (fun () -> $"SpiralKernel.serialize2 2 / ex: {ex |> Sm.format_exception}") _locals
                "Serialize error"

    let serialize obj =
        let result = serialize1 obj
        if result = "Serialize error"
        then serialize2 obj
        else $"%A{obj}"

    let getKindString (glyph: FSharpGlyph) =
        trace Verbose (fun () -> $"getKindString / glyph: %A{glyph}") _locals

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
        trace Verbose (fun () -> $"getFilterText / declarationItem: %A{declarationItem}") _locals

        match declarationItem.NamespaceToOpen, declarationItem.NameInList.Split '.' with
        // There is no namespace to open and the item name does not contain dots, so we don't need to pass special FilterText to Roslyn.
        | None, [|_|] -> null
        // Either we have a namespace to open ("DateTime (open System)") or item name contains dots ("Array.map"), or both.
        // We are passing last part of long ident as FilterText.
        | _, idents -> Array.last idents

    let tryGetXmlDocument xmlFile =
        trace Verbose (fun () -> $"tryGetXmlDocument / xmlFile: %A{xmlFile}") _locals

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
        trace Verbose (fun () -> $"tryGetDocumentationByXmlFileAndKey / xmlFile: %A{xmlFile}, key: %A{key}") _locals

        tryGetXmlDocument xmlFile
        |> Option.bind (fun doc ->
            match doc.SelectSingleNode(sprintf "doc/members/member[@name='%s']" key) with
            | null -> None
            | node ->
                match node.SelectSingleNode("summary") with
                | null -> None
                | summaryNode -> Some summaryNode.InnerText)

    let tryGetDocumentationByToolTipElementData (dataList: ToolTipElementData list) =
        trace Verbose (fun () -> $"tryGetDocumentationByToolTipElementData / dataList: %A{dataList}") _locals

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
        trace Verbose (fun () -> $"getDocumentation / declarationItem: %A{declarationItem}") _locals

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
        trace Verbose (fun () -> $"getCompletionItem / declarationItem: %A{declarationItem}") _locals

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
        trace Verbose (fun () -> $"getDiagnostic / error: %A{error}") _locals

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

    // let handleChangeWorkingDirectory (changeDirectory: ChangeWorkingDirectory) (context: KernelInvocationContext) =
    //     trace Verbose (fun () -> $"handleChangeWorkingDirectory / changeDirectory: %A{changeDirectory |> serialize2}") _locals

    //     task {
    //         this.workingDirectory <- changeDirectory.WorkingDirectory;
    //         return Task.CompletedTask;
    //     }

    let handleSubmitCode (codeSubmission: SubmitCode) (context: KernelInvocationContext) =
        trace Verbose (fun () -> $"SpiralKernel.handleSubmitCode / codeSubmission: %A{codeSubmission |> serialize2}") _locals

        async {
            let codeSubmissionReceived = CodeSubmissionReceived(codeSubmission)
            context.Publish(codeSubmissionReceived)
            trace Verbose (fun () -> $"SpiralKernel.handleSubmitCode / Publish(CodeSubmissionReceived): %A{codeSubmissionReceived |> serialize2}") _locals

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

            let _text = $"SpiralKernel.handleSubmitCode / codeSubmission.Parameters: %A{codeSubmission.Parameters} / codeSubmission.DestinationUri: %A{codeSubmission.DestinationUri} / fsiDiagnostics:\n{fsiDiagnostics |> Array.map (fun x -> x.ToString ()) |> serialize}"
            trace Verbose (fun () -> _text) _locals

            if fsiDiagnostics.Length > 0 then
                let diagnostics = fsiDiagnostics |> Array.map getDiagnostic |> _.ToImmutableArray()

                let formattedDiagnostics =
                    fsiDiagnostics
                    |> Array.map _.ToString()
                    |> Array.map (fun text -> new FormattedValue(PlainTextFormatter.MimeType, text))

                context.Publish(DiagnosticsProduced(diagnostics, formattedDiagnostics, codeSubmission))
                trace Verbose (fun () -> $"SpiralKernel.handleSubmitCode / Publish(DiagnosticsProduced): %A{DiagnosticsProduced(diagnostics, formattedDiagnostics, codeSubmission) |> serialize2}") _locals

            match result with
            | Ok(result) when not isError ->
                match result with
                | Some(value) when value.ReflectionType <> typeof<unit> ->
                    let resultValue = value.ReflectionValue
                    let formattedValues : IReadOnlyList<FormattedValue> =
                        match resultValue with
                        | :? FormattedValue as formattedValue -> Seq.singleton( formattedValue ).ToImmutableList()
                        | :? IEnumerable<FormattedValue> as formattedValueEnumerable -> formattedValueEnumerable.ToImmutableList()
                        | _ -> FormattedValue.CreateManyFromObject(resultValue)
                    context.Publish(ReturnValueProduced(resultValue, codeSubmission, formattedValues))
                    trace Verbose (fun () -> $"SpiralKernel.handleSubmitCode / Publish(ReturnValueProduced): %A{ReturnValueProduced(resultValue, codeSubmission, formattedValues) |> serialize2}") _locals

                | Some _
                | None -> ()
            | _ ->
                if not (tokenSource.IsCancellationRequested) then
                    let aggregateError = String.Join("\n", fsiDiagnostics)
                    match result with
                    | Error (:? FsiCompilationException)
                    | Ok _ ->
                        let ex = CodeSubmissionCompilationErrorException(Exception($"SpiralKernel.handleSubmitCode / aggregateError: {aggregateError} / result: {result}"))
                        context.Fail(codeSubmission, ex, aggregateError)
                        trace Critical (fun () -> $"SpiralKernel.handleSubmitCode / Fail / codeSubmission: %A{codeSubmission} / ex: %A{ex} / aggregateError: {aggregateError}") _locals
                    | Error ex ->
                        let ex = Exception($"SpiralKernel.handleSubmitCode / aggregateError: {aggregateError} / ex: %A{ex}")
                        context.Fail(codeSubmission, ex, null)
                        trace Critical (fun () -> $"SpiralKernel.handleSubmitCode / Fail / codeSubmission: %A{codeSubmission} / ex: %A{ex}") _locals
                else
                    context.Fail(codeSubmission, null, "Command cancelled")
                    trace Critical (fun () -> $"SpiralKernel.handleSubmitCode / Fail / codeSubmission: %A{codeSubmission} / Command cancelled") _locals
        }
        |> Async.StartAsTask


    let handleRequestCompletions (requestCompletions: RequestCompletions) (context: KernelInvocationContext) =
        // trace Verbose (fun () -> $"handleRequestCompletions / requestCompletions: %A{requestCompletions |> serialize}") _locals

        task {
            ()
            // let! declarationItems = script.Value.GetCompletionItems(requestCompletions.Code, requestCompletions.LinePosition.Line + 1, requestCompletions.LinePosition.Character)
            // let! completionItems =
            //     declarationItems
            //     |> Array.map getCompletionItem
            //     |> Task.WhenAll
            // context.Publish(CompletionsProduced(completionItems, requestCompletions))
            // trace Verbose (fun () -> $"handleRequestCompletions / Publish(CompletionsProduced): %A{CompletionsProduced(completionItems, requestCompletions)}") _locals
        }


    let mutable allCodeHover = ""

    let handleRequestHoverText (requestHoverText: RequestHoverText) (context: KernelInvocationContext) = task {
        trace Verbose (fun () -> $"SpiralKernel.handleRequestHoverText / requestHoverText: %A{requestHoverText |> serialize2}") _locals

        let rawCellCode =
            requestHoverText.Code |> SpiralSm.replace "\r\n" "\n"

        let lines = rawCellCode |> SpiralSm.split "\n"

        let cellCode, lastTopLevelIndex = Eval.prepareSpiral rawCellCode lines

        let character, line =
            match lastTopLevelIndex with
            | None -> requestHoverText.LinePosition.Character, requestHoverText.LinePosition.Line
            | _ -> requestHoverText.LinePosition.Character + 4, requestHoverText.LinePosition.Line + 2

        let newAllCode = $"{allCodeHover}\n\n{cellCode}"
        let newLine = line + allCodeHover.Split('\n').Length - 1 + 2

        let! hover = Supervisor.getCodeHoverAt None newAllCode {| character = character; line = newLine |}
        let hover = $"""```{'\n'}{hover |> Option.defaultValue "???"}{'\n'}```"""

        allCodeHover <- newAllCode

        let sp = LinePosition (line, character)
        let ep = LinePosition (line, character)
        let lps = LinePositionSpan (sp, ep)
        let reply = [| FormattedValue ("text/markdown", hover) |]
        context.Publish (HoverTextProduced (requestHoverText, reply, lps))
        trace Verbose (fun () -> $"handleRequestHoverText / Publish(HoverTextProduced): %A{HoverTextProduced(requestHoverText, reply, lps) |> serialize2} / lastTopLevelIndex: {lastTopLevelIndex}") _locals
    }

    let handleRequestDiagnostics (requestDiagnostics: RequestDiagnostics) (context: KernelInvocationContext) =
        // trace Verbose (fun () -> $"handleRequestDiagnostics / requestDiagnostics: %A{serialize requestDiagnostics}") _locals

        task {
            ()
            // let _parseResults, checkFileResults, _checkProjectResults = script.Value.Fsi.ParseAndCheckInteraction(requestDiagnostics.Code)
            // let errors = checkFileResults.Diagnostics

            // if errors.Length > 0 then
            //     let diagnostics = errors |> Array.map getDiagnostic |> fun x -> x.ToImmutableArray()
            //     context.Publish(DiagnosticsProduced(diagnostics, requestDiagnostics))
            //     trace Verbose (fun () -> $"handleRequestDiagnostics / Publish(DiagnosticsProduced): %A{DiagnosticsProduced(diagnostics, requestDiagnostics) |> serialize}") _locals
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
            // trace Verbose (fun () -> $"handleRequestValueValueInfos / Publish(ValueInfosProduced): %A{ValueInfosProduced(valueInfos, requestValueInfos) |> serialize}") _locals
        }

    let handleRequestValue (requestValue: RequestValue) (context: KernelInvocationContext) =
        trace Verbose (fun () -> $"handleRequestValue / requestValue: %A{requestValue |> serialize2}") _locals

        task {
            ()
            // match script.Value.Fsi.TryFindBoundValue(requestValue.Name) with
            // | Some cv ->
            //     context.PublishValueProduced(requestValue, cv.Value.ReflectionValue)
            //     trace Verbose (fun () -> $"handleRequestValue / PublishValueProduced: %A{requestValue} / cv.Value.ReflectionValue: %A{cv.Value.ReflectionValue}") _locals

            // | _ ->
            //     context.Fail(requestValue, message=(sprintf "Value '%s' not found in kernel %s" requestValue.Name this.Name))
            //     trace Verbose (fun () -> $"handleRequestValue / Fail: %A{requestValue}") _locals
        }

    // let createPackageRestoreContext (useResultsCache:bool) (registerForDisposal) =
    //     trace Verbose (fun () -> $"createPackageRestoreContext") _locals

    //     let packageRestoreContext = new PackageRestoreContext(useResultsCache)
    //     do registerForDisposal(fun () -> packageRestoreContext.Dispose())
    //     packageRestoreContext

    // let mutable _packageRestoreContext = lazy createPackageRestoreContext true this.RegisterForDisposal

    member this.GetValues() =
        trace Verbose (fun () -> $"GetValues") _locals

        []
        // script.Value.Fsi.GetBoundValues()
        // |> List.filter (fun x -> x.Name <> "it") // don't report special variable `it`
        // |> List.map (fun x -> KernelValue( new KernelValueInfo(x.Name, new FormattedValue(PlainTextFormatter.MimeType, x.Value.ToDisplayString(PlainTextFormatter.MimeType)) , x.Value.ReflectionType), x.Value.ReflectionValue, this.Name))

    member this.getValueType(name:string) =
        trace Verbose (fun () -> $"getValueType / name: %A{name}") _locals
        typeof<obj>
        // let result =
        //     match script.Value.Fsi.TryFindBoundValue(name) with
        //     | Some cv ->
        //         cv.Value.ReflectionValue.GetType()
        //     | _ ->
        //         null
        // trace Verbose (fun () -> $"getValueType / name: %A{name} / result: %A{result}") _locals
        // result

    member this.handleTryGetValue<'a>(name: string, [<Out>] value: 'a byref) =
        trace Verbose (fun () -> $"handleTryGetValue / name: %A{name}") _locals
        true

        // match script.Value.Fsi.TryFindBoundValue(name) with
        // | Some cv ->
        //     value <- cv.Value.ReflectionValue :?> 'a
        //     true
        // | _ ->
        //     false

    member this.AddAssemblyReferencesAndPackageRoots(assemblyReferences: IEnumerable<string>, packageRoots: IEnumerable<string>) =
        let sb = StringBuilder()
        let hashset = HashSet()

        for packageRoot in packageRoots do
            match packageRoot with
            | null -> ()
            | root ->
                if hashset.Add(root) then
                    if File.Exists root then
                        sb.AppendFormat("#I @\"{0}\"", root) |> ignore
                        sb.Append(Environment.NewLine) |> ignore

        for assemblyReference in assemblyReferences do
            if hashset.Add(assemblyReference) then
                if File.Exists assemblyReference then
                    sb.AppendFormat("#r @\"{0}\"", assemblyReference) |> ignore
                    sb.Append(Environment.NewLine) |> ignore

        let command = new SubmitCode(sb.ToString(), this.Name)

        this.DeferCommand(command)

    // member _.RestoreSources with get () = _packageRestoreContext.Value.RestoreSources

    // member _.RequestedPackageReferences with get () = _packageRestoreContext.Value.RequestedPackageReferences;

    // member _.ResolvedPackageReferences with get () = _packageRestoreContext.Value.ResolvedPackageReferences;

    // member _.PackageRestoreContext with get () = _packageRestoreContext.Value

    interface IKernelCommandHandler<RequestCompletions> with
        member this.HandleAsync(command: RequestCompletions, context: KernelInvocationContext) =
            // trace Verbose (fun () -> $"IKernelCommandHandler<RequestCompletions>.HandleAsync / command: %A{command |> serialize}") _locals
            handleRequestCompletions command context

    interface IKernelCommandHandler<RequestDiagnostics> with
        member this.HandleAsync(command: RequestDiagnostics, context: KernelInvocationContext) =
            // trace Verbose (fun () -> $"IKernelCommandHandler<RequestDiagnostics>.HandleAsync / command: %A{command |> serialize}") _locals
            handleRequestDiagnostics command context

    interface IKernelCommandHandler<RequestHoverText> with
        member this.HandleAsync(command: RequestHoverText, context: KernelInvocationContext) =
            handleRequestHoverText command context

    interface IKernelCommandHandler<RequestValueInfos> with
        member this.HandleAsync(command: RequestValueInfos, context: KernelInvocationContext) =
            // trace Verbose (fun () -> $"IKernelCommandHandler<RequestValueInfos>.HandleAsync / command: %A{command |> serialize2}") _locals
            handleRequestValueValueInfos command context

    interface IKernelCommandHandler<RequestValue> with
        member this.HandleAsync(command: RequestValue, context: KernelInvocationContext) =
            // trace Verbose (fun () -> $"IKernelCommandHandler<RequestValue>.HandleAsync / command: %A{command |> serialize2}") _locals
            handleRequestValue command context

    interface IKernelCommandHandler<SendValue> with
        member this.HandleAsync(command: SendValue, context: KernelInvocationContext) =
            trace Verbose (fun () -> $"IKernelCommandHandler<SendValue>.HandleAsync / command: %A{command |> serialize2}") _locals
            let handle (name : string) (value : obj) (declaredType : Type) : Task =
                script.Value.Fsi.AddBoundValue(name, value)
                Task.CompletedTask
            base.SetValueAsync(command, context, handle)

    interface IKernelCommandHandler<SubmitCode> with
        member this.HandleAsync(command: SubmitCode, context: KernelInvocationContext) =
            // trace Verbose (fun () -> $"IKernelCommandHandler<SubmitCode>.HandleAsync / command: %A{command |> serialize}") _locals
            handleSubmitCode command context

    // interface IKernelCommandHandler<ChangeWorkingDirectory> with
    //     member this.HandleAsync(command: ChangeWorkingDirectory, context: KernelInvocationContext) =
    //         // trace Verbose (fun () -> $"IKernelCommandHandler<ChangeWorkingDirectory>.HandleAsync / command: %A{command |> serialize}") _locals
    //         handleChangeWorkingDirectory command context

    // interface ISupportNuget with
    //     member _.TryAddRestoreSource(source: string) =
    //         trace Verbose (fun () -> $"ISupportNuget.TryAddRestoreSource / source: %A{source}") _locals
    //         this.PackageRestoreContext.TryAddRestoreSource source

    //     member _.GetOrAddPackageReference(packageName: string, packageVersion: string) =
    //         trace Verbose (fun () -> $"ISupportNuget.GetOrAddPackageReference / packageName: %A{packageName}, packageVersion: %A{packageVersion}") _locals
    //         this.PackageRestoreContext.GetOrAddPackageReference (packageName, packageVersion)

    //     member _.Configure(useResultsCache:bool) =
    //          _packageRestoreContext <- lazy createPackageRestoreContext useResultsCache this.RegisterForDisposal
    //     member _.RestoreAsync() =
    //         trace Verbose (fun () -> $"ISupportNuget.RestoreAsync") _locals
    //         this.PackageRestoreContext.RestoreAsync()

    //     member _.RestoreSources =
    //         this.PackageRestoreContext.RestoreSources

    //     member _.RequestedPackageReferences =
    //         this.PackageRestoreContext.RequestedPackageReferences

    //     member _.ResolvedPackageReferences =
    //         this.PackageRestoreContext.ResolvedPackageReferences

    //     member _.RegisterResolvedPackageReferences (packageReferences: IReadOnlyList<ResolvedPackageReference>) =
    //         trace Verbose (fun () -> $"ISupportNuget.RegisterResolvedPackageReferences / packageReferences: %A{packageReferences |> serialize}") _locals
    //         // Generate #r and #I from packageReferences
    //         let sb = StringBuilder()
    //         let hashset = HashSet()

    //         for reference in packageReferences do
    //             for assembly in reference.AssemblyPaths do
    //                 if hashset.Add(assembly) then
    //                     if File.Exists assembly then
    //                         sb.AppendFormat("#r @\"{0}\"", assembly) |> ignore
    //                         sb.Append(Environment.NewLine) |> ignore

    //             match reference.PackageRoot with
    //             | null -> ()
    //             | root ->
    //                 if hashset.Add(root) then
    //                     if File.Exists root then
    //                         sb.AppendFormat("#I @\"{0}\"", root) |> ignore
    //                         sb.Append(Environment.NewLine) |> ignore
    //         let command = new SubmitCode(sb.ToString(), "spiral")
    //         this.DeferCommand(command)
