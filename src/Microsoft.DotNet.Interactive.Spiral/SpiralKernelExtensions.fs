// Copyright (c) .NET Foundation and contributors. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Microsoft.DotNet.Interactive.Spiral

open System
open System.Runtime.CompilerServices
open Microsoft.AspNetCore.Html
open Microsoft.DotNet.Interactive
open Microsoft.DotNet.Interactive.Commands
open Microsoft.DotNet.Interactive.Spiral
open Microsoft.DotNet.Interactive.Formatting

open System.IO


[<AbstractClass; Extension; Sealed>]
type SpiralKernelExtensions private () =

    static let log (text : string) =
        if Lib.get_trace_level () = Lib.SpiralTrace.TraceLevel.US0_0 then
            try
                Polyglot.Eval.log text
            with ex ->
                Polyglot.Common.trace Polyglot.Common.Debug (fun () -> $"SpiralKernelExtensions.log / ex: {ex |> Sm.format_exception}") Polyglot.Common.getLocals

    static let referenceAssemblyContaining (typ: Type) =
        log $"referenceAssemblyContaining / typ: %A{typ}"
        sprintf "#r \"%s\"" (typ.Assembly.Location.Replace("\\", "/"))
    static let openNamespaceContaining (typ: Type) =
        log $"openNamespaceContaining / typ: %A{typ}"
        sprintf "open %s" typ.Namespace
    static let openType (typ: Type) =
        log $"openType / typ: %A{typ}"
        sprintf "open type %s.%s" typ.Namespace typ.Name

    [<Extension>]
    static member UseDefaultFormatting(kernel: SpiralKernel) =
        log $"UseDefaultFormatting"
        let code =
            [
                referenceAssemblyContaining typeof<IHtmlContent>
                referenceAssemblyContaining typeof<Kernel>
                referenceAssemblyContaining typeof<SpiralKernelHelpers.IMarker>
                referenceAssemblyContaining typeof<Formatter>

                openNamespaceContaining typeof<System.Console>
                openNamespaceContaining typeof<System.IO.File>
                openNamespaceContaining typeof<System.Text.StringBuilder>
                openNamespaceContaining typeof<Formatter>
            ] |> String.concat Environment.NewLine

        kernel.DeferCommand(SubmitCode code)
        kernel

    [<Extension>]
    static member UseKernelHelpers(kernel: SpiralKernel) =
        log $"UseKernelHelpers"
        let code =
            [
                referenceAssemblyContaining typeof<SpiralKernelHelpers.IMarker>

                // opens Microsoft.DotNet.Interactive.Spiral.SpiralKernelHelpers
                //    note this has some AutoOpen content inside
                openNamespaceContaining typeof<SpiralKernelHelpers.IMarker>

                referenceAssemblyContaining typeof<Kernel>
                openType typeof<Kernel>

            ] |> String.concat Environment.NewLine

        kernel.DeferCommand(SubmitCode code)
        kernel
