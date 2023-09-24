// Copyright (c) .NET Foundation and contributors. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Microsoft.DotNet.Interactive.Spiral.SpiralKernelHelpers

open Microsoft.DotNet.Interactive

open System
open System.IO

type internal IMarker = interface end

[<AutoOpen>]
module DisplayFunctions =

    let log (text : string) =
        if Polyglot.Common.traceLevel = Polyglot.Common.TraceLevel.Verbose then
            try
                let tmpPath = Path.GetTempPath ()
                let logDir = Path.Combine (tmpPath, "_log_spiral_kernel")
                Directory.CreateDirectory logDir |> ignore
                let logFile = Path.Combine (logDir, "log.txt")
                let dateTimeStr = DateTime.Now.ToString "yyyy-MM-dd HH:mm:ss.fff"
                let fileName = "SpiralKernelHelpers"
                File.AppendAllText (logFile, $"{dateTimeStr} {fileName} {text}{Environment.NewLine}") |> ignore
            with ex ->
                Polyglot.Common.trace Polyglot.Common.Debug (fun () -> $"SpiralKernelHelpers.log / ex: {ex |> Polyglot.Common.printException}") Polyglot.Common.getLocals

    /// Display the object using current display settings
    let display (value: obj) =
        log $"display / value: %A{value}"
        Kernel.display(value)

    /// Display the object as HTML using current display settings
    let HTML (value: string) =
        log $"HTML / value: %A{value}"

        Kernel.HTML(value)

    /// Specify CSS style specifications.  If displayed, the styles will apply to the current worksheet.
    let CSS (styles: string) =
        log $"CSS / styles: %A{styles}"

        Kernel.CSS styles

    /// Execute the content as Javascript
    let Javascript (content: string) =
        log $"Javascript / content: %A{content}"

        Kernel.Javascript content
