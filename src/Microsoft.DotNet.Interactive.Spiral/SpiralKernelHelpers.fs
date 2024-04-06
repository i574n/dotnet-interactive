// Copyright (c) .NET Foundation and contributors. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Microsoft.DotNet.Interactive.Spiral.SpiralKernelHelpers

open Microsoft.DotNet.Interactive

open System
open System.IO

type internal IMarker = interface end

[<AutoOpen>]
module DisplayFunctions =
    let log = Polyglot.Eval.log

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
