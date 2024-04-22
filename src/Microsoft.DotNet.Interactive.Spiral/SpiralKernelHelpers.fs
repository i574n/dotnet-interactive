// Copyright (c) .NET Foundation and contributors. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Microsoft.DotNet.Interactive.Spiral.SpiralKernelHelpers

open Microsoft.DotNet.Interactive

open System
open System.IO

open Polyglot.Common

type internal IMarker = interface end

[<AutoOpen>]
module DisplayFunctions =
    /// Display the object using current display settings
    let display (value: obj) =
        trace Verbose (fun () -> $"display / value: %A{value}") _locals
        Kernel.display(value)

    /// Display the object as HTML using current display settings
    let HTML (value: string) =
        trace Verbose (fun () -> $"HTML / value: %A{value}") _locals

        Kernel.HTML(value)

    /// Specify CSS style specifications.  If displayed, the styles will apply to the current worksheet.
    let CSS (styles: string) =
        trace Verbose (fun () -> $"CSS / styles: %A{styles}") _locals

        Kernel.CSS styles

    /// Execute the content as Javascript
    let Javascript (content: string) =
        trace Verbose (fun () -> $"Javascript / content: %A{content}") _locals

        Kernel.Javascript content
