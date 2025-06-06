// Copyright (c) .NET Foundation and contributors. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Linq;
using System.Text.Encodings.Web;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading.Tasks;
using Microsoft.DotNet.Interactive.Documents.Json;
using Microsoft.DotNet.Interactive.Documents.Jupyter;
using Microsoft.DotNet.Interactive.Documents.Utility;
using Microsoft.DotNet.Interactive.Utility;

namespace Microsoft.DotNet.Interactive.Documents;

public class InteractiveDocument : IEnumerable
{
    static InteractiveDocument()
    {
        JsonSerializerOptions = new JsonSerializerOptions
        {
            WriteIndented = false,
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            NumberHandling = JsonNumberHandling.AllowReadingFromString | JsonNumberHandling.AllowNamedFloatingPointLiterals,
            Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping,
            Converters =
            {
                new ByteArrayConverter(),
                new DataDictionaryConverter(),
                new JsonStringEnumConverter(JsonNamingPolicy.CamelCase),
                new InteractiveDocumentConverter()
            }
        };
    }

    public InteractiveDocument(IList<InteractiveDocumentElement>? elements = null)
    {
        Elements = elements ?? new List<InteractiveDocumentElement>();
    }

    public IList<InteractiveDocumentElement> Elements { get; }

    [field: AllowNull, MaybeNull]
    public IDictionary<string, object> Metadata =>
        field ??= new Dictionary<string, object>();

    public async IAsyncEnumerable<InteractiveDocument> GetImportsAsync(
        Func<string, DirectiveParseResult> parseDirective,
        bool recursive = false)
    {
        if (!TryGetKernelInfosFromMetadata(Metadata, out var kernelInfos))
        {
            kernelInfos = new();
        }

        foreach (var line in GetMagicCommandLines())
        {
            var parseResult = parseDirective(line);

            if (parseResult.CommandName is "#!import")
            {
                if (!parseResult.Errors.Any())
                {
                    var file = new FileInfo(parseResult.Parameters["--file"]);

                    var interactiveDocument = await LoadAsync(file, kernelInfos);

                    yield return interactiveDocument;

                    if (recursive)
                    {
                        await foreach (var import in interactiveDocument.GetImportsAsync(
                                           parseDirective,
                                           recursive))
                        {
                            yield return import;
                        }
                    }
                }
            }
        }
    }

    public IEnumerable<InputField> GetInputFields(Func<string, DirectiveParseResult> parseDirectiveLine)
    {
        var inputFields = new List<InputField>();

        foreach (var line in GetMagicCommandLines())
        {
            var parseResult = parseDirectiveLine(line);

            foreach (var field in parseResult.InputFields)
            {
                inputFields.Add(field);
            }
        }

        return inputFields.Distinct().ToArray();
    }

    IEnumerator IEnumerable.GetEnumerator() => Elements.GetEnumerator();

    public void Add(InteractiveDocumentElement element) => Elements.Add(element);

    internal void NormalizeElementKernelNames(KernelInfoCollection kernelInfos)
    {
        var defaultKernelName = GetDefaultKernelName(kernelInfos);

        foreach (var element in Elements)
        {
            if (element.InferredTargetKernelName is not null &&
                kernelInfos.TryGetByAlias(element.InferredTargetKernelName, out var byMagic))
            {
                element.KernelName = byMagic.Name;
            }

            if (element.KernelName is null)
            {
                element.KernelName = defaultKernelName;
            }

            if (element.KernelName is not null &&
                kernelInfos.TryGetByAlias(element.KernelName, out var n))
            {
                element.KernelName = n.Name;
            }
        }
    }

    public static async Task<InteractiveDocument> LoadAsync(
        FileInfo file,
        KernelInfoCollection? kernelInfos = null)
    {
        var fileContents = await IOExtensions.ReadAllTextAsync(file.FullName);

        return file.Extension.ToLowerInvariant() switch
        {
            // polyglot formats
            ".ipynb" => Notebook.Parse(fileContents, kernelInfos),
            ".dib" => CodeSubmission.Parse(fileContents, kernelInfos),

            // single-language formats
            ".cs" => new InteractiveDocument { new InteractiveDocumentElement(fileContents, "csharp") },
            ".csx" => new InteractiveDocument { new InteractiveDocumentElement(fileContents, "csharp") },
            ".fs" => new InteractiveDocument { new InteractiveDocumentElement(fileContents, "fsharp") },
            ".spi" => new InteractiveDocument { new InteractiveDocumentElement(fileContents, "spiral") },
            ".spir" => new InteractiveDocument { new InteractiveDocumentElement(fileContents, "spiral") },
            ".fsx" => new InteractiveDocument { new InteractiveDocumentElement(fileContents, "fsharp") },
            ".ps1" => new InteractiveDocument { new InteractiveDocumentElement(fileContents, "pwsh") },
            ".html" => new InteractiveDocument { new InteractiveDocumentElement(fileContents, "html") },
            ".http" => new InteractiveDocument { new InteractiveDocumentElement(fileContents, "http") },
            ".js" => new InteractiveDocument { new InteractiveDocumentElement(fileContents, "javascript") },

            _ => throw new InvalidOperationException($"Unrecognized extension for a notebook: {file.Extension}")
        };
    }

    public string? GetDefaultKernelName()
    {
        if (TryGetKernelInfosFromMetadata(Metadata, out var kernelInfos))
        {
            return kernelInfos.DefaultKernelName;
        }

        return null;
    }

    internal string? GetDefaultKernelName(KernelInfoCollection kernelInfos)
    {
        if (TryGetKernelInfosFromMetadata(Metadata, out var kernelInfoCollection))
        {
            return kernelInfoCollection.DefaultKernelName;
        }
        
        return kernelInfos.DefaultKernelName;
    }

    internal static void MergeKernelInfos(
        InteractiveDocument document, 
        KernelInfoCollection commonKernelInfos)
    {
        KernelInfoCollection? kernelInfos = null;
        if (TryGetKernelInfosFromMetadata(document.Metadata, out var documentMetadataKernelInfos))
        {
            MergeKernelInfos(documentMetadataKernelInfos, commonKernelInfos);
            kernelInfos = documentMetadataKernelInfos;
        }
        else
        {
            kernelInfos = commonKernelInfos;
        }
        var items = kernelInfos.Where(x => x.Name == "spiral").ToList();
        if (items.Count == 0) {
            items = new [] { new KernelInfo("spiral") }.ToList();
        }
        kernelInfos.Clear();
        kernelInfos.AddRange(items);
        document.Metadata["kernelInfo"] = kernelInfos;
    }

    internal static void MergeKernelInfos(
        KernelInfoCollection destination, 
        KernelInfoCollection source)
    {
        var added = new HashSet<string>();
        foreach (var kernelInfo in destination)
        {
            added.Add(kernelInfo.Name);
        }

        destination.AddRange(source.Where(ki => added.Add(ki.Name)));
    }

    public KernelInfoCollection? GetKernelInfo()
    {
        if (TryGetKernelInfosFromMetadata(Metadata, out var kernelInfos))
        {
            return kernelInfos;
        }

        return null;
    }

    internal static bool TryGetKernelInfosFromMetadata(
        IDictionary<string, object>? metadata,
        [NotNullWhen(true)] out KernelInfoCollection? kernelInfos)
    {
        if (metadata is not null)
        {
            if (metadata.TryGetValue("kernelInfo", out var kernelInfoObj))
            {
                if (kernelInfoObj is IDictionary<string, object>)
                {
                    var json = JsonSerializer.Serialize(kernelInfoObj, JsonSerializerOptions);
                    kernelInfos = JsonSerializer.Deserialize<KernelInfoCollection>(json, JsonSerializerOptions)!;
                    return true;
                }

                if (kernelInfoObj is JsonElement kernelInfoJson &&
                    kernelInfoJson.Deserialize<KernelInfoCollection>(JsonSerializerOptions) is
                        { } kernelInfoDeserialized)
                {
                    kernelInfos = kernelInfoDeserialized;
                    return true;
                }

                if (kernelInfoObj is KernelInfoCollection kernelInfoCollection)
                {
                    kernelInfos = kernelInfoCollection;
                    return true;
                }
            }

            if (metadata.TryGetValue("polyglot_notebook", out var polyglotNotebookObj))
            {
                switch (polyglotNotebookObj)
                {
                    case KernelInfoCollection kernelInfoCollection:
                        kernelInfos = kernelInfoCollection;
                        return true;

                    case IDictionary<string, object> polyglotNotebookDictionary:
                    {
                        kernelInfos = new();

                        if (polyglotNotebookDictionary.TryGetValue("defaultKernelName", out var nameObj) &&
                            nameObj is string name)
                        {
                            kernelInfos.DefaultKernelName = name;
                        }

                        return true;
                    }
                }
            }

            if (metadata.TryGetValue("dotnet_interactive", out var dotnetInteractiveObj))
            {
                // This is for backwards compatibility. Some older notebooks contain this metadata.
                switch (dotnetInteractiveObj)
                {
                    case KernelInfoCollection kernelInfoCollection:

                        kernelInfos = kernelInfoCollection;
                        return true;

                    case IDictionary<string, object> dotnetInteractiveDict:
                    {
                        kernelInfos = new();

                        if (dotnetInteractiveDict.TryGetValue("defaultKernelName", out var nameObj) &&
                            nameObj is string name)
                        {
                            kernelInfos.DefaultKernelName = name;
                        }

                        return true;
                    }
                }
            }

            // check for .ipynb / Jupyter metadata
            if (metadata.TryGetValue("kernelspec", out var kernelspecObj))
            {
                if (kernelspecObj is IDictionary<string, object> kernelspecDict)
                {
                    if (kernelspecDict.TryGetValue("language", out var languageObj) &&
                        languageObj is string defaultLanguage)
                    {
                        kernelInfos = new KernelInfoCollection
                        {
                            DefaultKernelName = defaultLanguage
                        };
                        return true;
                    }
                }
            }
        }

        // check if a KernelInfoCollection was directly serialized into the metadata
        kernelInfos = default;
        return false;
    }

    private IEnumerable<string> GetMagicCommandLines() =>
        Elements.SelectMany(e => e.Contents.SplitIntoLines())
                .Where(line => line.StartsWith("#!"));

    internal static JsonSerializerOptions JsonSerializerOptions { get; }
}