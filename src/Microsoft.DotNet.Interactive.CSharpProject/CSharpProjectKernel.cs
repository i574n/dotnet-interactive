// Copyright (c) .NET Foundation and contributors. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.DotNet.Interactive.Commands;
using Microsoft.DotNet.Interactive.Connection;
using Microsoft.DotNet.Interactive.CSharpProject.Commands;
using Microsoft.DotNet.Interactive.CSharpProject.Events;
using Microsoft.DotNet.Interactive.CSharpProject.Build;
using Microsoft.DotNet.Interactive.CSharpProject.Servers.Roslyn;
using Microsoft.DotNet.Interactive.Events;
using Microsoft.DotNet.Interactive.Formatting;

namespace Microsoft.DotNet.Interactive.CSharpProject;

public class CSharpProjectKernel :
    Kernel,
    IKernelCommandHandler<OpenProject>,
    IKernelCommandHandler<OpenDocument>,
    IKernelCommandHandler<CompileProject>,
    IKernelCommandHandler<RequestCompletions>,
    IKernelCommandHandler<RequestDiagnostics>,
    IKernelCommandHandler<RequestSignatureHelp>,
    IKernelCommandHandler<SubmitCode>
{
    static CSharpProjectKernel()
    {
        // register commands and event with serialization
        var commandTypes = typeof(CSharpProjectKernel)
                           .Assembly.ExportedTypes
                           .Where(t => t is { IsAbstract: false, IsInterface: false })
                           .Where(t => typeof(KernelCommand).IsAssignableFrom(t))
                           .OrderBy(t => t.Name)
                           .ToList();
        var eventTypes = typeof(CSharpProjectKernel)
                         .Assembly.ExportedTypes
                         .Where(t => t is { IsAbstract: false, IsInterface: false })
                         .Where(t => typeof(KernelEvent).IsAssignableFrom(t))
                         .OrderBy(t => t.Name)
                         .ToList();

        foreach (var commandType in commandTypes)
        {
            KernelCommandEnvelope.RegisterCommand(commandType);
        }

        foreach (var eventType in eventTypes)
        {
            KernelEventEnvelope.RegisterEvent(eventType);
        }
    }

    private readonly IPrebuildFinder _prebuildFinder;

    private WorkspaceServer _workspaceServer;

    private Workspace _workspace;

    private Buffer _buffer;

    public CSharpProjectKernel(string name = "csharp", IPrebuildFinder prebuildFinder = null) : base(name)
    {
        _prebuildFinder = prebuildFinder;
        KernelInfo.LanguageName = "C#";
        KernelInfo.LanguageVersion = "13.0";
    }

    async Task IKernelCommandHandler<OpenProject>.HandleAsync(OpenProject command, KernelInvocationContext context)
    {
        _workspaceServer = new WorkspaceServer(_prebuildFinder ?? PrebuildFinder.Create(() => Prebuild.GetOrCreateConsolePrebuildAsync(enableBuild: false)));

        var extractor = new BufferFromRegionExtractor();
        _workspace = extractor.Extract(command.Project.Files.Select(f => new ProjectFileContent(f.RelativeFilePath, f.Content)).ToArray());

        var projectOpened = new ProjectOpened(
            command,
            _workspace.Buffers
                      .GroupBy(b => b.Id.FileName)
                      .OrderBy(g => g.Key)
                      .Select(g => new ProjectItem(
                                  g.Key,
                                  g.Select(r => r.Id.RegionName).Where(r => r != null).OrderBy(r => r).ToList(),
                                  g.Where(r => r is not null && !string.IsNullOrWhiteSpace(r.Id.RegionName))
                                   .ToDictionary(r => r.Id.RegionName, b => b.Content)))
                      .ToList());
        context.Publish(projectOpened);
    }

    async Task IKernelCommandHandler<OpenDocument>.HandleAsync(OpenDocument command, KernelInvocationContext context)
    {
        ThrowIfProjectIsNotOpened();

        var file = _workspace.Files.SingleOrDefault(f => string.Equals(f.Name, command.RelativeFilePath, StringComparison.OrdinalIgnoreCase));

        if (file is null)
        {
            // check for a region-less buffer instead
            var buffer = _workspace.Buffers.SingleOrDefault(b => string.Compare(b.Id.FileName, command.RelativeFilePath, StringComparison.OrdinalIgnoreCase) == 0 && b.Id.RegionName is null);
            if (buffer is { })
            {
                // create a temporary file with the buffer's content
                file = new(command.RelativeFilePath, buffer.Content);
            }
            else
            {
                // add it to the workspace
                file = new(command.RelativeFilePath, string.Empty);
                _workspace = new Workspace(
                    files: _workspace.Files.Concat(new[] { file }).ToArray());
            }
        }

        if (string.IsNullOrWhiteSpace(command.RegionName))
        {
            _buffer = new Buffer(file.Name, file.Text);
        }
        else
        {
            var extractor = new BufferFromRegionExtractor();
            _workspace = extractor.Extract(_workspace.Files);
            _buffer = _workspace.Buffers.SingleOrDefault(b => string.Compare(b.Id.FileName, command.RelativeFilePath, StringComparison.OrdinalIgnoreCase) == 0 && b.Id.RegionName == command.RegionName);
            if (_buffer is null)
            {
                throw new Exception($"Region '{command.RegionName}' not found in file '{command.RelativeFilePath}'");
            }
        }

        context.Publish(new DocumentOpened(command, command.RelativeFilePath, command.RegionName, _buffer.Content));
    }

    async Task IKernelCommandHandler<SubmitCode>.HandleAsync(SubmitCode command, KernelInvocationContext context)
    {
        ThrowIfProjectIsNotOpened();
        ThrowIfDocumentIsNotOpened();

        var updatedWorkspace = await AppendCodeToWorkspaceAsync(command.Code);
        _buffer = updatedWorkspace.Buffers.Single(b => b.Id == _buffer.Id);
        _workspace = updatedWorkspace;
    }

    async Task IKernelCommandHandler<CompileProject>.HandleAsync(CompileProject command, KernelInvocationContext context)
    {
        ThrowIfProjectIsNotOpened();
        ThrowIfDocumentIsNotOpened();

        var request = new WorkspaceRequest(_workspace, _buffer.Id);
        var result = await _workspaceServer.CompileAsync(request);

        var diagnostics = GetDiagnostics(_buffer.Content, result).ToArray();

        context.Publish(new DiagnosticsProduced(
                            diagnostics,
                            diagnostics.Select(d => new FormattedValue(PlainTextFormatter.MimeType, d.ToString())).ToArray(),
                            command));

        if (diagnostics.Any(d => d.Severity == CodeAnalysis.DiagnosticSeverity.Error))
        {
            context.Fail(command);
            return;
        }

        context.Publish(new AssemblyProduced(command, new Base64EncodedAssembly(result.Base64Assembly)));
    }

    async Task IKernelCommandHandler<RequestCompletions>.HandleAsync(RequestCompletions command, KernelInvocationContext context)
    {
        ThrowIfProjectIsNotOpened();
        ThrowIfDocumentIsNotOpened();

        var position = GetPositionFromLinePosition(command.Code, command.LinePosition);
        var updatedWorkspace = await AppendCodeToWorkspaceAsync(command.Code, position);
        var request = new WorkspaceRequest(updatedWorkspace, _buffer.Id);
        var completionResult = await _workspaceServer.GetCompletionsAsync(request);
        var completionItems = completionResult.Items.Select(item => new CompletionItem(
            displayText: item.DisplayText,
            kind: item.Kind,
            filterText: item.FilterText,
            sortText: item.SortText,
            insertText: item.InsertText,
            documentation: item.Documentation)).ToList();

        context.Publish(new CompletionsProduced(completionItems, command));
    }

    async Task IKernelCommandHandler<RequestDiagnostics>.HandleAsync(RequestDiagnostics command, KernelInvocationContext context)
    {
        ThrowIfProjectIsNotOpened();
        ThrowIfDocumentIsNotOpened();

        var updatedWorkspace = await AppendCodeToWorkspaceAsync(command.Code);
        var request = new WorkspaceRequest(updatedWorkspace, _buffer.Id);
        var result = await _workspaceServer.CompileAsync(request);

        var diagnostics = GetDiagnostics(command.Code, result).ToArray();

        context.Publish(new DiagnosticsProduced(
                            diagnostics,
                            diagnostics.Select(d => new FormattedValue(PlainTextFormatter.MimeType, d.ToString())).ToArray(),
                            command));
    }

    async Task IKernelCommandHandler<RequestSignatureHelp>.HandleAsync(RequestSignatureHelp command, KernelInvocationContext context)
    {
        ThrowIfProjectIsNotOpened();
        ThrowIfDocumentIsNotOpened();

        var position = GetPositionFromLinePosition(command.Code, command.LinePosition);
        var updatedWorkspace = await AppendCodeToWorkspaceAsync(command.Code, position);
        var request = new WorkspaceRequest(updatedWorkspace, _buffer.Id);
        var sigHelpResult = await _workspaceServer.GetSignatureHelpAsync(request);
        var sigHelpItems = sigHelpResult.Signatures.Select(s =>
            new SignatureInformation(
                s.Label,
                s.Documentation,
                s.Parameters.Select(p => new ParameterInformation(p.Label, p.Documentation)).ToArray())).ToArray();

        context.Publish(new SignatureHelpProduced(command, sigHelpItems, sigHelpResult.ActiveSignature, sigHelpResult.ActiveParameter));
    }

    private static int GetPositionFromLinePosition(string code, LinePosition linePosition)
    {
        var position = 0;
        var currentLine = 0;
        var currentCharacter = 0;
        foreach (var c in code)
        {
            if (currentLine == linePosition.Line &&
                currentCharacter == linePosition.Character)
            {
                return position;
            }

            position++;
            currentCharacter++;
            if (c is '\n')
            {
                currentLine++;
                currentCharacter = 0;
            }
        }

        return position;
    }

    private static LinePosition GetLinePositionFromPosition(string code, int position)
    {
        var currentPosition = 0;
        var currentLine = 0;
        var currentCharacter = 0;
        foreach (var c in code)
        {
            if (currentPosition == position)
            {
                return new LinePosition(currentLine, currentCharacter);
            }

            currentPosition++;
            currentCharacter++;
            if (c is '\n')
            {
                currentLine++;
                currentCharacter = 0;
            }
        }

        return new LinePosition(currentLine, currentCharacter);
    }

    private async Task<Workspace> AppendCodeToWorkspaceAsync(string code, int position = 0)
    {
        var updatedWorkspace = new Workspace(
            files: _workspace.Files,
            buffers: _workspace.Buffers.Where(b => b.Id != _buffer.Id).Concat(new[] { new Buffer(_buffer.Id, code, position: position) }).ToArray());
        var inlinedWorkspace = await updatedWorkspace.InlineBuffersAsync();
        return inlinedWorkspace;
    }

    private static IEnumerable<Diagnostic> GetDiagnostics(string code, CompileResult result)
    {
        if (result.Features.TryGetValue(nameof(Diagnostics), out var candidateDiagnostics) &&
            candidateDiagnostics is Diagnostics diags)
        {
            return diags.Select(d => new Diagnostic(new LinePositionSpan(GetLinePositionFromPosition(code, d.Start), GetLinePositionFromPosition(code, d.End)), d.Severity, d.Id, d.Message));
        }

        return Enumerable.Empty<Diagnostic>();
    }

    private void ThrowIfProjectIsNotOpened()
    {
        if (_workspaceServer is null || _workspace is null)
        {
            throw new InvalidOperationException($"Project must be opened, send the command '{nameof(OpenProject)}' first.");
        }
    }

    private void ThrowIfDocumentIsNotOpened()
    {
        if (_buffer is null)
        {
            throw new InvalidOperationException($"Document must be opened, send the command '{nameof(OpenDocument)}' first.");
        }
    }
}