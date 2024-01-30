// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace FSharp.VisualStudio.Extension;

using System.Diagnostics;
using System.IO;
using System.IO.Pipelines;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Extensibility;
using Microsoft.VisualStudio.Extensibility.Editor;
using Microsoft.VisualStudio.Extensibility.LanguageServer;
using Microsoft.VisualStudio.ProjectSystem.Query;
using Microsoft.VisualStudio.RpcContracts.LanguageServerProvider;
using Nerdbank.Streams;

/// <inheritdoc/>
#pragma warning disable VSEXTPREVIEW_LSP // Type is for evaluation purposes only and is subject to change or removal in future updates.
[VisualStudioContribution]
internal class FSharpLanguageServerProvider : LanguageServerProvider
{
    /// <summary>
    /// Gets the document type for FSharp code files.
    /// </summary>
    [VisualStudioContribution]
    public static DocumentTypeConfiguration FSharpDocumentType => new("F#")
    {
        FileExtensions = [".fs", ".fsi", ".fsx"],
        BaseDocumentType = LanguageServerBaseDocumentType,
    };

    /// <inheritdoc/>
    public override LanguageServerProviderConfiguration LanguageServerProviderConfiguration => new(
        "%FSharpLspExtension.FSharpLanguageServerProvider.DisplayName%",
        [DocumentFilter.FromDocumentType(FSharpDocumentType)]);

    /// <inheritdoc/>
    public override Task<IDuplexPipe?> CreateServerConnectionAsync(CancellationToken cancellationToken)
    {
        var what = this.Extensibility.Workspaces();
        //var result = what.QueryProjectsAsync(project => project.With(p => p.Kind == "6EC3EE1D-3C4E-46DD-8F32-0CC8E7565705"), cancellationToken).Result;
        var result = what.QueryProjectsAsync(project => project, cancellationToken).Result;


        return (null);
    }

    /// <inheritdoc/>
    public override Task OnServerInitializationResultAsync(ServerInitializationResult serverInitializationResult, LanguageServerInitializationFailureInfo? initializationFailureInfo, CancellationToken cancellationToken)
    {
        if (serverInitializationResult == ServerInitializationResult.Failed)
        {
            // Log telemetry for failure and disable the server from being activated again.
            this.Enabled = false;
        }

        return base.OnServerInitializationResultAsync(serverInitializationResult, initializationFailureInfo, cancellationToken);
    }
}
#pragma warning restore VSEXTPREVIEW_LSP // Type is for evaluation purposes only and is subject to change or removal in future updates.
