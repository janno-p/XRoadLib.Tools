module XRoadLib.Tools.CodeGen

open Microsoft.Extensions.Logging
open System
open System.IO
open System.Net.Http
open System.Xml.Linq
open XRoadLib.Tools.Application
open XRoadLib.Tools.Syntax
open XRoadLib.Tools.Util

type private Marker = class end
let logger = Log.ofMarker<Marker>

let loadSchema (options: GeneratorOptions) = async {
    if options.WsdlUri |> String.IsNullOrEmpty then
        return failwith "WSDL location url is required."
    elif Uri.IsWellFormedUriString(options.WsdlUri, UriKind.Absolute) then
        logger.LogInformation("Requesting service description from network location: {0}.", options.WsdlUri)
        use client = new HttpClient()
        use! stream = client.GetStreamAsync(options.WsdlUri) |> Async.AwaitTask
        return XDocument.Load(stream)
    else
        let fileInfo = FileInfo(options.WsdlUri)
        if fileInfo.Exists then
            logger.LogInformation("Requesting service description from file: {0}.", fileInfo.FullName)
            return XDocument.Load(fileInfo.FullName)
        else
            return failwithf "Cannot resolve WSDL location: %s." options.WsdlUri
}

let genServiceCode (options: GeneratorOptions) (document: XDocument) =
    let definitions = document.Element(xnw "definitions") |> Object.except "Invalid WSDL document (definitions element is missing)."

    let mutable csAssemblyInfo =
        SourceFile.New(FileInfo(options.OutputPath.FullName </> "Properties" </> "AssemblyInfo.cs"))
        |> addUsing "System.Reflection"

    definitions.Element(xnw "documentation")
    |> Option.ofObj
    |> Option.iter (fun documentation -> csAssemblyInfo <- csAssemblyInfo |> addAssemblyDescription documentation.Value)

    if definitions.Elements(xnw "import") |> Seq.isNotEmpty then notImplemented "wsdl:import"

    (*
    if not (definitions.Elements(xnw "types") |> Seq.isEmpty) then notImplemented "wsdl:types"
    if not (definitions.Elements(xnw "message") |> Seq.isEmpty) then notImplemented "wsdl:message"
    if not (definitions.Elements(xnw "portType") |> Seq.isEmpty) then notImplemented "wsdl:portType"
    if not (definitions.Elements(xnw "binding") |> Seq.isEmpty) then notImplemented "wsdl:binding"
    if not (definitions.Elements(xnw "service") |> Seq.isEmpty) then notImplemented "wsdl:service"
    *)

    csAssemblyInfo |> saveFile
