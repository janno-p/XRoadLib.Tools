module XRoadLib.Tools.Application

open Microsoft.Extensions.CommandLineUtils
open Microsoft.Extensions.Logging
open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Reflection
open System.Xml.Linq
open XRoadLib

type GeneratorOptions =
    { WsdlUri: string
      IsVerbose: bool
      IsCodeOutput: bool
      OutputPath: DirectoryInfo
      MappingPath: FileInfo option
      RootNamespace: string
      Schemas: IDictionary<string, XElement> }

type private Marker = class end
let logger = Log.ofMarker<GeneratorOptions>

let loadSchema (uri: string) = async {
    if uri |> String.IsNullOrEmpty then
        return failwith "Schema location url is required."
    elif Uri.IsWellFormedUriString(uri, UriKind.Absolute) then
        logger.LogInformation("Requesting schema from network location: {0}.", uri)
        use client = new HttpClient()
        use! stream = client.GetStreamAsync(uri) |> Async.AwaitTask
        return XDocument.Load(stream)
    else
        let fileInfo = FileInfo(uri)
        if fileInfo.Exists then
            logger.LogInformation("Requesting schema from file: {0}.", fileInfo.FullName)
            return XDocument.Load(fileInfo.FullName)
        else
            return failwithf "Cannot resolve schema location: %s." uri
}

let getInformationalVersion () =
    let assembly = typeof<GeneratorOptions>.GetTypeInfo().Assembly
    let attribute = assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>()
    attribute |> Option.ofObj |> Option.fold (fun _ attr -> attr.InformationalVersion) (assembly.GetName().Version.ToString())

let createApp () =
    let app = CommandLineApplication()
    app.Name <- "dotnet-xroad-gen"
    app.Description <- "XRoadLib code generator"
    app.ShortVersionGetter <- (fun () -> getInformationalVersion())
    app.HelpOption("-?|-h|--help") |> ignore
    app

let configureOptions (app: CommandLineApplication) =
    let loadCustomSchema path = async {
        let! doc = loadSchema path
        let schema = doc.Element(XName.Get("schema", NamespaceConstants.XSD))
        return (schema.Attribute(XName.Get("targetNamespace")).Value, schema)
    }
    let optVerbose = app.Option("-v|--verbose", "Verbose output", CommandOptionType.NoValue)
    let optSource = app.Argument("[wsdl]", "Url of service description file")
    let optCode = app.Option("-c|--code", "Generate code", CommandOptionType.NoValue)
    let optOutput = app.Option("-o|--output", "Output path", CommandOptionType.SingleValue)
    let optMapping = app.Option("-m|--mapping", "Customization mappings for generated code", CommandOptionType.SingleValue)
    let optNamespace = app.Option("-n|--namespace", "Root namespace of generated code", CommandOptionType.SingleValue)
    let optSchemas = app.Option("-s|--schema", "Additional schema files", CommandOptionType.MultipleValue)
    lazy({ WsdlUri = optSource.Value
           IsVerbose = optVerbose.HasValue()
           IsCodeOutput = optCode.HasValue()
           MappingPath = if optMapping.HasValue() then Some(FileInfo(optMapping.Value())) else None
           OutputPath = DirectoryInfo(if optOutput.HasValue() then optOutput.Value() else "Output")
           RootNamespace = if optNamespace.HasValue() then optNamespace.Value() else "MyNamespace"
           Schemas = optSchemas.Values |> Seq.map loadCustomSchema |> Async.Parallel |> Async.RunSynchronously |> dict })
