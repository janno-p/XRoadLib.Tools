module XRoadLib.Tools.Application

open Microsoft.Extensions.CommandLineUtils
open System.Collections.Generic
open System.IO
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

let loadSchema path =
    let fi = FileInfo(path)
    use s = fi.OpenRead()
    let doc = XDocument.Load(s)
    let schema = doc.Root.Element(XName.Get("schema", NamespaceConstants.XSD))
    (schema.Attribute(XName.Get("targetNamespace")).Value, schema)

let configureOptions (app: CommandLineApplication) =
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
           Schemas = optSchemas.Values |> Seq.map loadSchema |> dict })
