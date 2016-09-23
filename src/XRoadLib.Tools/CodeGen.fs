module XRoadLib.Tools.CodeGen

open Microsoft.Extensions.Logging
open System
open System.IO
open System.Net.Http
open System.Xml.Linq
open XRoadLib
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

let genBinding (options: GeneratorOptions) (element: XElement) : SourceFile =
    let name = element |> requiredAttribute "name"
    let ifaceName = element |> requiredAttribute "type"
    let ctor =
        createCtor name
        |> addParameters []
        |> addBody emptyBlock
        |> toMember
    let services =
        []
    let bindingClass =
        declareClass name
        |> addBaseType (sprintf "I%s" ifaceName)
        |> addClassMembers (ctor :: services)
    SourceFile.New(FileInfo(options.OutputPath.FullName </> (sprintf "%s.cs" name)))
    |> addNamespace options.RootNamespace [bindingClass]

let genService (options: GeneratorOptions) (element: XElement) : SourceFile =
    let name = element |> requiredAttribute "name"
    let serviceClass =
        declareClass name
        |> addClassMembers (
            element.Elements(xnw "port")
            |> Seq.map (fun port ->
                let propType = port |> requiredAttribute "binding"
                let propName = port |> requiredAttribute "name"
                let initializer =
                    let i = createObjExpr propType
                    match port |> child (xns "address" NamespaceConstants.XROAD) |> Option.bind (fun x -> x |> attribute "producer") with
                    | None | Some("") -> i
                    | Some(producerName) -> i |> addArguments [stringLiteral producerName]
                upcast (declareProperty propType propName initializer)))
    SourceFile.New(FileInfo(options.OutputPath.FullName </> (sprintf "%s.cs" name)))
    |> addNamespace options.RootNamespace [serviceClass]

let genServiceCode (options: GeneratorOptions) (document: XDocument) =
    let definitions = document.Element(xnw "definitions") |> Object.except "Invalid WSDL document (definitions element is missing)."

    let mutable csAssemblyInfo =
        SourceFile.New(FileInfo(options.OutputPath.FullName </> "Properties" </> "AssemblyInfo.cs"))
        |> addUsing "System.Reflection"

    definitions.Element(xnw "documentation")
    |> Option.ofObj
    |> Option.iter (fun documentation -> csAssemblyInfo <- csAssemblyInfo |> addAssemblyDescription documentation.Value)

    if definitions.Elements(xnw "import") |> Seq.isNotEmpty then notImplemented "wsdl:import"
    //if definitions.Elements(xnw "types") |> Seq.isNotEmpty then notImplemented "wsdl:types"
    //if definitions.Elements(xnw "message") |> Seq.isNotEmpty then notImplemented "wsdl:message"
    //if definitions.Elements(xnw "portType") |> Seq.isNotEmpty then notImplemented "wsdl:portType"

    let bindingUnits = definitions.Elements(xnw "binding") |> Seq.map (genBinding options) |> Seq.toList
    let serviceUnits = definitions.Elements(xnw "service") |> Seq.map (genService options) |> Seq.toList

    csAssemblyInfo :: bindingUnits @ serviceUnits
    |> Seq.iter saveFile
