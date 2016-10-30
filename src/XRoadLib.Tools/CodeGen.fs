module XRoadLib.Tools.CodeGen

open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Xml.Linq
open XRoadLib
open XRoadLib.Tools.Application
open XRoadLib.Tools.Syntax
open XRoadLib.Tools.Util

type DefinitionLookup =
    { Types: Map<string, XElement>
      Elements: Map<string, XElement>
      Messages: Map<string, XElement>
      PortTypes: Map<string, XElement>
      Operations: Map<string, XElement>
      Bindings: Map<string, XElement>
      OperationBindings: Map<string, XElement>
      Services: Map<string, XElement>
      Ports: Map<string, XElement> }

let isSystemNamespace = function
    | NamespaceConstants.XMIME
    | NamespaceConstants.XROAD
    | NamespaceConstants.XSD
    | NamespaceConstants.XROAD_V4 -> true
    | _ -> false

let buildDefinitionLookup (options: GeneratorOptions) (definitions: XElement) =
    let types = Dictionary<string, XElement>()
    let elements = Dictionary<string, XElement>()
    let deftns = definitions |> requiredAttribute "targetNamespace"
    let rec parseSchema (schema: XElement) =
        let p = new XElementParser(schema)
        let tns = schema |> requiredAttribute "targetNamespace"
        let addCollection (coll: IDictionary<_,_>) (e: XElement) = coll.Add((xns (e |> requiredAttribute "name") tns).ToString(), e)
        let addTypes = addCollection types
        let addElements = addCollection elements
        let loadImport (e: XElement) =
            let nsname = e |> requiredAttribute "namespace"
            if nsname |> isSystemNamespace |> not then
                match options.Schemas.TryGetValue(nsname) with
                | true, sch -> parseSchema(sch)
                | _ -> e |> attribute "schemaLocation"
                         |> Option.iter (fun schloc ->
                            let doc = loadSchema schloc |> Async.RunSynchronously
                            parseSchema(doc.Element(xnd "schema")))
        let rec parseLeading () =
            if (p.ParseElement("include") ||
                p.ParseElement("import", loadImport) ||
                p.ParseElement("redefine") ||
                p.ParseElement("annotation"))
            then parseLeading()
        let rec parseFollowing () =
            if (p.ParseElement("simpleType", addTypes) ||
                p.ParseElement("complexType", addTypes) ||
                p.ParseElement("group") ||
                p.ParseElement("attributeGroup") ||
                p.ParseElement("element", addElements) ||
                p.ParseElement("attribute") ||
                p.ParseElement("notation") ||
                p.ParseElement("annotation"))
            then parseFollowing()
        parseLeading()
        parseFollowing()
        p.ThrowIfNotDone()
    definitions.Elements(xnw "types")
    |> Seq.iter (fun wtypes ->
        wtypes.Elements(xnd "schema")
        |> Seq.iter parseSchema)
    let elemName (e: XElement) = (xns (e |> requiredAttribute "name") deftns).ToString()
    let portTypes = definitions.Elements(xnw "portType") |> Seq.map (fun e -> elemName e, e) |> Map.ofSeq
    let bindings = definitions.Elements(xnw "binding") |> Seq.map (fun e -> elemName e, e) |> Map.ofSeq
    let services = definitions.Elements(xnw "service") |> Seq.map (fun e -> elemName e, e) |> Map.ofSeq
    { Types = types |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq
      Elements = elements |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq
      Messages = definitions.Elements(xnw "message") |> Seq.map (fun e -> elemName e, e) |> Map.ofSeq
      PortTypes = portTypes
      Operations = portTypes |> Seq.collect (fun kvp -> kvp.Value.Elements(xnw "operation")) |> Seq.map (fun e -> elemName e, e) |> Map.ofSeq
      Bindings = bindings
      OperationBindings = bindings |> Seq.collect (fun kvp -> kvp.Value.Elements(xnw "operation")) |> Seq.map (fun e -> elemName e, e) |> Map.ofSeq
      Services = services
      Ports = services |> Seq.collect (fun kvp -> kvp.Value.Elements(xnw "port")) |> Seq.map (fun e -> elemName e, e) |> Map.ofSeq }

let genPortType (options: GeneratorOptions) (element: XElement) : SourceFile =
    let name = element |> requiredAttribute "name" |> sprintf "I%s"
    let portTypeInterface = declareInterface name
    SourceFile.New(FileInfo(options.OutputPath.FullName </> (sprintf "%s.cs" name)))
    |> addNamespace options.RootNamespace [portTypeInterface]

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
                let initializer = createObjExpr propType |> addArguments []
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

    let schemaLookup = definitions |> buildDefinitionLookup options

    //if definitions.Elements(xnw "message") |> Seq.isNotEmpty then notImplemented "wsdl:message"

    let portTypeUnits = definitions.Elements(xnw "portType") |> Seq.map (genPortType options) |> Seq.toList
    let bindingUnits = definitions.Elements(xnw "binding") |> Seq.map (genBinding options) |> Seq.toList
    let serviceUnits = definitions.Elements(xnw "service") |> Seq.map (genService options) |> Seq.toList

    csAssemblyInfo :: portTypeUnits @ bindingUnits @ serviceUnits
    |> Seq.iter saveFile
