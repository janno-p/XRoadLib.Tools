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

type SchemaLookup =
    { Types: IDictionary<string, XElement>
      Elements: IDictionary<string, XElement> }

let isSystemNamespace = function
    | NamespaceConstants.XMIME
    | NamespaceConstants.XROAD
    | NamespaceConstants.XSD
    | NamespaceConstants.XROAD_V4 -> true
    | _ -> false

let buildSchemaLookup (options: GeneratorOptions) (definitions: XElement) =
    let types = Dictionary<string, XElement>()
    let elements = Dictionary<string, XElement>()
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
    { Types = types; Elements = elements }

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

    let schemaLookup = definitions |> buildSchemaLookup options

    //if definitions.Elements(xnw "message") |> Seq.isNotEmpty then notImplemented "wsdl:message"
    //if definitions.Elements(xnw "portType") |> Seq.isNotEmpty then notImplemented "wsdl:portType"

    let bindingUnits = definitions.Elements(xnw "binding") |> Seq.map (genBinding options) |> Seq.toList
    let serviceUnits = definitions.Elements(xnw "service") |> Seq.map (genService options) |> Seq.toList

    csAssemblyInfo :: bindingUnits @ serviceUnits
    |> Seq.iter saveFile
