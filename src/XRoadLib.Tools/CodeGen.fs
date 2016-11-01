module XRoadLib.Tools.CodeGen

open System
open System.Collections.Generic
open System.IO
open System.Net.Http
open System.Xml.Linq
open XRoadLib
open XRoadLib.Tools
open XRoadLib.Tools.Application
open XRoadLib.Tools.Syntax
open XRoadLib.Tools.Util

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

    let schemaLookup = definitions |> Definition.buildLookup options

    //if definitions.Elements(xnw "message") |> Seq.isNotEmpty then notImplemented "wsdl:message"

    let portTypeUnits = definitions.Elements(xnw "portType") |> Seq.map (genPortType options) |> Seq.toList
    let bindingUnits = definitions.Elements(xnw "binding") |> Seq.map (genBinding options) |> Seq.toList
    let serviceUnits = definitions.Elements(xnw "service") |> Seq.map (genService options) |> Seq.toList

    csAssemblyInfo :: portTypeUnits @ bindingUnits @ serviceUnits
    |> Seq.iter saveFile
