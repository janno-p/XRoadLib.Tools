module XRoadLib.Tools.Util

open System
open System.IO
open System.Xml.Linq
open XRoadLib

let xn nm = XName.Get(nm)
let xns nm ns = XName.Get(nm, ns)
let xnw nm = xns nm NamespaceConstants.WSDL
let xnd nm = xns nm NamespaceConstants.XSD

let notImplemented msg = failwithf "Not implemented: %s" msg

[<RequireQualifiedAccess>]
module Option =
    let except msg o = match o with Some(v) -> v | None -> failwith msg

[<RequireQualifiedAccess>]
module Object =
    let except msg o = o |> Option.ofObj |> Option.except msg

[<RequireQualifiedAccess>]
module Seq =
    let isNotEmpty (source: 't seq) = source |> Seq.isEmpty |> not

[<AutoOpen>]
module Operators =
    let (</>) a b = Path.Combine(a, b)

[<AutoOpen>]
module XElement =
    let child name (element: XElement) = element.Element(name) |> Option.ofObj
    let attribute attributeName (element: XElement) = element.Attribute(xn attributeName) |> Option.ofObj |> Option.map (fun x -> x.Value)
    let hasAttribute attributeName (element: XElement) = element.Attribute(xn attributeName) |> isNotNull
    let requiredAttribute attributeName (element: XElement) =
        element.Attribute(xn attributeName) |> Option.ofObj |> Option.map (fun x -> x.Value)
        |> Option.except (sprintf "Element `%A` is missing required attribute `%s`" element.Name attributeName)

type XElementParser(element: XElement, ?ns) as this =
    let ns = defaultArg ns NamespaceConstants.XSD
    let mutable isDone = true
    let enumerator = element.Elements().GetEnumerator()
    let moveToNext () = match enumerator.MoveNext() with true -> isDone <- false; true | x -> x
    member __.IsDone with get() = isDone
    member __.AttributeNotImplemented(attributeName) =
        if element |> hasAttribute attributeName then attributeName |> notImplemented
    member __.ParseElement(name, ?action: XElement -> unit) =
        if isDone && (moveToNext() |> not) then false
        elif enumerator.Current |> isNotNull && enumerator.Current.Name = XName.Get(name, ns) then
            if action |> Option.isNone then enumerator.Current.Name.ToString() |> notImplemented
            action |> Option.iter (fun f -> f(enumerator.Current))
            isDone <- true
            true
        else false
    member __.ThrowIfNotDone() =
        if not isDone then enumerator.Current.Name.ToString() |> notImplemented
    interface IDisposable with
        member __.Dispose() = this.ThrowIfNotDone()
