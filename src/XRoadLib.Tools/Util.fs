module XRoadLib.Tools.Util

open System.IO
open System.Xml.Linq
open XRoadLib

let xn nm = XName.Get(nm)
let xns nm ns = XName.Get(nm, ns)
let xnw nm = xns nm NamespaceConstants.WSDL

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
