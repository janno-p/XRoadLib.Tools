namespace XRoadLib.Tools

open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module Log =
    let loggerFactory = (new LoggerFactory()).AddConsole()
    let ofType<'t> = loggerFactory.CreateLogger<'t>()
    let ofMarker<'t> = loggerFactory.CreateLogger(typeof<'t>.DeclaringType)
