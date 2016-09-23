module XRoadLib.Tools.Program

open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open XRoadLib.Tools.Application
open XRoadLib.Tools.CodeGen

[<EntryPoint>]
let main args =
    let logger = Log.ofType<ILogger>
    try
        let app = createApp()
        let optionsAccessor = configureOptions app
        app.OnExecute(fun () ->
            async {
                let options = optionsAccessor.Value
                let! schemaDoc = loadSchema options
                options.OutputPath.Delete(true)
                genServiceCode options schemaDoc
                return 0
            } |> Async.StartAsTask)
        app.Execute(args)
    with ex ->
        logger.LogCritical(EventId(), ex, ex.Message)
        1
