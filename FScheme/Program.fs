// Learn more about F# at http://fsharp.org

open FScheme.Core
open Argu

[<NoAppSettings>]
type CliArguments =
    | [<MainCommand>] File of string
    | [<CliPrefix(CliPrefix.None)>] Run of string
    | [<CliPrefix(CliPrefix.None)>] Repl
    | Version
    with 
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | File file -> "Run a script"
                | Run file -> "Run a script"
                | Repl -> "Enter REPL"
                | Version -> "Get current version"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArguments>(programName = "fscheme.exe")
    let usage = parser.PrintUsage()
    try
        let result = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        let repl = result.TryGetResult Repl
        if result.TryGetResult Version |> Option.isSome then
            printfn "0.0.1"
        elif repl.IsSome then
               REPL.runREPL () |> ignore
        else
            let file = result.TryGetResult Run
            let finalFile = if file.IsSome then file else result.TryGetResult File
            if finalFile.IsSome then
                Eval.evalFile finalFile.Value |> fst |> print |> printfn "%s"
            else
                printfn "%s" usage
    with e ->
        printfn "%s" e.Message
    0 // return an integer exit code
