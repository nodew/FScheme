// Learn more about F# at http://fsharp.org

open System.Reflection
open Argu
open FScheme.Core

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
            let assemblyName = Assembly.GetEntryAssembly().GetName()
            printfn "%s" (assemblyName.Version.ToString())
        elif repl.IsSome then
                REPL.runREPL ()
        else
            let file = result.TryGetResult Run
            let finalFile = if file.IsSome then file else result.TryGetResult File
            if finalFile.IsSome then
                Eval.evalFile finalFile.Value |> ignore
            else
                printfn "%s" usage
    with e ->
        printfn "%s" e.Message
    0 // return an integer exit code
