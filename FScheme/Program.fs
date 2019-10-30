// Learn more about F# at http://fsharp.org

open FScheme.Core

[<EntryPoint>]
let main argv =
    match argv with
    | [| "repl" |] -> REPL.runREPL |> ignore
    | _ ->
        printfn "Unknown operation"
        exit 0
    0 // return an integer exit code
