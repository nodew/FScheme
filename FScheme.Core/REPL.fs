namespace FScheme.Core

open System

module REPL =
    let rec run env =
        printf "REPL> "
        let input = Console.ReadLine()
        let minput = input.Trim()
        match minput with
        | "quit" ->
            printfn "Exit"
            exit 0
        | _ ->
            let (result, newEnv) = Eval.evalSource env input
            result |> print |> printfn "%s"
            run newEnv

    let runREPL = run Eval.defaultEnv