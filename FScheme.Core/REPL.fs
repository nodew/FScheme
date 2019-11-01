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
            match Eval.safeExec Eval.evalExpr env input with
            | Some (result, newEnv) ->
                result |> printExpr |> printfn "%s"
                run newEnv
            | None -> run env

    let runREPL () =
        let env = Eval.envWithStd.Value
        run env