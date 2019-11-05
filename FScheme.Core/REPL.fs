namespace FScheme.Core

open System

module REPL =
    let safeExec source =
        try
            Some(Eval.evalExpr Eval.environment source)
        with ex ->
            printfn "%s" (showError ex)
            None

    let rec runREPL () =
        printf "REPL> "
        let input = Console.ReadLine()
        safeExec input |> Option.map (printExpr >> printfn "%s") |> ignore
        runREPL ()