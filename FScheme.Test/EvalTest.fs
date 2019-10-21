namespace FScheme.Test

open NUnit.Framework
open FsUnit
open FScheme.Core
open FParsec
open Swensen.Unquote

module EvalTest = 
    let toAst source = 
        Parser.read source 
        |> fun result ->
            match result with
            | Success (x, _, _) -> x
            | Failure (msg, _, _) -> failwith msg

    let evalTest source = source |> toAst |> Eval.evalForms Eval.defaultEnv

    [<Test>]
    let ``eval native expression`` () = 
        test <@ evalTest "(+ 1 2)" = Number (Integer 3) @>

    [<Test>]
    let ``eval let binding`` () = 
        let expr = @"
            (let ((a 1) (b 2)) 
                (+ a b))"
        test <@ evalTest expr = Number (Integer 3) @>

    [<Test>]
    let ``eval if expression`` () = 
        let expr = @"
            (if (> 1 2)
                1
                2)"
        test <@ evalTest expr = Number (Integer 2) @>