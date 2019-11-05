namespace FScheme.Test

open NUnit.Framework
open FsUnit
open FScheme.Core
open FParsec
open Swensen.Unquote

module EvalTest =
    let evalTest source = source |> Eval.evalText

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

    [<Test>]
    let ``eval car`` () =
        let expr = @"(car '(1 2))"
        test <@ evalTest expr = Number (Integer 1) @>

    [<Test>]
    let ``eval cdr`` () =
        let expr = @"(cdr '(1 2))"
        test <@ evalTest expr = List [Number (Integer 2)] @>

    [<Test>]
    let ``eval cadr`` () =
        let expr = @"(car (cdr '(1 2 3)))"
        test <@ evalTest expr = Number (Integer 2) @>

    [<Test>]
    let ``eval cons`` () =
        let expr = @"(cons 1 2)"
        test <@ evalTest expr = List [Number (Integer 1); Number (Integer 2)] @>

    [<Test>]
    let ``eval stdlib`` () =
        let expr = @"(cadr '((1 2) 3))"
        test <@ evalTest expr = Number (Integer 3) @>
