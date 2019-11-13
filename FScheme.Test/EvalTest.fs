namespace FScheme.Test

open NUnit.Framework
open FsUnit
open FScheme.Core
open FParsec
open Swensen.Unquote

module EvalTest =
    let evalTest source = source |> Eval.evalText |> printExpr

    [<Test>]
    let ``eval native expression`` () =
        test <@ evalTest "(+ 1 2)" = "3" @>

    [<Test>]
    let ``eval let binding`` () =
        let expr = @"
            (let ((a 1) (b 2))
                (+ a b))"
        test <@ evalTest expr = "3" @>

    [<Test>]
    let ``eval if expression`` () =
        let expr = @"
            (if (> 1 2)
                1
                2)"
        test <@ evalTest expr = "2" @>

    [<Test>]
    let ``eval car`` () =
        let expr = @"(car '(1 2))"
        test <@ evalTest expr = "1" @>

    [<Test>]
    let ``eval cdr`` () =
        let expr = @"(cdr '(1 2))"
        test <@ evalTest expr = "'(2)" @>

    [<Test>]
    let ``eval cadr`` () =
        let expr = @"(car (cdr '((1 2) 3)))"
        test <@ evalTest expr = "3" @>

    [<Test>]
    let ``eval cons`` () =
        let expr = @"(cons 1 2)"
        test <@ evalTest expr = "(1 2)" @>

    [<Test>]
    let ``eval stdlib`` () =
        let expr = @"(cadr '((1 2) 3))"
        test <@ evalTest expr = "3" @>

    [<Test>]
    let ``eval quasiquote`` () =
        let expr = @"(let ((a 1) (b 2)) `(,a ,b 3))"
        test <@ evalTest expr = "'(1 2 3)" @>

    [<Test>]
    let ``eval lambda`` () =
        let expr1 = @"((lambda x x) 1)"
        test <@ evalTest expr1 = "1" @>

        let expr1 = @"((lambda (a b) (+ a b)) 1 2)"
        test <@ evalTest expr1 = "3" @>

        let expr1 = @"((lambda (a . b) `,b) 1 2 3)"
        test <@ evalTest expr1 = "'(2 3)" @>

    [<Test>]
    let ``eval define`` () =
        let expr1 = @"
           (define myId (lambda x x)) 
           (myId 1)"
        test <@ evalTest expr1 = "1" @>

        let expr1 = @"
            (define (myAdd a b) (+ a b))
            (myAdd 1 2)"
        test <@ evalTest expr1 = "3" @>

        let expr1 = @"
            (define (cdrParam a . b) `,b)
            (cdrParam 1 2 3)"
        test <@ evalTest expr1 = "(2 3)" @>

