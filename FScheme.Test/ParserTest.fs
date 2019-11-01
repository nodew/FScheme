namespace FScheme.Test

open NUnit.Framework
open FsUnit
open FScheme.Core

module ParserTests =
    [<Test>]
    let ``parse number`` () =
        Parser.readExpr "123" |>  should equal (Lisp.Number (Integer 123))
        Parser.readExpr "-123" |>  should equal (Lisp.Number (Integer -123))
        Parser.readExpr "1.23" |>  should equal (Lisp.Number (Float 1.23))
        Parser.readExpr "-1.23" |>  should equal (Lisp.Number (Float -1.23))

    [<Test>]
    let ``parse atom`` () =
        Parser.readExpr "atom" |> should equal (Lisp.Atom "atom")
        Parser.readExpr "atom.test" |> should equal (Lisp.Atom "atom.test")
        Parser.readExpr "atom/test" |> should equal (Lisp.Atom "atom/test")

    [<Test>]
    let ``parse nil`` () =
        Parser.readExpr "'()" |> should equal Lisp.Nil

    [<Test>]
    let ``parse string`` () =
        Parser.readExpr "\"str\\\\ning\"" |> should equal (Lisp.Text "str\\ning")

    [<Test>]
    let ``parse bool`` () =
        Parser.readExpr "#t" |> should equal (Lisp.Bool true)
        Parser.readExpr "#f" |> should equal (Lisp.Bool false)

    [<Test>]
    let ``parse list`` () =
        Parser.readExpr "(1 2 3)" |> should equal (Lisp.List [Lisp.Number (Integer 1); Lisp.Number (Integer 2); Lisp.Number (Integer 3)])
        Parser.readExpr "(a b c)" |> should equal (Lisp.List [Lisp.Atom "a"; Lisp.Atom "b"; Lisp.Atom "c"])

    [<Test>]
    let ``parse quoted list`` () =
        Parser.readExpr "'(1 2 3)" |>
            should equal (Lisp.List [Lisp.Atom "quote"; Lisp.List [Lisp.Number (Integer 1); Lisp.Number (Integer 2); Lisp.Number (Integer 3)]])
        Parser.readExpr "'(a b c)" |>
            should equal (Lisp.List [Lisp.Atom "quote"; Lisp.List [Lisp.Atom "a"; Lisp.Atom "b"; Lisp.Atom "c"]])

    [<Test>]
    let ``parse define`` () =
        Parser.readExpr @"(define
                        (add a b)
                        (+ a b))"
            |> should equal (Lisp.List [
                                Lisp.Atom "define";
                                Lisp.List [Lisp.Atom "add"; Lisp.Atom "a"; Lisp.Atom "b"];
                                Lisp.List [Lisp.Atom "+"; Lisp.Atom "a"; Lisp.Atom "b"]
                            ])

    [<Test>]
    let ``print ast`` () =
        let expr = "(define (add a b) (+ a b))"
        Parser.readExpr expr
        |> fun result -> printExpr result |> should equal expr
