namespace FScheme.Test

open NUnit.Framework
open FsUnit
open FScheme.Core
open Swensen.Unquote

module ParserTests =
    [<Test>]
    let ``parse number`` () =
        Parser.readExpr "123" |>  should equal (Lisp.Number (Integer 123))
        Parser.readExpr "-123" |>  should equal (Lisp.Number (Integer -123))
        Parser.readExpr "1.23" |>  should equal (Lisp.Number (Float 1.23))
        Parser.readExpr "-1.23" |>  should equal (Lisp.Number (Float -1.23))
        Parser.readExpr "#b101" |>  should equal (Lisp.Number (Integer 5))
        Parser.readExpr "#o101" |>  should equal (Lisp.Number (Integer 65))
        Parser.readExpr "#x1a1" |>  should equal (Lisp.Number (Integer 417))

    [<Test>]
    let ``parse atom`` () =
        Parser.readExpr "atom" |> should equal (Lisp.Atom "atom")
        Parser.readExpr "atom/test" |> should equal (Lisp.Atom "atom/test")

    [<Test>]
    let ``parse nil`` () =
        Parser.readExpr "'()" |> should equal Lisp.Nil
    
    [<Test>]
    let ``parse char`` () =
        Parser.readExpr "#\c" |> should equal (Lisp.Char 'c')

    [<Test>]
    let ``parse string`` () =
        Parser.readExpr "\"str\\\\ning\"" |> should equal (Lisp.String "str\\ning")

    [<Test>]
    let ``parse bool`` () =
        Parser.readExpr "#t" |> should equal (Lisp.Bool true)
        Parser.readExpr "#f" |> should equal (Lisp.Bool false)

    [<Test>]
    let ``parse vector`` () =
        Parser.readExpr "#(1 2 3)" |> should equal (Lisp.Vector [| Lisp.Number (Integer 1); Lisp.Number (Integer 2); Lisp.Number (Integer 3) |])
        Parser.readExpr "#(a b c)" |> should equal (Lisp.Vector [| Lisp.Atom "a"; Lisp.Atom "b"; Lisp.Atom "c" |])

    [<Test>]
    let ``parse list`` () =
        Parser.readExpr "(1 2 3)" |> should equal (Lisp.List [Lisp.Number (Integer 1); Lisp.Number (Integer 2); Lisp.Number (Integer 3)])
        Parser.readExpr "(a b c)" |> should equal (Lisp.List [Lisp.Atom "a"; Lisp.Atom "b"; Lisp.Atom "c"])
    
    [<Test>]
    let ``parse dotted list`` () =
        Parser.readExpr "(1 . 2)" |> should equal (Lisp.DottedList ([Lisp.Number (Integer 1)], Lisp.Number (Integer 2)))

    [<Test>]
    let ``parse quoted list`` () =
        Parser.readExpr "'(1 2 3)" |>
            should equal (Lisp.List [Lisp.Atom "quote"; Lisp.List [Lisp.Number (Integer 1); Lisp.Number (Integer 2); Lisp.Number (Integer 3)]])
        Parser.readExpr "'(a b c)" |>
            should equal (Lisp.List [Lisp.Atom "quote"; Lisp.List [Lisp.Atom "a"; Lisp.Atom "b"; Lisp.Atom "c"]])

    [<Test>]
    let ``parse comment`` () =
        let expr = @"
                    ; this is a comment
                    (a)"
        Parser.readContent expr |> Assert.NotNull

    [<Test>]
    let ``parse quasiquote and unquote`` () =
        test <@ Parser.readExpr @"`'()" = (Lisp.List [(Atom "quasiquote"); Nil]) @>
        test <@ Parser.readExpr @"`(,a)" = (Lisp.List [(Atom "quasiquote"); Lisp.List[Lisp.List [Atom "unquote"; Atom "a"]]]) @>
        test <@ Parser.readExpr @"`(,@a)" = (Lisp.List [(Atom "quasiquote"); Lisp.List[Lisp.List [Atom "unquote-slicing"; Atom "a"]]]) @>