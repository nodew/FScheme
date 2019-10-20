namespace FScheme.Test

open NUnit.Framework
open FsUnit
open FScheme.Core
open FParsec

module ParserTests =
    let parseTest assertation result =
        match result with
            | Success (x, _, _) -> x |> assertation
            | Failure (msg, _, _) -> Assert.Fail(msg)

    [<Test>]
    let ``parse number`` () =
        Parser.read "123" |>  parseTest (should equal [Lisp.Number (Integer 123)])
        Parser.read "-123" |>  parseTest (should equal [Lisp.Number (Integer -123)])
        Parser.read "1.23" |>  parseTest (should equal [Lisp.Number (Float 1.23)])
        Parser.read "-1.23" |>  parseTest (should equal [Lisp.Number (Float -1.23)])

    [<Test>]
    let ``parse atom`` () =
        Parser.read "atom" |> parseTest (should equal [Lisp.Atom "atom"])
        Parser.read "atom.test" |> parseTest (should equal [Lisp.Atom "atom.test"])
        Parser.read "atom/test" |> parseTest (should equal [Lisp.Atom "atom/test"])

    [<Test>]
    let ``parse nil`` () =
        Parser.read "'()" |> parseTest (should equal [Lisp.Nil])

    [<Test>]
    let ``parse string`` () =
        Parser.read "\"str\\\\ning\"" |> parseTest (should equal [Lisp.Text "str\\ning"])

    [<Test>]
    let ``parse bool`` () =
        Parser.read "#t" |> parseTest (should equal [Lisp.Bool true])
        Parser.read "#f" |> parseTest (should equal [Lisp.Bool false])

    [<Test>]
    let ``parse list`` () =
        Parser.read "(1 2 3)" |> parseTest (should equal [Lisp.List [Lisp.Number (Integer 1); Lisp.Number (Integer 2); Lisp.Number (Integer 3)]])
        Parser.read "(a b c)" |> parseTest (should equal [Lisp.List [Lisp.Atom "a"; Lisp.Atom "b"; Lisp.Atom "c"]])

    [<Test>]
    let ``parse quoted list`` () =
        Parser.read "'(1 2 3)" |> parseTest
            (should equal [Lisp.List [Lisp.Atom "quote"; Lisp.List [Lisp.Number (Integer 1); Lisp.Number (Integer 2); Lisp.Number (Integer 3)]]])
        Parser.read "'(a b c)" |> parseTest
            (should equal [Lisp.List [Lisp.Atom "quote"; Lisp.List [Lisp.Atom "a"; Lisp.Atom "b"; Lisp.Atom "c"]]])

    [<Test>]
    let ``parse define`` () =
        Parser.read "(define (add a b) (+ a b))" |> parseTest
            (should equal [Lisp.List [
                                Lisp.Atom "define";
                                Lisp.List [Lisp.Atom "add"; Lisp.Atom "a"; Lisp.Atom "b"];
                                Lisp.List [Lisp.Atom "+"; Lisp.Atom "a"; Lisp.Atom "b"]
                          ]])

    [<Test>]
    let ``print ast`` () = 
        let expr = "(define (add a b) (+ a b))"
        Parser.read expr
        |> fun result ->
            match result with
            | Success (x, _, _) -> print x.[0] |> should equal expr
            | Failure (msg, _, _) -> Assert.Fail(msg)
