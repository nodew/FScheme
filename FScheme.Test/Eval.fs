﻿namespace FScheme.Test

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

    let evalTest source = source |> toAst |> List.map (Eval.eval Eval.defaultEnv)

    [<Test>]
    let ``eval native expression`` () = 
        test <@ evalTest "(+ 1 2)" = [Number (Integer 3)] @>

    