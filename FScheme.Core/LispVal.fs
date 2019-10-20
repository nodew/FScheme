﻿namespace FScheme.Core

open System

type Number =
    | Float of float
    | Integer of int32

type Lisp =
    | Nil
    | Bool of bool
    | Number of Number
    | Atom of string
    | Text of string
    | Func of IFunc
    | Lambda of EnvCtx * IFunc
    | List of Lisp list

and Application = Lisp list

and IFunc = Lisp list -> Lisp

and VarCtx = Map<string, Lisp>
and FunCtx = Map<string, Lisp>

and EnvCtx = {
    varCtx: VarCtx;
    funCtx: FunCtx;
}

type Error =
    | TypeMismatch of string * Lisp
    | NumArgs of int * Lisp list
    | UnboundedVar of string
    | BadSpecialForm of string
    | NotFunction of Lisp

[<AutoOpen>]
module lispVal =
    let numToFloat = function
        | Integer a -> float a
        | Float a -> a

    let private unwords = String.concat " "

    let rec print = function
        | Nil -> "'()"
        | Bool true -> "#t"
        | Bool false -> "#f"
        | Number (Integer n) -> string(n)
        | Number (Float n) -> string(n)
        | Atom atom -> atom
        | Text s -> sprintf "\"%s\"" s
        | List lst -> unwordsList lst |> sprintf "(%s)"
        | Func _             -> "(internal function)"
        | Lambda (ctx, expr) -> "(lambda function)"

    and private unwordsList lst = lst |> List.map print |> unwords

    and printApp app = app |> List.map print |> String.concat "\n"

    and showError = function
        | NumArgs (n, args) -> args |> unwordsList |> sprintf "Error Number Arguments, expected %d, received args %s" n
        | TypeMismatch (txt, var) -> var |> print |> sprintf "Error Type Mismatch: %s %s" txt
        | UnboundedVar var -> var |> sprintf "Error Unbounded variable: %s"
        | BadSpecialForm s -> s |> sprintf "Error Bad Special Form: %s"
        | NotFunction var -> var |> print |> sprintf "Error Not a Function: %s"

    and throwException e = showError e |> failwith