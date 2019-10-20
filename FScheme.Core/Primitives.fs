namespace FScheme.Core

module Primitives =
    let unop op args =
        match args with
        | [x] -> op x
        | _ -> NumArgs (1, args) |> throwException

    let binop op args =
        match args with
        | [x; y] -> op x y
        | _ -> NumArgs (2, args) |> throwException

    let binopFold op farg args =
        match args with
        | [a; b]  -> op a b
        | a :: rest -> List.fold op farg args
        | []-> NumArgs (2, args) |> throwException

    let numOp op x y =
        match x,y with
        | (Number (Integer a)), (Number (Integer b)) -> op (float a) (float b) |> int |> Integer |> Number
        | (Number a), (Number b) -> op (numToFloat a) (numToFloat b) |> Float |> Number
        | Nil,        (Number b) -> Number b
        | (Number a), Nil        -> Number a
        | (Number _),  b         -> TypeMismatch ("numeric op", b) |> throwException
        | a,           _         -> TypeMismatch ("numeric op", a) |> throwException

    let numCmp op x y =
        match x, y with
        | (Number a), (Number b) -> op (numToFloat a) (numToFloat b) |> Lisp.Bool
        | (Number _),  b         -> TypeMismatch ("numeric op", b) |> throwException
        | a,         _           -> TypeMismatch ("numeric op", a) |> throwException

    let stringOp op x y =
        match x,y with
        | (Lisp.Text x), (Lisp.Text y) -> op x y |> Lisp.Text
        | Nil,        (Lisp.Text y)    -> Lisp.Text y
        | (Lisp.Text x), Nil           -> Lisp.Text x
        | (Lisp.Text x),  y            -> TypeMismatch ("string op", y) |> throwException
        | x,           _               -> TypeMismatch ("string op", x) |> throwException

    let numBool op = function
        | (Number x) -> op (numToFloat x) |> Bool
        |  x         -> TypeMismatch ("numeric op", x) |> throwException

    let integerBool op = function
        | (Number (Integer x)) -> op x |> Bool
        |  x         -> TypeMismatch ("numeric op", x) |> throwException

    let mkFn fn = Func fn

    let primEnv: Map<string, Lisp> =
        Map.empty.
                Add("+", binopFold (numOp (+)) (Lisp.Number (Integer 0)) |> mkFn).
                Add("*", binopFold (numOp (*)) (Lisp.Number (Integer 1)) |> mkFn).
                Add("-", numOp (-) |> binop |> mkFn).
                Add("/", numOp (/) |> binop |> mkFn).
                Add("string-append", binopFold (stringOp (+)) (Lisp.Text "") |> mkFn).
                Add("<", numCmp (<) |> binop |> mkFn).
                Add(">", numCmp (>) |> binop |> mkFn).
                Add(">=", numCmp (>=) |> binop |> mkFn).
                Add("<=", numCmp (<=) |> binop |> mkFn).
                Add("==", numCmp (=) |> binop |> mkFn).
                Add("even?", integerBool (fun x -> x % 2 = 0) |> unop |> mkFn).
                Add("odd?", integerBool (fun x -> x % 2 <> 0) |> unop |> mkFn).
                Add("neg?", numBool (fun x -> x < 0.) |> unop |> mkFn).
                Add("pos?", numBool (fun x -> x > 0.) |> unop |> mkFn)
