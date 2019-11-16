namespace FScheme.Core

module Primitives =
    let unop op args =
        match args with
        | [x] -> op x
        | _ -> NumArgsException (1, args) |> raise

    let binop op args =
        match args with
        | [x; y] -> op x y
        | _ -> NumArgsException (2, args) |> raise

    let binopFold op farg args =
        match args with
        | [a; b]  -> op a b
        | a :: rest -> List.fold op farg args
        | []-> NumArgsException (2, args) |> raise

    let numOp op x y =
        match x,y with
        | (Number (Integer a)), (Number (Integer b)) -> op (float a) (float b) |> int |> Integer |> Number
        | (Number a), (Number b) -> op (numToFloat a) (numToFloat b) |> Float |> Number
        | Nil,        (Number b) -> Number b
        | (Number a), Nil        -> Number a
        | (Number _),  b         -> TypeMismatchException ("numeric op", b) |> raise
        | a,           _         -> TypeMismatchException ("numeric op", a) |> raise

    let numCmp op x y =
        match x, y with
        | (Number a), (Number b) -> op (numToFloat a) (numToFloat b) |> Lisp.Bool
        | (Number _),  b         -> TypeMismatchException ("numeric op", b) |> raise
        | a,         _           -> TypeMismatchException ("numeric op", a) |> raise

    let stringOp op x y =
        match x,y with
        | (Lisp.String x), (Lisp.String y) -> op x y |> Lisp.String
        | Nil,        (Lisp.String y)    -> Lisp.String y
        | (Lisp.String x), Nil           -> Lisp.String x
        | (Lisp.String x),  y            -> TypeMismatchException ("string op", y) |> raise
        | x,           _               -> TypeMismatchException ("string op", x) |> raise

    let numBool op = function
        | (Number x) -> op (numToFloat x) |> Bool
        |  x         -> TypeMismatchException ("numeric op", x) |> raise

    let integerBool op = function
        | (Number (Integer x)) -> op x |> Bool
        |  x         -> TypeMismatchException ("numeric op", x) |> raise

    let notOp bool =
        match bool with
        | Bool true  -> Bool false
        | Bool false -> Bool true
        | _          -> TypeMismatchException ("bool op", bool) |> raise

    let eqCmd = fun a b -> a = b |> Bool

    let cons (exprs : Lisp list) =
        match exprs with
        | [x; List y] -> List (x :: y)
        | [x; y] -> List [x; y]
        | _ -> ExpectedListException "cons, in second argumnet" |> raise

    let car = function
        | [List (x :: _)] -> x
        | [List []] -> Nil
        | [] -> Nil
        | _ -> ExpectedListException "car" |> raise

    let cdr = function
        | [List (_ :: xs)] -> List xs
        | [List []] -> Nil
        | [] -> Nil
        | _ -> ExpectedListException "cdr" |> raise

    let list args = List args

    let display = function
        | (String s) -> printf "%s" s; Nil
        | other -> TypeMismatchException("string op", other) |> raise

    let exit = function
        | [] -> exit 0
        | [Number (Integer n)] -> exit n
        | _ -> MalformException "(exit) or (exit exitCode)" |> raise

    let mkFn fn = 
        ref (Func (fun cont args -> fn args |> cont))

    let primEnv = Map.ofList [
        ("+", binopFold (numOp (+)) (Lisp.Number (Integer 0)) |> mkFn)
        ("*", binopFold (numOp (*)) (Lisp.Number (Integer 1)) |> mkFn)
        ("-", numOp (-) |> binop |> mkFn)
        ("/", numOp (/) |> binop |> mkFn)
        ("string-append", binopFold (stringOp (+)) (Lisp.String "") |> mkFn)
        ("<", numCmp (<) |> binop |> mkFn)
        (">", numCmp (>) |> binop |> mkFn)
        (">=", numCmp (>=) |> binop |> mkFn)
        ("<=", numCmp (<=) |> binop |> mkFn)
        ("==", numCmp (=) |> binop |> mkFn)
        ("even?", integerBool (fun x -> x % 2 = 0) |> unop |> mkFn)
        ("odd?", integerBool (fun x -> x % 2 <> 0) |> unop |> mkFn)
        ("neg?", numBool (fun x -> x < 0.) |> unop |> mkFn)
        ("pos?", numBool (fun x -> x > 0.) |> unop |> mkFn)
        ("eq?", eqCmd |> binop |> mkFn)
        ("null?", (eqCmd Nil) |> unop |> mkFn)
        ("not", notOp |> unop |> mkFn)
        ("cons", cons |> mkFn)
        ("car", car |> mkFn)
        ("cdr", cdr |> mkFn)
        ("list", list |> mkFn)
        ("display", display |> unop |> mkFn)
        ("show", unop (printExpr >> Lisp.String) |> mkFn)
        ("exit", exit |> mkFn)
    ]

