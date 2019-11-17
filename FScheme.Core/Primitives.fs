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
        | a :: rest -> op a (List.fold op farg rest)
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
        | [x; List []] -> List [x]
        | [x; List xs] -> List (x :: xs)
        | [x; DottedList (xs, t)] -> DottedList (x :: xs, t)
        | [x; y] -> DottedList ([x], y)
        | _ -> ExpectedListException "cons, in second argumnet" |> raise

    let car = function
        | [List (x :: _)] -> x
        | [DottedList ((x :: _) , _)] -> x
        | _ -> ExpectedListException "car" |> raise

    let cdr = function
        | [List (_ :: xs)] -> List xs
        | [DottedList ([_], x)] -> x
        | [DottedList (_ :: xs, x)] -> DottedList (xs, x)
        | _ -> ExpectedListException "cdr" |> raise

    let list args = List args

    let display = function
        | (String s) -> printf "%s" s; Nil
        | other -> TypeMismatchException("string op", other) |> raise

    let exit = function
        | [] -> exit 0
        | [Number (Integer n)] -> exit n
        | _ -> MalformException "(exit) or (exit exitCode)" |> raise

    let mkFn (name, fn) = 
        (name, ref (Func (name, (fun cont args -> fn args |> cont))))

    let primEnv = 
        [
            ("+", binopFold (numOp (+)) (Lisp.Number (Integer 0)))
            ("*", binopFold (numOp (*)) (Lisp.Number (Integer 1)))
            ("-", numOp (-) |> binop)
            ("/", numOp (/) |> binop)
            ("string-append", binopFold (stringOp (+)) (Lisp.String ""))
            ("<", numCmp (<) |> binop)
            (">", numCmp (>) |> binop)
            (">=", numCmp (>=) |> binop)
            ("<=", numCmp (<=) |> binop)
            ("==", numCmp (=) |> binop)
            ("even?", integerBool (fun x -> x % 2 = 0) |> unop)
            ("odd?", integerBool (fun x -> x % 2 <> 0) |> unop)
            ("neg?", numBool (fun x -> x < 0.) |> unop)
            ("pos?", numBool (fun x -> x > 0.) |> unop)
            ("eq?", eqCmd |> binop)
            ("null?", (eqCmd Nil) |> unop)
            ("not", notOp |> unop)
            ("cons", cons)
            ("car", car)
            ("cdr", cdr)
            ("list", list)
            ("display", display |> unop)
            ("show", unop (showVal >> Lisp.String))
            ("exit", exit)]
        |> List.map mkFn
        |> Map.ofList

