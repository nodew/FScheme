namespace FScheme.Core

type Number =
    | Float of float
    | Integer of int32

[<CustomEquality; NoComparison>]
type Lisp =
    | Nil
    | Bool of bool
    | Number of Number
    | Atom of string
    | Char of char
    | String of string
    | Func of IFunc
    | Macro of IMacro
    | List of Lisp list
    | DottedList of head: Lisp list * tail: Lisp
    | Continuation of Continuation
    | Vector of Lisp array
    override x.Equals other =
        let otherType = other.GetType()
        let lispType = typeof<Lisp>
        if (otherType = lispType || otherType.BaseType = lispType) then
            let y = other :?> Lisp
            match x, y with
            | Nil, Nil -> true
            | Bool a, Bool b -> a = b
            | Atom a, Atom b -> a = b
            | Char a, Char b -> a = b
            | Number a, Number b -> a = b
            | String a, String b -> a = b
            | Vector a, Vector b -> a = b
            | List a, List b -> a = b
            | DottedList (h1, t1), DottedList (h2, t2) -> h1 = h2 && t1 = t2
            | _, _ -> false
        else
            false
    override x.GetHashCode () = hash x

and Application = Lisp list

and Continuation = Lisp -> Lisp

and IFunc = Continuation -> Lisp list -> Lisp

and IMacro = (Environment -> (Lisp list) -> Lisp)

and Frame = Map<string, Lisp ref> ref

and Environment = Frame list

/// exceptions
exception TypeMismatchException of string * Lisp
exception NumArgsException of int * Lisp list
exception UnboundedVarException of string
exception VarHasBeenBoundedException of string
exception MalformException of string
exception NotFunctionException of Lisp
exception ExpectedListException of string
exception PErrorException of string

[<AutoOpen>]
module lispVal =
    let numToFloat = function
        | Integer a -> float a
        | Float a -> a

    let private unwords = String.concat " "

    let escape s =
        s 
        |> Seq.toList 
        |> List.map (fun c -> 
                        match c with
                        | '\n' -> "\\\n"
                        | '\r' -> "\\\r"
                        | '\t' -> "\\\t"
                        | '\b' -> "\\\b"
                        | '\a' -> "\\\a"
                        | a -> sprintf "%c" a)
        |> Seq.ofList
        |> String.concat ""

    let rec printExpr = function
        | Nil -> "'()"
        | Bool true -> "#t"
        | Bool false -> "#f"
        | Char c -> sprintf @"#\%c" c
        | Number (Integer n) -> string(n)
        | Number (Float n) -> string(n)
        | Atom atom -> atom
        | String s -> sprintf "\"%s\"" (escape s)
        | DottedList (head, tail) -> unwordsList head |> fun h -> sprintf "'(%s . %s)" h (printExpr tail)
        | List lst -> unwordsList lst |> sprintf "'(%s)"
        | Vector v -> v |> Seq.toList |> unwordsList |> sprintf "#(%s)"
        | Func _         -> "#<procedure>"
        | Macro _        -> "#<macro>"
        | Continuation _ -> "#<continuation>"

    and private unwordsList lst = lst |> List.map printExpr |> unwords

    and printApp (app: Application) = app |> List.map printExpr |> String.concat "\n"

    and showError = function
        | NumArgsException (n, args) -> args |> unwordsList |> sprintf "Error Number Arguments, expected %d, received args %s" n
        | TypeMismatchException (txt, var) -> var |> printExpr |> sprintf "Error Type Mismatch: %s %s" txt
        | UnboundedVarException var -> var |> sprintf "Error Unbounded variable: %s"
        | VarHasBeenBoundedException var -> var |> sprintf "Error variable has been defined: %s"
        | MalformException s -> s |> sprintf "Error Bad Special Form: %s"
        | NotFunctionException var -> var |> printExpr |> sprintf "Error Not a Function: %s"
        | ExpectedListException s -> sprintf "Error Expected List in funciton %s" s
        | PErrorException s -> sprintf "Parser Error, expression cannot evaluate: %s" s
        | ex -> sprintf "Unexpected internal error: %s" (ex.ToString())
