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
    | String of string
    | Func of IFunc
    | List of Lisp list
    override x.Equals other =
        let otherType = other.GetType()
        let lispType = typeof<Lisp>
        if (otherType = lispType || otherType.BaseType = lispType) then
            let y = other :?> Lisp
            match x, y with
            | Nil, Nil -> true
            | Bool a, Bool b -> a = b
            | Atom a, Atom b -> a = b
            | Number a, Number b -> a = b
            | String a, String b -> a = b
            | List a, List b -> List.zip a b |> List.forall (fun (a, b) -> a = b)
            | _, _ -> false
        else
            false
    override x.GetHashCode () = hash x

and Application = Lisp list

and IFunc = Lisp list -> Lisp

and Frame = Map<string, Lisp ref> ref

and Environment = Frame list

/// exceptions
exception TypeMismatchException of string * Lisp
exception NumArgsException of int * Lisp list
exception UnboundedVarException of string
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

    let rec printExpr = function
        | Nil -> "'()"
        | Bool true -> "#t"
        | Bool false -> "#f"
        | Number (Integer n) -> string(n)
        | Number (Float n) -> string(n)
        | Atom atom -> atom
        | String s -> sprintf "\"%s\"" s
        | List lst -> unwordsList lst |> sprintf "(%s)"
        | Func _         -> "(internal function)"

    and private unwordsList lst = lst |> List.map printExpr |> unwords

    and printApp (app: Application) = app |> List.map printExpr |> String.concat "\n"

    and showError = function
        | NumArgsException (n, args) -> args |> unwordsList |> sprintf "Error Number Arguments, expected %d, received args %s" n
        | TypeMismatchException (txt, var) -> var |> printExpr |> sprintf "Error Type Mismatch: %s %s" txt
        | UnboundedVarException var -> var |> sprintf "Error Unbounded variable: %s"
        | MalformException s -> s |> sprintf "Error Bad Special Form: %s"
        | NotFunctionException var -> var |> printExpr |> sprintf "Error Not a Function: %s"
        | ExpectedListException s -> sprintf "Error Expected List in funciton %s" s
        | PErrorException s -> sprintf "Parser Error, expression cannot evaluate: %s" s
        | ex -> sprintf "Unexpected internal error: %s" (ex.ToString())
