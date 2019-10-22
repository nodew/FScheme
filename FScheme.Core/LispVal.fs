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
    | Text of string
    | Func of IFunc
    | Lambda of EnvCtx * IFunc
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
            | Text a, Text b -> a = b
            | List a, List b -> List.zip a b |> List.forall (fun (a, b) -> a = b)
            | _, _ -> false
        else 
            false
    override x.GetHashCode () = hash x

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
    | ExpectedList of string

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
        | ExpectedList s -> sprintf "Error Expected List in funciton %s" s

    and throwException e = showError e |> failwith