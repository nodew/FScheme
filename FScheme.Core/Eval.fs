namespace FScheme.Core

open System.IO
open System.Reflection
open System.Text
open Microsoft.Extensions.FileProviders
open FParsec

module Eval =
    let environment = [ref Primitives.primEnv]

    let extends env bindings = ref (Map.ofList bindings) :: env

    let lookup (env: Environment) key =
        match List.tryPick (fun (frame: Frame) -> Map.tryFind key frame.Value) env with
        | Some value -> value
        | _ -> UnboundedVarException key |> raise

    let updateEnv (env: Environment) key value =
        if env.Head.Value.ContainsKey(key) then
            VarHasBeenBoundedException key |> raise
        else
            env.Head := Map.add key (ref value) env.Head.Value

    let ensureAtom = function
        | Atom atom -> Atom atom
        | n -> TypeMismatchException ("expected an atomic value", n) |> raise

    let extractVar = function
        | (Atom atom) -> atom
        | n -> TypeMismatchException ("expected an atomic value", n) |> raise

    let testTrue = function
        | Bool false -> false
        | String "" -> false
        | Number (Integer 0) -> false
        | Number (Float 0.) -> false
        | Nil -> false
        | _ -> true

    let rec zipWith op a b =
        match a, b with
        | [], _ -> []
        | _, [] -> []
        | (a1 :: aa), (b1 :: bb) -> op a1 b1 :: (zipWith op aa bb)

    let rec eval (env: Environment) expr =
        match expr with
        | Nil -> Nil
        | Number x -> Number x
        | Bool bool -> Bool bool
        | String s -> Lisp.String s
        | Atom x -> (lookup env x).Value
        | List [Atom "quote"; expr] ->
            let rec unquote = function
                | List [Atom "unquote"; e] -> eval env e
                | List (Atom "unquote" :: _) -> MalformException "unquote (too many args)" |> raise
                | List lst -> List.fold (fun acc item -> (unquote item) :: acc) [] lst |> List.rev |> List
                | e -> e
            unquote expr

        | List [Atom "begin"; rest] -> evalForms env [rest]
        | List (Atom "begin" :: rest) -> evalForms env rest

        | List [Atom "define"; Atom name; defExpr] ->
            let expr = eval env defExpr
            updateEnv env name expr |> ignore
            eval env (Atom name)
        | List (Atom "define" :: _) ->
            MalformException "(define name <s-expr>)" |> raise

        | List [Atom "defun"; Atom name; List parameters; defExpr] ->
            parameters |> List.map ensureAtom |> ignore
            let expr = lambda env parameters defExpr |> Func
            updateEnv env name expr
            expr
        | List (Atom "defun" :: _) ->
            MalformException "(defun name (parameters) <s-expr>)" |> raise

        | List [Atom "defmacro"; Atom name; List parameters; defExpr] ->
            parameters |> List.map ensureAtom |> ignore
            let macro =
                fun env' args ->
                    let bindings = zipWith (fun binding value -> (extractVar binding, ref value)) parameters args
                    let env'' = extends env bindings
                    eval env' (eval env'' defExpr)
                |> Macro
            updateEnv env name macro
            macro

        | List (Atom "defmacro" :: _) ->
            MalformException "(defmacro name (parameters) <s-expr>)"
            |> raise

        | List [Atom "let"; List (pairs : Lisp list); expr] ->
            let (atoms, vals) =
                pairs
                |> List.fold (fun (atoms, vals) pair ->
                    match pair with
                    | List [binding; value] -> ((extractVar binding) :: atoms, ref (eval env value) :: vals)
                    | _ -> MalformException "let function expects list of parameters\n(let ((atom value) ...) <s-expr>)"
                           |> raise
                ) ([], [])
            let bindings = List.zip atoms vals
            let env' = extends env bindings
            eval env' expr
        | List (Atom "let" :: _) ->
            MalformException "let function expects list of parameters and S-Expression body\n(let <pairs> <s-expr>)"
            |> raise

        | List [Atom "lambda"; List parameters; expr] ->
            parameters |> List.map ensureAtom |> ignore
            lambda env parameters expr |> Lambda

        | List (Atom "lambda" :: _) ->
            MalformException "lambda function expects list of parameters and S-Expression body\n(lambda <params> <s-expr>)"
            |> raise

        | List [Atom "if"; pred; trueExpr; falseExpr] ->
            if testTrue (eval env pred) then
                eval env trueExpr
            else eval env falseExpr
        | List (Atom "if" :: _) -> MalformException "(if <expr> <s-expr> <s-expr>)" |> raise

        | List [Atom "cdr"; List [Atom "quote"; List (x :: xs)]] -> List xs
        | List [Atom "cdr"; List (x :: xs)] ->
            match x with
            | Atom  _ ->
                let value = eval env (List (x :: xs))
                eval env (List [Atom "cdr"; value])
            | _           -> List xs

        | List [Atom "car"; List [Atom "quote"; List (x :: xs)]] -> x
        | List [Atom "car"; List (x :: xs)] ->
            match x with
            | Atom  _ ->
                let value = eval env (List (x :: xs))
                eval env (List [Atom "car"; value])
            | _           -> x

        | List (x :: xs) ->
            match eval env x with
            | Func fn -> apply env fn xs
            | Lambda fn -> apply env fn xs
            | Macro macro -> macro env xs
            | x -> NotFunctionException x |> raise

        | _ -> MalformException "Malform expression" |> raise

    and lambda env parameters expr =
        fun args ->
            let bindings = zipWith (fun binding value -> (extractVar binding, ref value)) parameters args
            let env' = extends env bindings
            eval env' expr

    and apply env fn args =
        let args' = args |> List.map (eval env)
        fn args'

    and evalForms (env: Environment) (forms: Lisp list) =
        match forms with
        | [] -> Nil
        | [x] -> eval env x
        | x :: xs ->
            eval env x |> ignore
            evalForms env xs

    let evalSource source =
        try
            let ast = Parser.readContent source
            evalForms environment ast
        with ex -> showError ex |> failwith

    let private getStdLibContent () =
        let assembly = (typeof<Lisp>).GetTypeInfo().Assembly;
        let embeddedProvider = EmbeddedFileProvider(assembly, "FScheme.Core");
        let file = embeddedProvider.GetFileInfo(@"lib\stdlib.scm");
        let stream = file.CreateReadStream();
        use reader = new StreamReader(stream, Encoding.UTF8)
        reader.ReadToEnd()

    let loadStdEnv () =
        let stdlibContent = getStdLibContent ()
        let stdLibExprs = Parser.readContent stdlibContent
        evalForms environment stdLibExprs |> ignore

    loadStdEnv ()

    let getFileContent (filepath: string) =
        if File.Exists(filepath) then
            File.ReadAllText(filepath)
        else
            failwith "File does not exist."

    let evalFile filepath =
        let source = getFileContent filepath
        evalSource source

    let evalText source =
        evalSource source

    let evalExpr env source =
        let expr = Parser.readExpr source
        evalForms env [expr]