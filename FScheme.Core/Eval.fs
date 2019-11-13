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
        | Nil | Number _ | Bool _ | Char _ | String _ as lit -> lit
        | Atom x -> (lookup env x).Value
        | Vector v -> v |> Array.map (fun item -> eval env item) |> Vector
        | List [Atom "quote"; expr] -> expr
        | List [Atom "quasiquote"; expr] -> evalQuasiquote env expr
        | List (Atom "begin" :: rest) -> evalForms env rest
        | List (Atom "define" :: rest) -> evalDefine env rest
        | List (Atom "lambda" :: rest) -> evalLambda env rest
        | List (Atom "let" :: rest)  -> evalLet env rest
        | List (Atom "if" :: rest) -> evalIf env rest

        | List [Atom "cdr"; List [Atom "quote"; List (x :: xs)]] -> List xs
        | List [Atom "cdr"; List (x :: xs)] ->
            match x with
            | Atom  _ ->
                let value = eval env (List (x :: xs))
                eval env (List [Atom "cdr"; value])
            | _           -> List xs

        | List [Atom "car"; List [Atom "quote"; List (x :: _)]] -> x
        | List [Atom "car"; List (x :: xs)] ->
            match x with
            | Atom  _ ->
                let value = eval env (List (x :: xs))
                eval env (List [Atom "car"; value])
            | _           -> x

        | List (x :: xs) ->
            match eval env x with
            | Func fn -> apply env fn xs
            | Macro macro -> macro env xs
            | x -> NotFunctionException x |> raise

        | _ -> MalformException "Malform expression" |> raise

    and lambda env (parameters: Lisp list) exprs =
        fun (args: Lisp list) ->
            if args.Length <> parameters.Length then
                NumArgsException (parameters.Length, args) |> raise
            let bindings = zipWith (fun binding value -> (extractVar binding, ref value)) parameters args
            let env' = extends env bindings
            evalForms env' exprs
        |> Func

    and apply env fn args =
        let args' = args |> List.map (eval env)
        fn args'

    and evalQuasiquote env expr =
        let rec unquote x =
            match x with
            | List [Atom "unquote"; e] -> eval env e
            | List (Atom "unquote" :: _) -> MalformException "unquote (too many args)" |> raise
            | List (Atom "quasiquote" :: _) -> x
            | List (Atom "quote" :: _) -> x
            | List lst -> List.fold (fun acc item -> (unquote item) :: acc) [] lst |> List.rev |> List
            | _ -> x
        unquote expr

    and evalDefine env = function
        | [(Atom name); value] ->
            let expr = eval env value
            updateEnv env name expr |> ignore
            expr
        | List (Atom name :: parameters) :: body ->
            let fn (args: Lisp list) =
                if args.Length <> parameters.Length then
                    NumArgsException (parameters.Length, args) |> raise
                let bindings = zipWith (fun binding value -> (extractVar binding, ref value)) parameters args
                let env' = extends env bindings
                evalForms env' body
            let expr = Func fn
            updateEnv env name expr |> ignore
            expr
        | DottedList ((Atom name :: parameters), rest) :: body ->
            let fn (args: Lisp list) =
                if args.Length < parameters.Length then
                    NumArgsException (parameters.Length, args) |> raise
                let (arg', rest') = List.splitAt parameters.Length args
                let bindings = zipWith (fun binding value -> (extractVar binding, ref value)) (rest :: parameters) (List rest' :: arg')
                let env' = extends env bindings
                evalForms env' body
            let expr = Func fn
            updateEnv env name expr |> ignore
            expr
        | _ -> MalformException "(define name <s-expr>) or (define (name params...) (s-expr))" |> raise
    
    and evalLambda env = function
        | Atom s :: body ->
            lambda env [Atom s] body
        | (List parameters) :: body ->
            List.map ensureAtom parameters |> ignore
            lambda env parameters body
        | (DottedList (parameters, rest)) :: body ->
            let fn (args: Lisp list) =
                if args.Length < parameters.Length then
                    NumArgsException (parameters.Length, args) |> raise
                let (arg', rest') = List.splitAt parameters.Length args
                let bindings = zipWith (fun binding value -> (extractVar binding, ref value)) (rest :: parameters) ((List rest') :: arg')
                let env' = extends env bindings
                evalForms env' body
            let expr = Func fn
            expr
        | _ -> 
            MalformException "lambda function expects list of parameters and S-Expression body\n(lambda <params> <s-expr>)"
            |> raise

    and evalIf env = function
        | [pred; trueExpr; falseExpr] ->
            if testTrue (eval env pred) then
                eval env trueExpr
            else eval env falseExpr
        | _ -> MalformException "(if <expr> <s-expr> <s-expr>)" |> raise

    and evalLet env = function
        | List (pairs : Lisp list) :: exprs ->
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
            evalForms env' exprs
        | _ ->
            MalformException "let function expects list of parameters and S-Expression body\n(let <pairs> <s-expr>)"
            |> raise

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