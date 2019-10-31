namespace FScheme.Core

open System
open System.IO
open System.Reflection
open System.Text
open Microsoft.Extensions.FileProviders
open FParsec

module Eval =
    let private getStdLibContent () =
        let assembly = (typeof<Lisp>).GetTypeInfo().Assembly;
        let embeddedProvider = EmbeddedFileProvider(assembly, "FScheme.Core");
        let file = embeddedProvider.GetFileInfo(@"lib\stdlib.scm");
        let stream = file.CreateReadStream();
        use reader = new StreamReader(stream, Encoding.UTF8)
        reader.ReadToEnd()

    let getFileContent (filepath: string) =
        if File.Exists(filepath) then
            File.ReadAllText(filepath)
        else
            failwith "File does not exist."

    let basicFunEnv =
        Primitives.primEnv.
            Add("show", Primitives.unop (print >> Lisp.Text) |> Func)

    let defaultEnv = {
        funCtx = basicFunEnv;
        varCtx = Map.empty
    }

    let getVar env key =
        let mutable value = Nil
        if env.funCtx.TryGetValue(key, &value) then
            value
        elif env.varCtx.TryGetValue(key, &value) then
            value
        else
            UnboundedVar key |> throwException

    let isLambda = function
        | List (Atom "lambda" :: _) -> true
        | _  -> false

    let ensureAtom = function
        | Atom atom -> Atom atom
        | n -> TypeMismatch ("expected an atomic value", n) |> throwException

    let extractVar = function
        | (Atom atom) -> atom
        | n -> TypeMismatch ("expected an atomic value", n) |> throwException

    let rec getEven = function
        | [] -> []
        | x :: xs -> x :: getOdd xs
    and getOdd = function
        | [] -> []
        | _ :: xs -> getEven xs

    let rec zipWith op a b =
        match a, b with
        | [], _ -> []
        | _, [] -> []
        | (a1 :: aa), (b1 :: bb) -> op a1 b1 :: (zipWith op aa bb)

    let updateEnv (env: EnvCtx) var e =
        match e with
        | Func x   -> { env with funCtx = env.funCtx.Add(var, e)}
        | Lambda _ -> { env with funCtx = env.funCtx.Add(var, e)}
        | _        -> { env with varCtx = env.varCtx.Add(var, e)}

    let rec eval (env: EnvCtx) expr =
        match expr with
        | Nil -> Nil
        | Number x -> Number x
        | Bool bool -> Bool bool
        | Text s -> Lisp.Text s
        | Atom x -> getVar env x
        | List [Atom "quote"; expr] -> expr

        | List [Atom "begin"; rest] -> fst (evalForms env [rest])
        | List (Atom "begin" :: rest) -> fst (evalForms env rest)

        | List [Atom "define"; varExpr; defExpr] ->
            let _ = ensureAtom varExpr
            let _ = eval env defExpr
            bindArgsEval env [varExpr] [defExpr] varExpr

        | List [Atom "let"; List (pairs : Lisp list); expr] ->
            let (atoms, vals) =
                pairs
                |> List.fold (fun (atoms, vals) pair ->
                    match pair with
                    | List [binding; value] -> (ensureAtom binding :: atoms, eval env value :: vals)
                    | _ -> BadSpecialForm "let function expects list of parameters\n(let ((atom value) ...) <s-expr>)"
                           |> throwException
                ) ([], [])
            bindArgsEval env atoms vals expr
        | List (Atom "let" :: _) ->
            BadSpecialForm "let function expects list of parameters and S-Expression body\n(let <pairs> <s-expr>)"
            |> throwException

        | List [Atom "lambda"; List parameters; expr] ->
            Lambda (env, applyLambda env expr parameters)
        | List (Atom "lambda" :: _) ->
            BadSpecialForm "lambda function expects list of parameters and S-Expression body\n(lambda <params> <s-expr>)"
            |> throwException

        | List [Atom "if"; pred; trueExpr; falseExpr] ->
            match eval env pred with
            | Bool true -> eval env trueExpr
            | Bool false -> eval env falseExpr
            | _ -> BadSpecialForm "if's first arg must eval into a boolean" |> throwException
        | List (Atom "if" :: _) -> BadSpecialForm "(if <bool> <s-expr> <s-expr>)" |> throwException

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
            let funVar = eval env x
            let args = xs |> List.map (eval env)
            match funVar with
            | Func funcall -> funcall args
            | Lambda (beforeEnv, funcall) -> eval beforeEnv (funcall args)
            | _ -> NotFunction funVar |> throwException

        | _ -> failwith "Unsupport pattern"

    and bindArgsEval env parameters args expr =
        let newVars = zipWith (fun a b -> (extractVar a, b)) parameters args
        let (newEnv, newFenv) = newVars |> List.partition (fun (a, b) -> isLambda b |> not)
        let finalVarCtx = newEnv |> List.fold (fun (s: VarCtx) (key, value) -> s.Add(key, value)) env.varCtx
        let finalFunCtx = newFenv |> List.fold (fun (s: FunCtx) (key, value) -> s.Add(key, value)) env.funCtx
        let finalCtx = { varCtx = finalVarCtx; funCtx = finalFunCtx }
        eval finalCtx expr

    and applyLambda (env: EnvCtx) (expr: Lisp) (parameters: Lisp list) (args: Lisp list) = bindArgsEval env parameters args expr

    and evalForms (env: EnvCtx) (forms: Lisp list) =
        match forms with
        | (List [Atom "define"; Atom var; defExpr]) :: rest ->
            let evalVal = eval env defExpr
            let newEnv = updateEnv env var evalVal
            evalForms newEnv rest
        | [] -> (Nil, env)
        | [x] -> (eval env x, env)
        | (x::xs) ->
            eval env x |> ignore
            evalForms env xs

    let evalSource env source =
        let ast = Parser.readContent source
        evalForms env ast

    let getEnvWithStdLib () =
        let stdlibContent = getStdLibContent ()
        let stdLibExprs = Parser.readContent stdlibContent
        let (_, env) = evalForms defaultEnv stdLibExprs
        env

    let envWithStd = lazy (getEnvWithStdLib ())

    let safeExec env source =
        try
            Some(evalSource env source)
        with
        | LispException ex ->
            showError ex |> printfn "%s"
            None
        | ex ->
            printfn "Unexpected internal error: %s" (ex.ToString())
            None

    let evalFile filepath =
        let source = getFileContent filepath
        let env = envWithStd.Value
        evalSource env source

    let evalText source =
        let env = envWithStd.Value
        evalSource env source
