namespace FScheme.Core

module Eval =
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
        | Text s -> Text s
        | Atom x -> getVar env x
        | List [Atom "quote"; expr] -> expr

        | List [Atom "begin"; rest] -> evalForms env [rest]
        | List (Atom "begin" :: rest) -> evalForms env rest

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
        | x :: [] -> eval env x
        | (List [Atom "define"; Atom var; defExpr]) :: rest ->
            let evalVal = eval env defExpr
            let newEnv = updateEnv env var evalVal
            evalForms newEnv rest
        | _ -> failwith "Unexpected parameters"
