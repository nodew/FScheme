namespace FScheme.Core

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

    let rec eval (cont: Continuation) (env: Environment) expr =
        match expr with
        | Nil | Number _ | Bool _ | Char _ | String _ as lit -> lit |> cont
        | Atom x -> (lookup env x).Value |> cont
        | Vector v -> v |> List.ofArray |> evalFoldList (fun x -> x |> Array.ofList |> Vector |> cont) env
        | List [Atom "quote"; expr] -> expr |> cont
        | List [Atom "quasiquote"; expr] -> evalQuasiquote cont env expr
        | List (Atom "begin" :: rest) -> evalForms cont env rest
        | List (Atom ("set!") :: rest) -> evalSet cont env rest
        | List (Atom "define" :: rest) -> evalDefine cont env rest
        | List (Atom "lambda" :: rest) -> evalLambda cont env rest
        | List (Atom "let" :: rest)  -> evalLet cont env rest
        | List (Atom "if" :: rest) -> evalIf cont env rest
        | List (Atom "call/cc":: rest) -> evalCallCC cont env rest
        | List [Atom "apply"; fn; List args] -> apply cont env fn args
        | List [Atom "eval"; expr] -> eval (fun expr' -> eval cont env expr') env expr
        | List [Atom "load"; filename] -> 
            match filename with 
            | String filename' -> load filename' |> cont 
            | _ -> MalformException "load" |> raise
        | List (fn :: xs) -> apply cont env fn xs
        | _ -> MalformException "Malform expression" |> raise

    and evalFoldList cont env (lst: Lisp list) = 
        let rec fold' (acc: Lisp list) = function
        | h :: t -> eval (fun x -> fold' (x :: acc) t) env h
        | [] -> acc |> List.rev |> cont
        fold' [] lst

    and lambda env (parameters: Lisp list) body =
        fun cont (args: Lisp list) ->
            if args.Length <> parameters.Length then
                NumArgsException (parameters.Length, args) |> raise
            bindArgsEval cont env parameters args body

    and lambda2 env (parameters: Lisp list) extra body =
        fun cont (args: Lisp list) ->
            if args.Length < parameters.Length then
                NumArgsException (parameters.Length, args) |> raise
            let (args', extra') = List.splitAt parameters.Length args
            bindArgsEval cont env (extra :: parameters) (List extra' :: args') body
 
    and bindArgsEval cont env parameters args body = 
        let bindings = zipWith (fun binding value -> (extractVar binding, ref value)) parameters args
        let env' = extends env bindings
        evalForms cont env' body

    and apply cont env fn args =
        let cont' = function
        | Func (_, f) | Lambda f -> 
            let cont' args' = f cont args'
            evalFoldList cont' env args
        | Continuation cont -> 
            match args with 
            | [] -> cont Nil
            | [rtn] -> eval cont env rtn
            | _ -> MalformException "call/cc (too many args)" |> raise
        | Macro macro -> macro env args
        | x -> NotFunctionException x |> raise
        eval cont' env fn

    and evalQuasiquote cont env expr =
        let rec unquote cont' x =
            match x with
            | List [Atom "unquote"; e] -> eval cont' env e
            | List (Atom "unquote" :: _) -> MalformException "unquote (too many args)" |> raise
            | List (Atom "quasiquote" :: _) -> x |> cont'
            | List [] -> Nil |> cont'
            | List lst -> 
                let rec mapunquote acc = function
                | h' :: t' ->
                    unquote (fun x -> mapunquote (x :: acc) t') h'
                | [] -> List(List.rev acc)
                mapunquote [] lst |> cont'
            | _ -> x |> cont'
        unquote cont expr

    and evalSet cont env = function
        | [Atom name; expr] -> eval (fun x -> (lookup env name) := x; Nil |> cont) env expr
        | _ -> MalformException "set!" |> raise

    and evalDefine cont env = function
        | [(Atom name); value] -> eval (fun x -> updateEnv env name x; Nil |> cont) env value
        | List (Atom name :: parameters) :: body ->
            let expr = Func (name, lambda env parameters body)
            updateEnv env name expr |> ignore
            expr |> cont
        | DottedList ((Atom name :: parameters), extra) :: body ->
            let expr = Func (name, lambda2 env parameters extra body)
            updateEnv env name expr |> ignore
            expr |> cont
        | _ -> MalformException "(define name <s-expr>) or (define (name params...) (s-expr))" |> raise
    
    and evalLambda cont env = function
        | Atom s :: body ->
            lambda env [Atom s] body |> Lambda |> cont
        | (List parameters) :: body ->
            List.map ensureAtom parameters |> ignore
            lambda env parameters body |> Lambda |> cont
        | (DottedList (parameters, extra)) :: body ->
            lambda2 env parameters extra body |> Lambda |> cont
        | _ -> 
            MalformException "lambda function expects list of parameters and S-Expression body\n(lambda <params> <s-expr>)"
            |> raise

    and evalIf cont env = function
        | [cond; trueExpr; falseExpr] ->
            let cont' x =
                if testTrue x then
                    eval cont env trueExpr
                else eval cont env falseExpr
            eval cont' env cond
        | _ -> MalformException "(if <expr> <s-expr> <s-expr>)" |> raise

    and evalLet cont env = function
        | List (pairs : Lisp list) :: body ->
            let rec mapbind acc = function
                | List([Atom(s); e]) :: t -> eval (fun x -> mapbind ((s, ref x) :: acc) t) env e
                | [] ->
                    let frame = List.rev acc
                    let env' = extends env frame
                    evalForms cont env' body
                | _ -> failwith "Malformed 'let' binding."
            mapbind [] pairs
        | _ ->
            MalformException "let function expects list of parameters and S-Expression body\n(let <pairs> <s-expr>)"
            |> raise
    
    and evalCallCC cont env = function
    | [callee] ->
        let cont' = function
            | Lambda fn -> fn cont [Continuation(cont)]
            | m -> MalformException "call/cc" |> raise
        eval cont' env callee
    | _ -> MalformException "call/cc" |> raise

    and evalForms cont (env: Environment) (forms: Lisp list) =
        let rec fold' prev = function
        | h :: t -> eval (fun x -> fold' x t) env h
        | [] -> prev |> cont
        fold' Nil forms

    and load filename =
        let content = loadFileContent filename
        let stdLibExprs = Parser.readContent content
        evalForms id environment stdLibExprs

    let evalSource source =
        try
            let ast = Parser.readContent source
            evalForms id environment ast
        with ex -> showError ex |> failwith

    let evalFile filepath =
        let source = getFileContent filepath
        evalSource source

    let evalExpr env source =
        let expr = Parser.readExpr source
        evalForms id env [expr]