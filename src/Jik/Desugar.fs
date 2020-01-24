module Desugar

open SExpr
open Common

type S = SExpr

let special = [
    "lambda"
    "let"
    "let*"
    "letrec"
    "letrec*"
    "and"
    "or"
    "cond"
    "else"
    "when"
    "unless"
]

let undefinedExpr = S.Bool false

let isUnused s env = List.contains s env |> not

let wrapInLet var value body =
    exprsToList [S.Symbol "let"
                 exprsToList [exprsToList [var; value]]
                 body]

let rec desugar2 env sexpr =
    match sexpr with
    | List (Symbol s :: rest) when (List.contains s special) && (List.contains s env |> not) ->
        expand env sexpr
    | List sexprs ->
        exprsToList (List.map (desugar2 env) sexprs)
    | _ -> sexpr

and expand env sexpr =
    let desugar = desugar2 env
    match sexpr with
    | List(Symbol "lambda" :: Symbol arg :: body) ->
        let env = arg :: env
        let body = List.map (desugar2 env) body
        exprsToList (Symbol "lambda" :: Symbol arg :: body)
    | List(Symbol "lambda" :: List args :: body) ->
        let argsString = symbolsToStrings args
        let env = argsString @ env
        let body = List.map (desugar2 env) body
        exprsToList (Symbol "lambda" :: exprsToList args :: body)
    | List(Symbol "lambda" :: ListImproper args :: body) ->
        let argsString = symbolsToStrings args
        let env = argsString @ env
        let body = List.map (desugar2 env) body
        let argsRevert = exprsToDottedList args
        SExpr.Cons(Symbol "lambda", SExpr.Cons(argsRevert, exprsToList body))
    | List(Symbol "let" :: List bindings :: body) ->
        if List.isEmpty body then
            failwithf "let: body should not be empty\n%s\n" (sexprToString sexpr)

        let folder sexpr (names, initExprs) =
            match sexpr with
            | List [Symbol name; initExpr] ->
                name :: names, initExpr :: initExprs
            | _ -> failwith "sexprToExpr: let: wrong bindings"

        let names, initExprs = List.foldBack folder bindings ([], [])
        let args = List.map Symbol names
        let lam = exprsToList (Symbol "lambda" :: exprsToList args :: body)
        exprsToList (lam :: initExprs)
        |> desugar2 env
    | List(Symbol "let" :: Symbol name :: List bindings :: body) ->
        let bindings = List.map (function | List [Symbol name; sexpr] -> Symbol name, sexpr | _ -> failwith "wrong binding") bindings
        let names, initExprs = List.unzip bindings
        let lambda = exprsToList (Symbol "lambda" :: exprsToList names :: body)
        let call = exprsToList (Symbol name :: initExprs)
        let binding = exprsToList [exprsToList [Symbol name; lambda]]
        let letrec = exprsToList [Symbol "letrec"; binding; call]
        desugar letrec
    | List(Symbol "letrec" :: List bindings :: body) ->
        let folder sexpr (bindings) =
            match sexpr with
            | List [Symbol name; initExpr] ->
                (name, initExpr) :: bindings
            | _ -> failwith "sexprToExpr: let: wrong bindings"

        let bindings = List.foldBack folder bindings []
        let names = List.map fst bindings
        let bindInitial name = exprsToList [Symbol name; Number 0]
        let letBindings =
            names
            |> List.map bindInitial
            |> exprsToList
        let body = List.foldBack (fun (name, expr) acc ->
            exprsToList [Symbol "set!"; Symbol name; expr] :: acc) bindings body
        exprsToList (Symbol "let" :: letBindings :: body)
        |> desugar
    | List [S.Symbol "and"] -> S.Bool true
    | List [S.Symbol "and"; a] ->
        let a = desugar2 env a
        exprsToList [S.Symbol "if"; a; a; S.Bool false]
    | List (S.Symbol "and" :: a :: rest) ->
        let a = desugar a
        let rest = exprsToList (S.Symbol "and" :: rest)
        exprsToList [S.Symbol "if"; a; desugar rest; S.Bool false]
    | List [S.Symbol "or"] -> S.Bool false
    | List [S.Symbol "or"; a] ->
        desugar a
    | List (S.Symbol "or" :: a :: rest) ->
        let a = desugar a
        let t = S.Symbol (freshLabel "t")
        let rest =
            exprsToList (S.Symbol "or" :: rest)
            |> desugar
        exprsToList [S.Symbol "if"; t; t; rest]
        |> wrapInLet t a
        |> expand env
    | List (S.Symbol "when" :: condition :: body) ->
        if List.isEmpty body then
            failwith "unless body should not be empty"

        let condition = desugar condition
        let body = exprsToList (S.Symbol "begin" :: List.map desugar body)
        exprsToList [S.Symbol "if"; condition; body; undefinedExpr]
    | List (S.Symbol "unless" :: condition :: body) ->
        if List.isEmpty body then
            failwith "unless body should not be empty"

        let condition = desugar condition
        let body = exprsToList (S.Symbol "begin" :: List.map desugar body)
        exprsToList [S.Symbol "if"; condition; undefinedExpr; body]
    | List [S.Symbol "cond"] ->
        undefinedExpr
    | List [S.Symbol "cond"; List [S.Symbol "else"; expr]] when isUnused "else" env ->
        desugar expr
    | List (S.Symbol "cond" :: List [clause] :: rest) ->
        let clause = desugar clause
        let t = S.Symbol (freshLabel "t")
        let rest = (exprsToList (S.Symbol "cond" :: rest))
        wrapInLet t clause (exprsToList [S.Symbol "if"; t; t; rest])
        |> expand env
    | List (S.Symbol "cond" :: List [condition; S.Symbol "=>"; conseq] :: rest) when isUnused "=>" env ->
        let condition = desugar condition
        let conseq = desugar conseq
        let t = S.Symbol (freshLabel "t")
        let t2 = S.Symbol (freshLabel "t2")
        let rest = desugar (exprsToList (S.Symbol "cond" :: rest))
        wrapInLet t2 conseq (exprsToList [S.Symbol "if"; t; exprsToList [t2; t]; rest])
        |> expand env
        |> wrapInLet t condition
        |> expand env
    | List (S.Symbol "cond" :: List (condition :: conseq) :: rest) ->
        let condition = desugar condition
        let conseq = exprsToList (S.Symbol "begin" :: List.map desugar conseq)
        let rest = desugar (exprsToList (S.Symbol "cond" :: rest))
        exprsToList [S.Symbol "if"; condition; conseq; rest]
    | _ ->
        printfn "Oh no! %A" sexpr
        sexpr