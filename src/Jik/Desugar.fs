module rec Desugar

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

let undefinedExpr = Bool false

let symbLambda = Symbol "lambda"

let isUnused s env = List.contains s env |> not

let wrapInLet var value body =
    exprsToList [Symbol "let"
                 exprsToList [exprsToList [var; value]]
                 body]

let desugar expr =
    ()

let rec desugar2 env sexpr =
    match sexpr with
    | List (Symbol s :: rest) when (List.contains s special) && (List.contains s env |> not) ->
        expand env sexpr
    | List sexprs ->
        exprsToList (List.map (desugar2 env) sexprs)
    | _ -> sexpr

and expand env expr =
    let desugar = desugar2 env
    match expr with
    | Cons(Symbol "quote", rest) -> expr
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
    | List(Symbol "lambda" :: args :: body) ->
        desugarLambda env args body
    | List(Symbol "let" :: List bindings :: body) ->
        desugarLet env expr bindings body
    | List(Symbol "let" :: Symbol name :: List bindings :: body) ->
        desugarLetLoop env name bindings body
    | List(Symbol "letrec" :: List bindings :: body) ->
        desugarLetrec env bindings body
    | List(Symbol "and" :: args) -> desugarAnd env args
    | List(Symbol "or" :: args) -> desugarOr env args
    | List (Symbol "when" :: condition :: body) ->
        if List.isEmpty body then
            failwith "unless body should not be empty"

        let condition = desugar condition
        let body = exprsToList (Symbol "begin" :: List.map desugar body)
        exprsToList [Symbol "if"; condition; body; undefinedExpr]
    | List (Symbol "unless" :: condition :: body) ->
        if List.isEmpty body then
            failwith "unless body should not be empty"

        let condition = desugar condition
        let body = exprsToList (Symbol "begin" :: List.map desugar body)
        exprsToList [Symbol "if"; condition; undefinedExpr; body]
    | List [Symbol "cond"] ->
        undefinedExpr
    | List [Symbol "cond"; List [Symbol "else"; expr]] when isUnused "else" env ->
        desugar expr
    | List (Symbol "cond" :: List [clause] :: rest) ->
        let clause = desugar clause
        let t = Symbol (freshLabel "t")
        let rest = (exprsToList (Symbol "cond" :: rest))
        wrapInLet t clause (exprsToList [Symbol "if"; t; t; rest])
        |> expand env
    | List (Symbol "cond" :: List [condition; Symbol "=>"; conseq] :: rest) when isUnused "=>" env ->
        let condition = desugar condition
        let conseq = desugar conseq
        let t = Symbol (freshLabel "t")
        let t2 = Symbol (freshLabel "t2")
        let rest = desugar (exprsToList (Symbol "cond" :: rest))
        wrapInLet t2 conseq (exprsToList [Symbol "if"; t; exprsToList [t2; t]; rest])
        |> expand env
        |> wrapInLet t condition
        |> expand env
    | List (Symbol "cond" :: List (condition :: conseq) :: rest) ->
        let condition = desugar condition
        let conseq = exprsToList (Symbol "begin" :: List.map desugar conseq)
        let rest = desugar (exprsToList (Symbol "cond" :: rest))
        exprsToList [Symbol "if"; condition; conseq; rest]
    | _ ->
        printfn "Oh no! %A" expr
        expr

let getArgsList args =
    match args with
    | Symbol arg -> [arg]
    | List args -> symbolsToStrings args
    | ListImproper args -> symbolsToStrings args
    | _ -> failwithf "wrong args list: %s" (sexprToString args)

let desugarLambda env args body =
    let env' = getArgsList args @ env
    let body' = desugarMany env' body
    exprsToList (symbLambda :: args :: body')

let desugarAnd env exprs =
    let handle expr desugared =
        let expr = desugar2 env expr
        match desugared with
        | Some(prev) ->
            Some(exprsToList [Symbol "if"; expr; prev; Bool(false)])
        | None -> Some(expr)

    List.foldBack handle exprs None
    |> Option.defaultValue (Bool true)

let desugarOr env exprs =
    let handle expr desugared =
        let expr = desugar2 env expr
        match desugared with
        | Some(prev) ->
            let t = Symbol (freshLabel "or")
            let ifExpr = exprsToList [Symbol "if"; t; t; prev]
            let lam = exprsToList [symbLambda; exprsToList [t]; ifExpr]
            Some (exprsToList [lam; expr])
        | None -> Some(expr)

    List.foldBack handle exprs None
    |> Option.defaultValue (Bool false)

let desugarLet env sexpr bindings body =
    if List.isEmpty body then
            failwithf "let: body should not be empty\n%s\n" (sexprToString sexpr)

    let getBindings sexpr (names, initExprs) =
        match sexpr with
        | List [Symbol name; initExpr] ->
            name :: names, initExpr :: initExprs
        | _ -> failwith "sexprToExpr: let: wrong bindings"

    let names, initExprs = List.foldBack getBindings bindings ([], [])
    let args = List.map Symbol names
    let lam = exprsToList (Symbol "lambda" :: exprsToList args :: body)
    let app = exprsToList (lam :: initExprs)
    desugar2 env app

let desugarLetLoop env name bindings body =
    let bindings = List.map (function | List [Symbol name; sexpr] -> Symbol name, sexpr | _ -> failwith "wrong binding") bindings
    let names, initExprs = List.unzip bindings
    let lambda = exprsToList (Symbol "lambda" :: exprsToList names :: body)
    let call = exprsToList (Symbol name :: initExprs)
    let binding = exprsToList [exprsToList [Symbol name; lambda]]
    let letrec = exprsToList [Symbol "letrec"; binding; call]
    desugar2 env letrec

let desugarLetrec env bindings body =
    let getBindings sexpr bindings =
        match sexpr with
        | List [Symbol name; initExpr] ->
            (name, initExpr) :: bindings
        | _ -> failwith "sexprToExpr: let: wrong bindings"

    let convertToAssign (name, expr) acc =
        exprsToList [Symbol "set!"; Symbol name; expr] :: acc

    let bindInitial name = exprsToList [Symbol name; Number 0]

    let bindings = List.foldBack getBindings bindings []
    let names = List.map fst bindings
    let letBindings = exprsToList (List.map bindInitial names)
    let body = List.foldBack convertToAssign bindings body
    let letExpr = exprsToList (Symbol "let" :: letBindings :: body)
    desugar2 env letExpr

let collectDefinitions defs es =
    match es with
    | List (Symbol "define" :: Cons(Symbol name, args) :: body) :: rest ->
        let lambda = exprsToList (Symbol "lambda" :: args :: body)
        let def = name, lambda
        collectDefinitions (def :: defs) rest
    | List [Symbol "define"; Symbol name; rhs] :: rest ->
        let def = name, rhs
        collectDefinitions (def :: defs) rest
    | _ -> (List.rev defs), es
// (name, (lambda args body)) => ()

let desugarMany env exprs =
    let defs, exprs = collectDefinitions [] exprs
    match defs, exprs with
    | [], expr :: exprs ->
        desugar2 env expr :: desugarMany env exprs
    | [], [] -> []
    | _, [] -> failwith "there must be some expressions after definitions"
    | _, expr :: exprs ->
        let defs = List.map (fun (name, expr) -> name, desugar2 env expr) defs
        let defs = List.map (function | name, expr -> exprsToList [Symbol name; expr] ) defs
        let exprs = desugar2 env expr :: desugarMany env exprs
        [desugarLetrec env defs exprs]


// let convertInnerDefinitions sexpr =
//     match sexpr with
//     | Symbol name -> Ref name
//     | List [Symbol "if"; cond; conseq; altern] ->
//         If(sexprToExpr cond, sexprToExpr conseq, sexprToExpr altern)
//     | List (Symbol "if" :: _) ->
//         failwithf "wrong 'if' form:\n%A" (sexprToString sexpr)
//     | List(Symbol "begin" :: e :: exprs) ->
//         let es = convertList exprs
//         Begin(sexprToE| List [Symbol "set!"; Symbol name; rhs] -> Assign(name, sexprToExpr rhs)xpr e :: es)
//
//     | List(Symbol "lambda" :: args :: body) ->
//         if List.isEmpty body then
//             failwithf "lambda: body should not be empty\n%s\n" (sexprToString sexpr)

//         let args, dotted = parseArgs args
//         let strings = symbolsToStrings args
//         Lambda(strings, dotted, convertList body)
//     | List [Symbol "quote"; List []] -> EmptyList
//     | List [Symbol "quote"; form] ->
//         Quote(quotedFormToExpr form)
//     | List (Symbol "foreign-call" :: String foreignName :: tail) ->
//         let args = convertList tail
//         ForeignCall(foreignName, args)
//     | List(Symbol op :: tail) when isPrimop op ->
//         let op = tryStringToPrimop op
//         PrimApp(Option.get op, convertList tail)
//     | List(head :: tail) -> App(sexprToExpr head, convertList tail)
//     | e -> failwith <| sexprToString e

let convertDefinitions sexpr =

    let convertTopLevel sexpr =
        match sexpr with
        | List (Symbol "define" :: Cons(Symbol name, args) :: body) ->
            let lambda = exprsToList (Symbol "lambda" :: args :: body)
            exprsToList [Symbol "set!"; Symbol name; lambda]
        | List [Symbol "define"; Symbol name; sexpr] ->
            exprsToList [Symbol "set!"; Symbol name; sexpr]
        | sexpr -> sexpr

    match sexpr with
    | List sexprs when not (List.isEmpty sexprs) ->
        let sexprs = List.map convertTopLevel sexprs
        1
    | _ -> failwith "stringToProgram: parsing failed"