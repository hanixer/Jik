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

let makeIf cond conseq altern =
    exprsToList [Symbol "if"; cond; conseq; altern]

let desugarSingle expr =
    match expr with
    // core forms
    | Cons(Symbol "quote", _) -> expr
    | List [Symbol "if"; cond; conseq] ->
        let cond = desugarSingle cond
        let conseq = desugarSingle conseq
        makeIf cond conseq undefinedExpr
    | List [Symbol "if"; cond; conseq; altern] ->
        let cond = desugarSingle cond
        let conseq = desugarSingle conseq
        let altern = desugarSingle altern
        makeIf cond conseq altern
    | List [Symbol "set!"; Symbol var; rhs] ->
        let rhs = desugarSingle rhs
        exprsToList [Symbol "set!"; Symbol var; rhs]
    | List (Symbol "lambda" :: args :: body) ->
        let body = desugarBody body
        exprsToList (Symbol "lambda" :: args :: body)
    | List (Symbol "begin" :: body) ->
        desugarBegin body
    | List (Symbol "foreign-call" :: name :: args) ->
        let args = List.map desugarSingle args
        exprsToList (Symbol "foreign-call" :: name :: args)
    // derived forms
    | List(Symbol "let" :: List bindings :: body) ->
        desugarLet bindings body
    | List(Symbol "let" :: Symbol name :: List bindings :: body) ->
        desugarLetLoop name bindings body
    | List(Symbol "letrec" :: List bindings :: body) ->
        desugarLetrec bindings body
    | List(Symbol "and" :: args) ->
        desugarAnd args
    | List(Symbol "or" :: args) ->
        desugarOr args
    | List (Symbol "when" :: condition :: body) ->
        desugarWhen condition body
    | List (Symbol "unless" :: condition :: body) ->
        desugarUnless condition body
    | List (Symbol "cond" :: clauses) ->
        desugarCond clauses
    // primitive or function application
    | Cons(head, tail) ->
        let head = desugarSingle head
        let tail = desugarSingle tail
        Cons(head, tail)
    // other forms: nil, number, string, char, symbol
    // just return this expression
    | _ -> expr

let desugarLambda args body =
    let body = desugarBody body
    exprsToList (Symbol "lambda" :: args :: body)

// else e1 e2 ... => (begin e1 e2 ...)
// (test) => or test
// (test => exp) => (let ((t test)) (if t (exp t) rest))
// (test e1 e2 ...) => (if t (begin e1 e2 ...) rest)

let desugarCond clauses =
    let handleClause clause handled =
        match clause with
        | List (Symbol "else" :: rest) when Option.isNone handled ->
            Some(desugarBegin rest)
        | List [test] ->
            Some(desugarOr (test :: Option.toList handled))
        | List [test; Symbol "=>"; expr] ->
            let t = Symbol (freshLabel "t")
            let app = exprsToList [expr; t]
            let bindings = [exprsToList [t; test]]
            let altern = Option.defaultValue undefinedExpr handled
            let ifExpr = makeIf t app altern
            Some(desugarLet bindings [ifExpr])
        | List (test :: conseq) ->
            let test = desugarSingle test
            let conseq = desugarBegin conseq
            let altern = Option.defaultValue undefinedExpr handled
            Some(makeIf test conseq altern)
        | _ ->
            failwithf "desugarCond: wrong cond clause: %s" (sexprToString clause)

    List.foldBack handleClause clauses None
    |> Option.defaultValue undefinedExpr

let desugarWhen condition body =
    if List.isEmpty body then
        failwith "'when' body should not be empty"

    let condition = desugarSingle condition
    let body = desugarBegin body
    makeIf condition body undefinedExpr

let desugarUnless condition body =
    if List.isEmpty body then
        failwith "'unless' body should not be empty"

    let condition = desugarSingle condition
    let body = desugarBegin body
    makeIf condition undefinedExpr body

let desugarBegin body =
    let body = desugarBody body
    exprsToList (Symbol "begin" :: body)

let desugarAnd exprs =
    let handle expr desugared =
        let expr = desugarSingle expr
        match desugared with
        | Some(prev) ->
            Some(makeIf expr prev (Bool false))
        | None -> Some(expr)

    List.foldBack handle exprs None
    |> Option.defaultValue (Bool true)

let desugarOr exprs =
    let handle expr desugared =
        let expr = desugarSingle expr
        match desugared with
        | Some(prev) ->
            let t = Symbol (freshLabel "or")
            let ifExpr = makeIf t t prev
            let args = exprsToList [t]
            let lam = desugarLambda args [ifExpr]
            Some (exprsToList [lam; expr])
        | None -> Some(expr)

    List.foldBack handle exprs None
    |> Option.defaultValue (Bool false)

/// Returns list of symbol names and list of init expressions
let parseBindings bindings =
    let bindings = List.map (function
        | List [Symbol name; expr] -> Symbol name, expr
        | _ -> failwith "wrong binding") bindings

    List.unzip bindings

let desugarLetLoop name bindings body =
    if List.isEmpty body then
        let b = sexprToString(exprsToList bindings)
        failwithf "letloop: body should not be empty\nname: %s, bindings: %s\n" name b

    let names, initExprs = parseBindings bindings
    let args =  exprsToList names
    let lam = desugarLambda args body
    let binding = [exprsToList [Symbol name; lam]]
    let app = [exprsToList (Symbol name :: initExprs)]
    desugarLetrec binding app

let desugarLetrec bindings body =
    let convertToAssign (name, expr) =
        exprsToList [Symbol "set!"; name; expr]

    let bindInitial name = exprsToList [name; Number 0]

    let names, initExprs = parseBindings bindings
    let letBindings = List.map bindInitial names
    let assignments = List.map convertToAssign (List.zip names initExprs)
    let letBody = assignments @ body
    desugarLet letBindings letBody

let desugarLet bindings body =
    if List.isEmpty body then
        failwithf "let: body should not be empty\n%s\n" (sexprToString(exprsToList bindings))

    let names, initExprs = parseBindings bindings
    let initExprs = List.map desugarSingle initExprs
    let args = exprsToList names
    let lam = desugarLambda args body
    let app = exprsToList (lam :: initExprs)
    desugarSingle app

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

/// Desugar expression and handle internal (define ...) expressions.
let desugarBody exprs =
    let defs, exprs = collectDefinitions [] exprs
    match defs, exprs with
    | [], expr :: exprs ->
        desugarSingle expr :: desugarBody exprs
    | [], [] -> []
    | _, [] -> failwith "there must be some expressions after definitions"
    | _, expr :: exprs ->
        let defs = List.map (fun (name, expr) -> name, desugarSingle expr) defs
        let defs = List.map (function | name, expr -> exprsToList [Symbol name; expr] ) defs
        let exprs = desugarSingle expr :: desugarBody exprs
        [desugarLetrec defs exprs]

let desugarTopLevel expr =
    match expr with
    | List (Symbol "define" :: Cons(Symbol name, args) :: body) ->
        let lambda = exprsToList (Symbol "lambda" :: args :: body)
        exprsToList [Symbol "set!"; Symbol name; lambda]
    | List [Symbol "define"; Symbol name; sexpr] ->
        exprsToList [Symbol "set!"; Symbol name; sexpr]
    | sexpr -> sexpr

let desugar expr =
    match expr with
    | List exprs when not exprs.IsEmpty ->
        let exprs = List.map desugarTopLevel exprs
        List.map desugarSingle exprs
    | _ -> failwith "desugar: list of SExpr is expected"