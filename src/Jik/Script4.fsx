#load "SExpr.fs"

open SExpr

let sexprMap func sexpr =
    let rec loop sexpr =
        match sexpr with
        | Nil -> Nil
        | Cons(a, b) -> Cons(func a, func b)
        | _ -> func sexpr
    
    loop sexpr

let datumToSyntax rename datum =
    let func = function
        | Symbol s -> Symbol (rename s)
        | e -> e
    
    sexprMap func datum

let mutable level = 0

let makeRenamingProcedure initialLevel colour initialEnvironment capturing =
    let levelCorrection = level - initialLevel
    let inserted = []
    let environment = initialEnvironment
    1











    


let special = [
    "lambda"
    "let"
    "let*"
    "letrec"
    "letrec*"
    "and"
    "or"
    "cond"
]

let rec desugar2 env sexpr =
    match sexpr with
    | List (Symbol s :: rest) when (List.contains s special) && (List.contains s env |> not) ->
        expand env sexpr
    | List sexprs ->
        exprsToList (List.map (desugar2 env) sexprs)
    | _ -> sexpr

and expand env sexpr =
    match sexpr with
    | List(Symbol "lambda" :: List args :: body) ->
        let argsString = symbolsToStrings args
        let env = argsString @ env
        let body = List.map (desugar2 env) body
        exprsToList (Symbol "lambda" :: exprsToList args :: body)
    | List(Symbol "let" :: List bindings :: body) -> 
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
    | _ ->
        printfn "Oh no! %A" sexpr
        sexpr

"(let ((y 1)) (let ((x 1)) x))" |> stringToSExpr |> desugar2 [] |> sexprToString 