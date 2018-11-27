// Learn more about F# at http://fsharp.org

open System

open SExpr

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

[<EntryPoint>]
let main argv =
    // "(let ((let 1)) (let ((x 1)) x))"
    "(let ((y 1)) (let ((x 1)) x))"
    |> stringToSExpr
    |> desugar2 []
    |> sexprToString
    |> printfn "Result: %s"
    0 // return an integer exit code
