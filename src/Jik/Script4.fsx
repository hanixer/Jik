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

type Thing() =
    inherit FSharpFunc<int, int>()
    override this.Invoke(n) = n + 1

let makeRenamingProcedure initialLevel colour initialEnvironment capturing =
    let levelCorrection = level - initialLevel
    let inserted = []
    let environment = initialEnvironment
    1

