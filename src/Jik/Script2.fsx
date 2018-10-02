#load "Base.fs"
#load "Util.fs"

open System
open Base
open System.IO

type Expr =
    | Int of int
    | Ref of string
    | If of Expr * Expr * Expr
    | Assign of string * Expr
    | Lambda of string list * Expr list
    | Begin of Expr list
    | App of Expr * Expr list

let rec freeVarsExpr bound = function
    | If (cond, thenc, elsec) ->
        Set.unionMany [
            freeVarsExpr bound cond
            freeVarsExpr bound thenc
            freeVarsExpr bound elsec
        ]
    | Assign (v, rhs) ->
        Set.add v bound
        |> Set.union (freeVarsExpr bound rhs) 
    | App (head, tail) ->
        freeVarsExprList bound tail
        |> Set.union (freeVarsExpr bound head) 
    | Lambda (vars, call) ->
        let bound' = (Set.union bound (Set.ofList vars))
        freeVarsExprList bound' call
    | Begin exprs ->
        freeVarsExprList bound exprs
    | Ref name -> 
        if Set.contains name bound then
            Set.empty
        else 
            Set.singleton name
    | _ -> bound

and freeVarsExprList bound exprList =
    List.map (freeVarsExpr bound) exprList
    |> Set.unionMany

let rec findModifiedVars vars = function 
    | Ref name -> 
        Set.empty
    | If (cond, thenc, elsec) ->
        Set.unionMany [
            findModifiedVars vars cond
            findModifiedVars vars thenc
            findModifiedVars vars elsec
        ]
    | Assign (v, rhs) ->
        if Set.contains v vars then Set.singleton v else Set.empty 
        |> Set.union (findModifiedVars vars rhs) 
    | App (head, tail) ->
        findModifiedVarsList vars tail
        |> Set.union (findModifiedVars vars head) 
    | Lambda (args, call) ->
        let vars' = (Set.difference vars (Set.ofList args))
        freeVarsExprList vars' call
    | Begin exprs ->
        freeVarsExprList vars exprs
    | _ -> Set.empty

and findModifiedVarsList vars exprList =
    List.map (findModifiedVars vars) exprList
    |> Set.unionMany

let rec sexprToExpr = function
    | SExpr.Number n ->
        Int n
    | SExpr.Symbol name ->
        Ref name
    | List [SExpr.Symbol "if"; cond; conseq; altern] ->
        If (sexprToExpr cond,
            sexprToExpr conseq,
            sexprToExpr altern)
    | List (Symbol "begin" :: e :: exprs) ->
        let es = List.map sexprToExpr exprs
        Begin (sexprToExpr e :: es)
    | List [Symbol "set!"; Symbol name; rhs] ->
        Assign (name, sexprToExpr rhs)
    | List (Symbol "lambda" :: List args :: body) ->
        Lambda (symbolsToStrings args, List.map sexprToExpr body)
    | List [Symbol "quote"; form] ->
        failwith "" //
    | List (head :: tail) ->
        App (sexprToExpr head, List.map sexprToExpr tail)
    | e -> failwith <| sexprToString e

let e = "(lambda (x) (begin
    (set! x 2)
    (f x (lambda (y) (+ y x)))))"
stringToSExpr e |> sexprToExpr |> freeVarsExpr Set.empty