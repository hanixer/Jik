#load "Base.fs"
#load "Graph.fs"
#load "Util.fs"
#load "Display.fs"
#load "TestDriver.fs"
#load "RuntimeConstants.fs"
#load "Base.fs"
#load "Core.fs"
#load "Cps.fs"
#load "Codegen.fs"

open Base
open Core
open Cps
open Codegen
open Graph
open TestDriver

type Simple =
    | Prim of Prim * Var list
    | Int of int
    | Bool of bool

and Transfer =
    | Return of Var
    | Jump of Var * Var list
    | Call of Var * Var * Var list
    | If of Var * Var * Var

and Decl = Var * Simple

and Stmt = 
    | Decl of Decl
    | Transfer of Transfer

and Label = Var * Var list * Stmt list

and Function = Var list * Label list * Var

let rec convert expr (cont : Var -> Label list * Stmt list) =
    match expr with            
    | Expr.PrimApp(op, args) ->
        convertMany args (fun vars ->
            let fresh = freshLabel "v"
            let blocks, stmts = cont fresh
            blocks, [Decl(fresh, Prim(op, vars));] @ stmts)

    | Expr.If(exprc, exprt, exprf) ->
        convert exprc (fun var ->
            let l1, l2, l3, fresh = freshLabel "L", freshLabel "L", freshLabel "L", freshLabel "v"
            let blockst, stmtst = convert exprt (fun var -> [], [Transfer(Jump(l3, [var]))])
            let blocksf, stmtsf = convert exprf (fun var -> [], [Transfer(Jump(l3, [var]))])
            let blocks, stmts = cont fresh
            (l1, [], stmtst) :: (l2, [], stmtsf) :: (l3, [fresh], stmts) :: (blockst @ blocksf @ blocks), [Transfer(If(var, l1, l2))])

    | Expr.Ref var -> cont var
    | Expr.Int n -> 
        let var = freshLabel "n"
        let blocks, stmts = cont var
        blocks, Decl(var, Int n) :: stmts

and convertMany exprs (cont : Var list -> Label list * Stmt list) =
    let rec loop vars = function
        | expr :: rest ->
            convert expr (fun var -> loop (var :: vars) rest)
        | [] ->
            cont (List.rev vars)
    loop [] exprs

let tope expr =
    let blocks, stmts = convert expr (fun var -> [], [Transfer(Return var)])
    ("start", [], stmts) :: blocks

let test s =
    stringToExpr s
    |> tope
    |> printfn "%A"

"(if (+ x) (+ y) (+ z))" |> test
"(if (if a b c) y  z)" |> test
"(if  y (if a b c) z)" |> test
"(+ (+ x y) z (+ a b))" |> test