#load "Base.fs"
#load "Util.fs"
#load "Display.fs"


open System
open Base
open Display
open System.Text

// Input:
//  <exp> ::= <literal>
//         |  <variable>
//         |  (if <exp> <exp> <exp>)
//         |  (set! <var> <exp>)
//         |  (lambda (<var>*) <body>)
//         |  (begin <body>)
//         |  (<exp> <exp>*)
//         |  (let ((<var> <exp)*) <body>)
//         |  (letrec ((<var> <exp)*) <body>)

type Expr =
    | Int of int
    | Ref of string
    | If of Expr * Expr * Expr
    | Assign of string * Expr
    | Lambda of string list * Expr list
    | Begin of Expr list
    | App of Expr * Expr list

let rec sexprToExpr sexpr =
   match sexpr with
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
        failwith "sexprToExpr: quote is not supported"
    | List (Symbol "let" :: List bindings :: body) ->
        let folder sexpr (names, initExprs) =
            match sexpr with
            | List [Symbol name; initExpr] -> name :: names, initExpr :: initExprs
            | _ -> failwith "sexprToExpr: let: wrong bindings"
        let names, initExprs = List.foldBack folder bindings ([], [])
        let lam = Lambda (names, List.map sexprToExpr body)
        let argExprs = List.map sexprToExpr initExprs
        App (lam, argExprs)
    | List (Symbol "letrec" :: List bindings :: body) ->
        let bindings = transformLetrecBindings bindings
        let fnames = List.map fst bindings
        let bindInitial fname = 
            Cons (Symbol fname, Cons (Number 0, Nil))
        let letBindings = 
            fnames
            |> List.map bindInitial
            |> exprsToList
        let foldSets (name, (args, lamBody)) next =
            let args = List.map Symbol args |> exprsToList
            let lambda = exprsToList [Symbol "lambda"; args; lamBody]
            let set = exprsToList [Symbol "set!"; Symbol name; lambda]
            set :: next
        let body =
            List.foldBack foldSets bindings body
        exprsToList (Symbol "let" :: letBindings :: body)
        |> sexprToExpr
    | List (head :: tail) ->
        App (sexprToExpr head, List.map sexprToExpr tail)
    | e -> failwith <| sexprToString e

type Var = string

type Cps =
    | Const of int
    | Ref of Var
    | Lambda of Var list * Cps
    | LetVal of (Var * Cps) list * Cps
    | LetCont of (Var * Var list * Cps) list * Cps
    | LetRec of (Var * Var list * Cps) list * Cps
    | If of Var * Var * Var
    | Assign of Var * Var
    | Seq of Cps list
    | TailCall of Var * Var list
    | NonTailCall of Var * Var * Var list
    | ContCall of Var * Var list
    | Return of Var

// Higher-order
//  <exp> ::= (if <simple> <exp> <exp>)
//         |  (set! <var> <simple>)
//         |  (lambda (<var>*) <body>)
//         |  (begin <body>)
//         |  (<simple> <simple>*)
//         |  (let ((<var> <exp)*) <body>)
//         |  (letrec ((<var> <exp)*) <body>)
//  <simple> ::= <literal>
//            |  <var>

let mutable n = 0

let freshLabel s =
        n <- n + 1
        sprintf "%s%d" s n

let rec convert expr cont =
    match expr with
    | Expr.Ref v -> cont v
    | Expr.App (func, args) ->
        convertApp func args cont
    | Expr.Int(_) -> failwith "Not Implemented"
    | Expr.If _ -> failwith "Not Implemented"
    | Expr.Assign(_, _) -> failwith "Not Implemented"
    | Expr.Lambda(_) -> failwith "Not Implemented"
    | Expr.Begin(_) -> failwith "Not Implemented"

and convertApp func args cont =
    let argFolder arg next =
        convert arg 
    convert func
    |> failwith ""