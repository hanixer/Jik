#load "Base.fs"
#load "Util.fs"
#load "Display.fs"


open System
open Base
open Display
open System.Text

// Supported features:
//  - integers
//  - conditionals
//  - assignments
// Assumption for source language: rhs expressions in letrec are lambdas.

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
    | Lambda of LambdaData
    | Begin of Expr list
    | App of Expr * Expr list
    | LetRec of (string * LambdaData) list * Expr list

and LambdaData = string list * Expr list

let transformLetrecBindings2 bindings =
  List.map (function
    | List [Symbol name; List (Symbol "lambda" :: List args :: body)] -> 
        let args = List.map (function | Symbol n -> n | e -> failwithf "arg is expected") args
        name, (args, body)
    | e -> failwithf "letrec: wrong bindings %A" e) bindings

let rec sexprToExpr sexpr =
   let convertList = List.map sexprToExpr
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
        let es = convertList exprs
        Begin (sexprToExpr e :: es)
    | List [Symbol "set!"; Symbol name; rhs] ->
        Assign (name, sexprToExpr rhs)
    | List (Symbol "lambda" :: List args :: body) ->
        Lambda (symbolsToStrings args, convertList body)
    | List [Symbol "quote"; form] ->
        failwith "sexprToExpr: quote is not supported"
    | List (Symbol "let" :: List bindings :: body) ->
        let folder sexpr (names, initExprs) =
            match sexpr with
            | List [Symbol name; initExpr] -> name :: names, initExpr :: initExprs
            | _ -> failwith "sexprToExpr: let: wrong bindings"
        let names, initExprs = List.foldBack folder bindings ([], [])
        let lam = Lambda (names, convertList body)
        let argExprs = convertList initExprs
        App (lam, argExprs)
    | List (Symbol "letrec" :: List bindings :: body) ->
        let bindings = transformLetrecBindings2 bindings
        let handleLambda (name, (args, body)) =
            name, (args, convertList body)
        let lambdas = 
            List.map handleLambda bindings
        LetRec (lambdas, convertList body)
    | List (head :: tail) ->
        App (sexprToExpr head, convertList tail)
    | e -> failwith <| sexprToString e

type Var = string

type Cps =
    | Const of int
    | Ref of Var
    | Lambda of Var list * Cps
    | LetVal of (Var * Cps) list * Cps
    | LetCont of (Var * Var list * Cps) list * Cps
    | LetRec of (Var * Var list * Cps) list * Cps
    | If of Var * Cps * Cps
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
    | Expr.Int n ->
        let var = freshLabel "num"
        LetVal ([var, Const n], cont var)
    | Expr.If (cond, ethen, eelse) ->
        convertIf cond ethen eelse cont
    | Expr.Assign(var, rhs) ->
        convertAssign var rhs cont
    | Expr.Lambda(_) -> failwith "Not Implemented"
    | Expr.Begin(_) -> failwith "Not Implemented"
    | Expr.LetRec(_, _) -> failwith "Not Implemented"

and convertApp func args cont =
    convert func (fun funcVar ->
        let rec loop vars = function
            | [] -> 
                let vars = List.rev vars
                let contVar = freshLabel "k"
                let resultVar = freshLabel "callResult"
                LetCont ([contVar, [resultVar], cont resultVar], NonTailCall (contVar, funcVar, vars))
            | arg :: args ->
                convert arg (fun argVar -> loop (argVar :: vars) args)
        loop [] args)

and convertIf cond ethen eelse cont =
    convert cond (fun condVar ->
        let thenVar = freshLabel "thenk"
        let elseVar = freshLabel "elsek"
        let thenk = thenVar, [], convert ethen cont
        let elsek = elseVar, [], convert eelse cont
        LetCont ([thenk; elsek], If (condVar, ContCall (thenVar, []), ContCall (elseVar, []))))

and convertAssign var rhs cont =
    convert rhs (fun value ->
        let dummy = freshLabel "unspecified"
        LetVal ([dummy, Assign (var, value)], cont dummy))

let rec showCps cps = 
    let funclike needEq s args body = 
        let v = if needEq then iStr " = " else iNil
        iConcat [iStr s; iStr " ("; iInterleave (iStr ",") (List.map iStr args); iStr ")"; v; iNewline; iStr "  "; iIndent <| showCps body; iNewline]
    match cps with
    | Cps.Ref v -> iStr v
    | Cps.Const n -> iNum n
    | Cps.Lambda(args, body) -> funclike false "lambda" args body
    | Cps.LetVal(bindings, body) -> 
        iConcat [iStr "letval "; 
                 iIndent (iConcat (List.map (fun (v, e) -> 
                    iConcat [iStr v; iStr " = "; showCps e; iNewline]) bindings));
                 iStr " in ";
                 showCps body]
    | Cps.LetCont(bindings, body) -> 
        iConcat [iStr "letcont "; 
                 iIndent (iConcat (List.map (fun (v, args, e) -> 
                    funclike true v args e) bindings));
                 iStr " in ";
                 showCps body]
    | Cps.LetRec(bindings, body) -> 
        iConcat [iStr "letrec "; 
                 iIndent (iConcat (List.map (fun (v, args, e) -> 
                    funclike true v args e) bindings));
                 iStr " in ";
                 showCps body]
    | Cps.If(c, t, e) ->
        iConcat [iStr "if "; iStr c; iNewline
                 showCps t;
                 showCps e]
    | Cps.Assign(v, e) -> iConcat [iStr v; iStr " <- "; iStr e]
    | Cps.Seq(es) -> List.map showCps es |> iConcat
    | Cps.TailCall(f, v) -> [iStr "tailcall"; iStr f; List.map iStr v |> iInterleave (iStr ",")] |> iInterleave (iStr " ")
    | Cps.NonTailCall(k, f, v) -> [iStr "call"; iStr k; iStr f; List.map iStr v|> iInterleave (iStr ",")] |> iInterleave (iStr " ")
    | Cps.ContCall(k, x) -> [iStr "contcall"; iStr k; List.map iStr x |> iInterleave (iStr ",")] |> iInterleave (iStr " ")
    | Cps.Return(v) -> iConcat [iStr "return "; iStr v] 

let cpsToString = showCps >> iDisplay

"(if (a j)(b i)(c w))" |> stringToSExpr |> sexprToExpr |> convert <| (fun v -> Return v) |> cpsToString
"(if (a j)(set! b i)(set! c w))" |> stringToSExpr |> sexprToExpr |> convert <| (fun v -> Return v) |> cpsToString