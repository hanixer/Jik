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
open RuntimeConstants

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
    let makeJumpCont label var =
        [], [Transfer(Jump(label, [var]))]

    match expr with            
    | Expr.PrimApp(op, args) ->
        convertMany args (fun vars ->
            let fresh = freshLabel "v"
            let labels, stmts = cont fresh
            labels, [Decl(fresh, Prim(op, vars));] @ stmts)

    | Expr.If(exprc, exprt, exprf) ->
        convert exprc (fun var ->
            let l1, l2, l3, fresh = 
                freshLabel "L", freshLabel "L", freshLabel "L", freshLabel "v"
            let jumpCont = (makeJumpCont l3)
            let labelst, stmtst = 
                convert exprt jumpCont
            let labelt = (l1, [], stmtst)
            let labelsf, stmtsf = 
                convert exprf jumpCont
            let labelf = (l2, [], stmtsf)
            let labels, stmts = cont fresh
            let labelJoin = (l3, [fresh], stmts)
            let restLabels = labels
            [labelt] @ labelst @ [labelf] @ labelsf @ [labelJoin] @ restLabels, [Transfer(If(var, l1, l2))])

    | Expr.Ref var -> cont var
    
    | Expr.Int n -> 
        let var = freshLabel "n"
        let labels, stmts = cont var
        labels, Decl(var, Int n) :: stmts
    
    | Expr.Bool b -> 
        let var = freshLabel "b"
        let labels, stmts = cont var
        labels, Decl(var, Bool b) :: stmts

and convertMany exprs (cont : Var list -> Label list * Stmt list) =
    let rec loop vars = function
        | expr :: rest ->
            convert expr (fun var -> loop (var :: vars) rest)
        | [] ->
            cont (List.rev vars)
    loop [] exprs

let tope expr =
    let labels, stmts = convert expr (fun var -> [], [Transfer(Return var)])
    ("start", [], stmts) :: labels

let getArgsOfLabel labels name =
    match List.tryFind (fun (name2, _, _) -> name2 = name) labels with
    | Some (_, args, _) -> args
    | None -> failwithf "getArgsOfLabel: name=%s" name

let selectInstructions labels =
    let convertNumber n = n <<< fixnumShift
    let moveInt n var = [Mov, [Operand.Int n; Operand.Var var]]

    let comparison var1 var2 cc dest =
        let instrs =
            [Mov, [Var var1; Reg Rax]
             Cmp, [Var var2; Reg Rax]
             Set cc, [Reg Al]
             Movzb, [Reg Al; Reg Rax]
             Sal, [Operand.Int boolBit; Reg Rax]
             Or, [Operand.Int falseLiteral; Reg Rax]]
        match dest with
        | None ->
            instrs
        | Some dest ->
            instrs @ [Mov, [Reg Rax; dest]]

    let handleDecl = function
        | var, Simple.Int n -> moveInt (convertNumber n) var
        | var, Simple.Bool true -> moveInt trueLiteral var
        | var, Simple.Bool false -> moveInt falseLiteral var
        | var, Simple.Prim(Prim.Add, [var1; var2]) ->
            [InstrName.Mov, [Var var1; Var var]
             InstrName.Add, [Var var2; Var var]]
        | var, Simple.Prim(Prim.Lt, [var1; var2]) ->
            comparison var1 var2 Cc.L (Some(Var var))

    let handleTransfer = function
        | Return var -> 
            [Mov, [Var var; Reg Rax]
             Ret, []]
        | Transfer.Jump(label, vars) ->
            let args = getArgsOfLabel labels label
            if List.length args <> List.length vars then 
                failwith "handleTransfer: wrong number of vars"
            let movArgs =
                List.zip vars args
                |> List.map (fun (var, arg) -> Mov, [Var var; Var arg])
            movArgs @ [InstrName.Jmp label, []]
        | Transfer.If(varc, labelt, labelf) ->
            [Mov, [Var varc; Reg Rax]
             Cmp, [Codegen.Int falseLiteral; Reg Rax]
             JmpIf (E, labelf), []]

    let handleStmt = function
        | Decl decl -> handleDecl decl
        | Transfer tran -> handleTransfer tran

    let handleLabel (name, vars, stmts) =
        let l = [InstrName.Label name, []]
        l @ List.collect handleStmt stmts

    List.collect handleLabel labels

let test s =
    stringToExpr s
    |> tope
    |> printfn "%A"

let compile = compileHelper (fun s ->
    let labels = tope (stringToExpr s)
    labels |> dbg selectInstructions)

"(if (+ x) (+ y) (+ z))"
"(if (if a b c) y  z)"
"(if  y (if a b c) z)"
"(+ (+ x y) z (+ a b))"

let tests = [
    "#t", "#t\n"
    "#f", "#f\n"
    "1", "1\n"
    "-1", "-1\n"
    "(+ 1 2)", "3\n"
    "(+ 1 (+ 2 3))", "6\n"
    "(+ (+ 1 4) (+ 2 3))", "10\n"
    "(< 1 2)", "#t\n"
    "(> 1 2)", "#f\n"
    "(<= 1 2)", "#t\n"
    "(>= 1 2)", "#f\n"
    "(eq? 1 2)", "#f\n"
    "(eq? 2 2)", "#t\n"
    "(if 1 2 3)", "2\n"
    "(if #f 2 3)", "3\n"
    "(if (if 1 2 3) 4 5)", "4\n"
    "(if 4 (if #f 2 3) 5)", "3\n"
    "(if 4 (if #t 8 9) (if #f 2 3))", "8\n"
]

runTestsWithName compile "basic" tests