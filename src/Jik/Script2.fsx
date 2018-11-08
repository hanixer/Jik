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
open Display

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

let generalAccess labels name f =
    match List.tryFind (fun (name2, _, _) -> name2 = name) labels with
    | Some x -> f x
    | None -> failwithf "generalAccess: name=%s" name

let getArgsOfLabel labels name =
    generalAccess labels name (fun (_, args, _) -> args)

let getStmts labels name =
    generalAccess labels name (fun (_, _, stmts) -> stmts)

let getSuccs stmts =
    match List.tryLast stmts with
    | Some(Transfer(Jump (label, _))) -> [label]
    | Some(Transfer(If (_, labelt, labelf))) -> [labelt; labelf]
    | Some(Transfer(Call (label, _, _))) -> [label]
    | _ -> []    

let getPredecessors labels name =
    let fold preds (name2, _, stmts) =
        if name2 <> name && List.contains name (getSuccs stmts) then
            name2 :: preds
        else preds

    List.fold fold [] labels

let showLabel (name, vars, stmts) =
    let comma = iInterleave (iStr ", ")

    let showDecl = function
        | var, s ->
          let s =
            match s with
            | Simple.Int n -> iNum n
            | Simple.Bool true -> iStr "#t"
            | Simple.Bool false -> iStr "#f"
            | Simple.Prim(op, vars) ->
                iConcat [(sprintf "%A" op |> iStr)
                         iStr " "
                         List.map iStr vars |> comma ]
          iConcat [iStr var 
                   iStr " = "
                   s]

    let showTransf = function
        | Return var -> iAppend (iStr "return ") (iStr var)
        | Jump(var, vars) -> 
            iConcat [iStr "jump "
                     iStr var
                     iStr " "
                     comma (List.map iStr vars)]
        | Call _ -> iStr "call "
        | If(a, b, c) -> 
            iConcat [iStr "if "
                     iStr a
                     iStr " "
                     iStr b
                     iStr " "
                     iStr c]

    let showStmt = function
        | Decl d -> showDecl d
        | Transfer t -> showTransf t

    iConcat [iStr name
             iStr " ("
             comma  (List.map iStr vars)
             iStr ")"
             iNewline 
             iInterleave iNewline <| List.map showStmt stmts]

let labelsToString labels =
    List.map showLabel labels
    |> iInterleave (iStr "\n\n")
    |> iDisplay

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

    | Expr.App(Expr.Lambda(formals, body), args) ->
        convertMany args (fun vars ->
            let vars = List.rev vars
            let mapping = 
                List.zip formals vars |> Map.ofList
            let body = replaceVars mapping (Begin body)
            convert body cont)

    | Expr.Begin(exprs) ->
        convertMany exprs (fun vars ->
            let last = List.last vars
            cont last)


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
        | var, Simple.Prim(Prim.Sub, [var1]) ->
            [InstrName.Mov, [Var var1; Var var]
             InstrName.Neg, [Var var]]
        | var, Simple.Prim(Prim.Lt, [var1; var2]) ->
            comparison var1 var2 Cc.L (Some(Var var))
        | var, e -> failwithf "handleDecl: %s %A" var e

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
    

let computeLiveAfter labels : Map<string, Set<string>> =
    let st = Set.singleton
    let lt = Set.ofList

    let getUsed = function
        | Transfer(Return v) -> st v
        | Transfer(Jump(_, args)) -> Set.ofList args
        | Transfer(Call(_, func, args)) -> Set.ofList (func :: args)
        | Transfer(If(cond, _, _)) -> st cond
        | Decl(_, Prim(_, args)) -> Set.ofList args
        | _ -> Set.empty
        
    let getDefined = function
        | Decl(var, _) -> Set.singleton var
        | _ -> Set.empty
    
    let foldStmt liveAfter stmt =
        let used = getUsed stmt
        let defined = getDefined stmt
        Set.union used (Set.difference liveAfter defined)

    let liveBeforeStmts liveAfter stmts =
        List.fold foldStmt liveAfter (List.rev stmts)

    let updatePredecessors liveAfterMap liveBefore label =
        let preds = getPredecessors labels label
        List.fold (fun (todo, liveAfterMap) pred ->
            let liveAfter = Map.find label liveAfterMap
            if Set.isSubset liveBefore liveAfter then
                todo, liveAfterMap
            else
                let map = Map.add pred (Set.union liveAfter liveBefore) liveAfterMap
                Set.add pred todo, map)
            (Set.empty, liveAfterMap)
            preds

    let handleLabel liveAfterMap label =
        let stmts = getStmts labels label
        let liveAfter = Map.find label liveAfterMap
        let liveBefore = 
            Set.difference
                (liveBeforeStmts liveAfter stmts)
                (getArgsOfLabel labels label |> Set.ofList)
        updatePredecessors liveAfterMap liveBefore label
        |> (fun (todo, map) -> printfn "label: %s\ntodo: %A\nmap: %A\n\n" label todo map; (todo, map))

    let rec loop todo liveAfterMap =
        if Set.isEmpty todo then
            liveAfterMap
        else
            let label = Seq.head todo
            let todo1 = Set.remove label todo
            let todo2, liveAfterMap = handleLabel liveAfterMap label
            loop (Set.union todo1 todo2) liveAfterMap

    let liveAfterMap =
        List.fold (fun acc (name, vars, stmts) ->
            Map.add name Set.empty acc) Map.empty labels

    let todo = 
        List.map (fun (name, _, _) -> name) labels
        |> Set.ofList

    loop todo liveAfterMap

let test s =
    stringToExpr s
    |> tope
    |> labelsToString
    |> printfn "%s"

let compile = compileHelper (fun s ->
    let labels = tope (stringToExpr s)
    labels |> dbg selectInstructions)

"(if (+ x) (+ y) (+ z))"
"(if (if a b c) y  z)"
"(if  y (if a b c) z)"
"(+ (+ x y) z (+ a b))"

let tests = [
    // "#t", "#t\n"
    // "#f", "#f\n"
    // "1", "1\n"
    // "-1", "-1\n"
    // "(+ 1 2)", "3\n"
    // "(+ 1 (+ 2 3))", "6\n"
    // "(+ (+ 1 4) (+ 2 3))", "10\n"
    // "(< 1 2)", "#t\n"
    // "(if 1 2 3)", "2\n"
    // "(if #f 2 3)", "3\n"
    // "(if (if 1 2 3) 4 5)", "4\n"
    // "(if 4 (if #f 2 3) 5)", "3\n"
    // "(if 4 (if #t 8 9) (if #f 2 3))", "8\n"    
//     "
// (let ([v 1])
// (let ([w 46])
// (let ([x (+ v 7)])
// (let ([y (+ 4 x)])
// (let ([z (+ x w)])
// (+ z (- y)))))))", "42\n"
    "
(let ([a 1]
      [b 2]
      [c 3])
  (if (< a b)
    (let ([d 4]
          [e 5])
      (+ c (+ d e)))
    (let ([f 6])
      (+ a (+ c f)))))", "12\n"
]

// runTestsWithName compile "basic" tests

let e ="
(let ([a 1]
      [b 2]
      [c 3])
  (if (< a b)
    (let ([d 4]
          [e 5])
      (+ c (+ d e)))
    (let ([f 6])
      (+ a (+ c f)))))"
let labels = 
    stringToExpr e
    |> tope
labelsToString labels |> printfn "%s\n\n\n"
computeLiveAfter labels |> printfn "%A"