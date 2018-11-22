module Intermediate

open Core
open Graph
open Display

type Var = string

/// Intermediate language. It is a mix of SSA and CPS.
/// Labels (blocks) with arguments consist of statements. The last
/// statement in block is transfer statement.
/// Other statements are declarations.

type CallCont =
    | NonTail of string
    | Tail

type Simple =
    | Prim of Prim * Var list
    | Int of int
    | Bool of bool
    | FunctionRef of string
    | Lambda of Var list * Var list * Label list

and Transfer =
    | Return of Var
    | Jump of Var * Var list
    | Call of CallCont * Var * Var list
    | If of Var * Var * Var

and Decl = Var * Simple

and Stmt = 
    | Decl of Decl
    | Transfer of Transfer

and Label = Var * Var list * Stmt list

and Function = Var * Var list * Var list * Label list

type Program = 
    { Procedures : Function list
      Main : Function 
      Globals : string list }

let schemeEntryLabel = "schemeEntry"

let generalAccess labels name f cn =
    match List.tryFind (fun (name2, _, _) -> name2 = name) labels with
    | Some x -> f x
    | None -> failwithf "%s: name=%s" cn name

let getArgsOfLabel labels name =
    generalAccess labels name (fun (_, args, _) -> args) "getArgsOfLabel"

let getStmts labels name =
    generalAccess labels name (fun (_, _, stmts) -> stmts) "getStmts"

let getSuccs stmts =
    match List.tryLast stmts with
    | Some(Transfer(Jump (label, _))) -> [label]
    | Some(Transfer(If (_, labelt, labelf))) -> [labelt; labelf]
    | Some(Transfer(Call (NonTail label, _, _))) -> [label]
    | _ -> []    

let getPredecessors labels name =
    let fold preds (name2, _, stmts) =
        if name2 <> name && List.contains name (getSuccs stmts) then
            name2 :: preds
        else preds

    List.fold fold [] labels

let getUsed = function
    | Transfer(Return v) -> Set.singleton v
    | Transfer(Jump(_, args)) -> Set.ofList args
    | Transfer(Call(_, func, args)) -> Set.ofList (func :: args)
    | Transfer(If(cond, _, _)) -> Set.singleton cond
    | Decl(_, Prim(_, args)) -> Set.ofList args
    | _ -> Set.empty
    
let getDefined = function
    | Decl(var, _) -> Set.singleton var
    | _ -> Set.empty

let getVars labels =
    let handleLabel (name, args, stmts) =
        List.map getUsed stmts @
        List.map getDefined stmts
        |> List.fold Set.union Set.empty
        |> Set.union (Set.ofList args)

    List.map handleLabel labels
    |> List.fold Set.union Set.empty

let comma = iInterleave (iStr ", ")

let rec showLabel (name, vars, stmts) =
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
            | Simple.FunctionRef lab -> 
                iConcat [iStr "(func-ref "; iStr lab; iStr ")"]
            | Simple.Lambda(free, args, labels) ->
                iConcat [iNewline
                         iStr "  "
                         showDef ("lam", free, args, labels) |> iIndent]                
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
        | Call (tail, func, args) -> 
            let tail =
                match tail with
                | NonTail lab ->
                    iStr (" => " + lab)
                | _ -> iNil            
            iConcat [iStr ("call " + func + " ")
                     comma (List.map iStr args)
                     iStr " "
                     tail]
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

and showDef (name, free, args, labels) =
    iConcat [iStr "def "
             iStr name
             iStr " <"
             comma  (List.map iStr free)
             iStr ">"
             iStr " ("
             comma  (List.map iStr args)
             iStr ")"
             iNewline
             iInterleave iNewline (List.map showLabel labels)
             iNewline]

let showProgram prog =
    iConcat [List.map showDef prog.Procedures |> iConcat
             iNewline
             iNewline
             showDef prog.Main]

let labelsToString labels =
    List.map showLabel labels
    |> iInterleave (iStr "\n\n")
    |> iDisplay

let programToString (prog : Program) =
    showProgram prog |> iDisplay

let rec convertExpr expr (cont : Var -> Label list * Stmt list) =
    let makeJumpCont label var =
        [], [Transfer(Jump(label, [var]))]

    match expr with            
    | Expr.Ref var -> cont var
    | Expr.Bool b -> 
        let var = freshLabel "b"
        let labels, stmts = cont var
        labels, Decl(var, Bool b) :: stmts
    | Expr.Int n -> 
        let var = freshLabel "n"
        let labels, stmts = cont var
        labels, Decl(var, Int n) :: stmts    
    | Expr.If(exprc, exprt, exprf) ->
        let join, fresh = freshLabel "LJ", freshLabel "v"
        let labels, stmts = cont fresh
        let labelJoin = (join, [fresh], stmts)
        let labels = labelJoin :: labels
        convertIf exprc exprt exprf labels join
    | Expr.PrimApp(op, args) ->
        convertMany args (fun vars ->
            let fresh = freshLabel "v"
            let labels, stmts = cont fresh
            labels, [Decl(fresh, Prim(op, vars));] @ stmts)
    | Expr.App(Expr.Lambda(formals, body), args) ->
        convertMany args (fun vars ->
            let mapping = 
                List.zip formals vars |> Map.ofList
            let body = replaceVars mapping (Begin body)
            convertExpr body cont)
    | Expr.Begin(exprs) ->
        convertMany exprs (fun vars ->
            let last = List.last vars
            cont last)
    | Expr.Assign(_, _) -> failwith "Not Implemented Expr.Assign"
    | Expr.Lambda(args, body) ->
        let var = freshLabel "lam"
        let labels, stmts = cont var
        let lamLabels, lamStmts = convertExprTail (Begin body)
        labels, (Decl(var, Lambda([], args, (var, args, lamStmts) :: lamLabels))) :: stmts
    | Expr.App(func, args) ->
        convertExpr func (fun func ->
            convertMany args (fun args -> 
                let fresh = freshLabel "r"
                let labelName = freshLabel "LC"
                let labels, stmts = cont fresh
                let label = (labelName, [fresh], stmts)
                label :: labels, [Transfer(Call(NonTail labelName, func, args))]))
    | Expr.FunctionRef(label) -> 
        let var = freshLabel "fr"
        let labels, stmts = cont var
        labels, Decl(var, FunctionRef label) :: stmts

and convertExprJoin expr (contVar : Var) =
    let jump var = Transfer(Jump(contVar, [var]))
    match expr with
    | Expr.Bool b -> 
        let var = freshLabel "b"
        [], [Decl(var, Bool b); jump var]
    | Expr.Int n -> 
        let var = freshLabel "n"
        [], [Decl(var, Int n); jump var]
    | Expr.Ref(var) -> [], [jump var]
    | Expr.If(exprc, exprt, exprf) ->
        convertIf exprc exprt exprf [] contVar    
    | Expr.Assign(_, _) -> failwith "Not Implemented Expr.Assign"
    | Expr.Lambda(args, body) -> 
        let var = freshLabel "lam"
        let labels, stmts = convertExprTail (Begin body)
        [], [Decl(var, Lambda([], args, (var, args, stmts) :: labels)); jump var]
    | Expr.Begin(exprs) ->
        let heads, tail = List.splitAt (List.length exprs - 1) exprs
        convertMany heads (fun _ ->
            convertExprJoin tail.Head contVar)
    | Expr.App(Expr.Lambda(formals, body), args) ->
        convertMany args (fun vars ->
            let mapping = 
                List.zip formals vars |> Map.ofList
            let body = replaceVars mapping (Begin body)
            convertExprJoin body contVar)
    | Expr.App(func, args) ->
        convertExpr func (fun func ->
            convertMany args (fun args -> 
                [], [Transfer(Call(NonTail contVar, func, args))]))
    | Expr.PrimApp(op, args) ->
        convertMany args (fun vars ->
            let fresh = freshLabel "v"
            [], [Decl(fresh, Prim(op, vars)); Transfer(Jump(contVar, [fresh]))])
    | Expr.FunctionRef(label) -> 
        let var = freshLabel "fr"
        [], [Decl(var, FunctionRef label); jump var]

and convertExprTail expr =
    match expr with
    | Expr.Bool b -> 
        let var = freshLabel "b"
        [], [Decl(var, Bool b); Transfer(Return var)]
    | Expr.Int n -> 
        let var = freshLabel "n"
        [], [Decl(var, Int n); Transfer(Return var)]
    | Expr.Ref(var) -> [], [Transfer(Return var)]
    | Expr.If(exprc, exprt, exprf) ->
        convertExpr exprc (fun var ->
            let lt, lf = 
                freshLabel "LT", freshLabel "LF"
            let labelst, stmtst = 
                convertExprTail exprt 
            let labelt = (lt, [], stmtst)
            let labelsf, stmtsf = 
                convertExprTail exprf
            let labelf = (lf, [], stmtsf)
            [labelt] @ labelst @ [labelf] @ labelsf, [Transfer(If(var, lt, lf))])
    | Expr.Assign(_, _) -> failwith "Not Implemented Expr.Assign"
    | Expr.Lambda(args, body) -> 
        let var = freshLabel "lam"
        let labels, stmts = convertExprTail (Begin body)
        [], [Decl(var, Lambda([], args, (var, args, stmts) :: labels))]
    | Expr.Begin(exprs) ->
        let heads, tail = List.splitAt (List.length exprs - 1) exprs
        convertMany heads (fun _ ->
            convertExprTail tail.Head)
    | Expr.App(Expr.Lambda(formals, body), args) ->
        convertMany args (fun vars ->
            let mapping = 
                List.zip formals vars |> Map.ofList
            let body = replaceVars mapping (Begin body)
            convertExprTail body)
    | Expr.App(func, args) ->
        convertExpr func (fun func ->
            convertMany args (fun args -> 
                [], [Transfer(Call(Tail, func, args))]))
    | Expr.PrimApp(op, args) ->
        convertMany args (fun vars ->
            let var = freshLabel "v"
            [], [Decl(var, Prim(op, vars)); Transfer(Return var)])
    | Expr.FunctionRef(label) -> 
        let var = freshLabel "fr"
        [], [Decl(var, FunctionRef label); Transfer(Return var)]

and convertIf exprc exprt exprf labels join =
    convertExpr exprc (fun var ->
        let lt, lf = 
            freshLabel "LT", freshLabel "LF"
        let labelst, stmtst = 
            convertExprJoin exprt join
        let labelt = (lt, [], stmtst)
        let labelsf, stmtsf = 
            convertExprJoin exprf join
        let labelf = (lf, [], stmtsf)
        [labelt] @ labelst @ [labelf] @ labelsf @ labels, [Transfer(If(var, lt, lf))])

and convertMany exprs (cont : Var list -> Label list * Stmt list) =
    let rec loop vars = function
        | expr :: rest ->
            convertExpr expr (fun var -> 
                loop (var :: vars) rest)
        | [] ->
            cont (List.rev vars)
    loop [] exprs

let convertMainExprs expr : Function =
    let labels, stmts = convertExprTail expr
    schemeEntryLabel, [], [], (schemeEntryLabel, [], stmts) :: labels

let convertProgram (prog : Core.Program) : Program =
    { Procedures = []
      Main = convertMainExprs (Begin prog.Main)
      Globals = prog.Globals }
    

let analyzeFreeVars (prog : Program) : Program =
    let globals = Set.ofList prog.Globals

    let rec transformStmt stmt =
        match stmt with
        | Decl(var, Lambda(_, args, labels)) ->
            let free, args, labels = transformLambda (args, labels)
            Set.singleton var, Set.remove var (Set.ofList free), Decl(var, Lambda(free, args, labels))
        | Decl(var, _) -> Set.singleton var, Set.difference (getUsed stmt) globals, stmt
        | _ -> Set.empty, Set.difference (getUsed stmt) globals, stmt

    and transformLabel (name, args, stmts) =
        let defined, free, stmts =
            List.fold (fun (defined, free, stmts) stmt ->
                let defined1, free1, stmt = transformStmt stmt
                Set.union defined defined1, Set.union free free1, stmt :: stmts)
                (Set.empty, Set.empty, []) stmts
        Set.union defined (Set.ofList args), free, (name, args, List.rev stmts)

    and transformLambda ((args, labels) as func) =
        let defined, free, labels =
            List.fold (fun (defined, free, labels) label ->
                let defined1, free1, label = transformLabel label
                Set.union defined defined1, (Set.union free (Set.ofSeq free1)), label :: labels) 
                (Set.empty, Set.empty, []) labels
        let labels = List.rev labels
        let free = Set.difference free (Set.union defined (Set.ofList args)) |> Set.toList
        free, args, labels

    and transformFunction (name, free, args, labels) =
        let free, args, labels = transformLambda (args, labels)
        name, free, args, labels

    { prog with Procedures = List.map transformFunction prog.Procedures
                Main = transformFunction prog.Main }

let replaceClosureRef free var = 
    match List.tryFindIndex ((=) var) free with
    | Some i ->
        let closRef = freshLabel "cr"
        let closIndex = freshLabel "ci"
        [Decl(closIndex, Int i)
         Decl(closRef, Prim(ClosureRef, [closIndex]))], closRef
     | _ -> [], var


let replaceClosureRefs free vars =
    List.foldBack (fun var (stmts, vars) ->
        let stmts1, var = replaceClosureRef free var
        stmts1 @ stmts, var :: vars)
        vars ([], []) 

let closureConversion (prog : Program) : Program =
    let rec convertStmt free = function
        | Decl(var, Prim(op, args)) ->
            let stmts, args = replaceClosureRefs free args
            [], stmts @ [Decl(var, Prim(op, args))]
        | Decl(var, Lambda(free, args, labels)) ->
            let proc = freshLabel "proc"
            let procs = convertLambda proc (free, args, labels)
            procs, [Decl(var, Prim(Prim.MakeClosure, proc :: free))]
        | Decl(_) as decl ->
            [], [decl]
        | Transfer(Transfer.Return(var)) ->
            let stmts, var = replaceClosureRef free var
            [], stmts @ [Transfer(Transfer.Return(var))]
        | Transfer(Transfer.Jump(label, args)) ->
            let stmts, args = replaceClosureRefs free args
            [], stmts @ [Transfer(Transfer.Jump(label, args))]
        | Transfer(Transfer.Call(tail, func, args)) ->
            let stmts1, func = replaceClosureRef free func
            let stmts2, args = replaceClosureRefs free args
            [], stmts1 @ stmts2 @ [Transfer(Transfer.Call(tail, func, args))]
        | Transfer(Transfer.If(cond, labelt, labelf)) ->
            let stmts, cond = replaceClosureRef free cond
            [], stmts @ [Transfer(Transfer.If(cond, labelt, labelf))]

    and convertLabel free (name, args, stmts) =
        let procs, stmts =
            List.foldBack (fun stmt (procs, stmts) -> 
                let procs1, stmts1 = convertStmt free stmt
                procs1 @ procs, stmts1 @ stmts)
                stmts ([], [])
        procs, (name, args, stmts)

    and foldLabels free labels =
        List.foldBack (fun label (procs, labels) ->
            let procs1, label = convertLabel free label
            procs1 @ procs, label :: labels)
            labels ([], [])
    
    and convertLambda proc (free, args, labels) =
        match labels with
        | (name, args, stmts) :: restLabels ->
            let procs, labels = foldLabels free ((proc, args, stmts) :: restLabels)
            (proc, free, args, labels) :: procs
        | _ -> failwith "convertLambda: wrong labels format"
        
    let convertFunction (name, free, args, labels) =
        let procs, labels = foldLabels free labels
        procs, (name, free, args, labels)

    let converted = List.map convertFunction prog.Procedures
    let procs = List.collect fst converted
    let defs = List.map snd converted
    let procs1, main = convertFunction prog.Main
    { prog with Procedures = defs @ procs @ procs1 
                Main = main }
