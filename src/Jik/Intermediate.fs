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

and Function = Var * Var list * Label list

type Program = Function list * Label list

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

let showLabel (name, vars, stmts) =
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
        | Call _ -> iStr "call \n"
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

let showDef (name, args, labels) =
    iConcat [iStr "def "
             iStr name
             iStr " ("
             comma  (List.map iStr args)
             iStr ")"
             iNewline
             iInterleave iNewline (List.map showLabel labels)]

let showProgram (defs, labels) =
    iConcat [List.map showDef defs |> iConcat
             iNewline
             iNewline
             List.map showLabel labels |> iConcat]

let labelsToString labels =
    List.map showLabel labels
    |> iInterleave (iStr "\n\n")
    |> iDisplay

let programToString prog =
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
    | Expr.Assign(_, _) -> failwith "Not Implemented"
    | Expr.Lambda(_) -> failwith "Not Implemented"
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
    | Expr.Lambda(_) -> failwith "Not Implemented Expr.Lambda"
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
    | Expr.Lambda(_) -> failwith "Not Implemented Expr.Lambda"
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

let convertFunction (name, (args, body)) : Function =
    let labels, stmts = 
        convertExpr (Begin body) (fun var -> [], [Transfer(Return var)])
    let labels = (name, args, stmts) :: labels
    name, args, labels

let tope expr =
    let labels, stmts = convertExpr expr (fun var -> [], [Transfer(Return var)])
    (schemeEntryLabel, [], stmts) :: labels

let convertProgram (defs, expr) : Program =
    List.map convertFunction defs, tope expr
    
let computeLiveAfter labels : Map<string, Set<string>> =
    let st = Set.singleton
    let lt = Set.ofList
    
    let foldStmt liveAfter stmt =
        let used = getUsed stmt
        let defined = getDefined stmt
        Set.union used (Set.difference liveAfter defined)

    let liveForStmts liveAfter stmts =
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
                (liveForStmts liveAfter stmts)
                (getArgsOfLabel labels label |> Set.ofList)
        updatePredecessors liveAfterMap liveBefore label

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

let buildInterference labels =
    let liveAfterMap = computeLiveAfter labels
    let graph = makeGraph (getVars labels)

    let addEdge var1 var2 = 
        if var1 <> var2 then
            addEdge graph var1 var2

    let handleStmt stmt liveNow = 
        match stmt with
        | Decl(var, Prim(_, vars)) -> 
            Set.iter (addEdge var) (Set.union liveNow (Set.ofList vars))
            Set.union (Set.ofList vars) (Set.remove var liveNow)
        | Decl(var, _) -> 
            Set.iter (addEdge var) liveNow
            Set.remove var liveNow
        | _ -> Set.union liveNow (getUsed stmt)

    let handleLabel (name, args, stmts) =
        let live = Map.find name liveAfterMap
        let liveNow = List.foldBack handleStmt stmts live 
        List.iter 
            (fun x -> Set.iter (fun y -> addEdge x y) liveNow)
            args

    List.iter handleLabel labels

    graph
 