module Intermediate

open Core
open Graph
open Display

/// Intermediate language. It is a mix of SSA and CPS.
/// Labels (blocks) with arguments consist of statements. The last
/// statement in block is transfer statement.
/// Other statements are declarations.

type Var = string

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
            let lt, lf, lj, fresh = 
                freshLabel "LT", freshLabel "LF", freshLabel "LJ", freshLabel "v"
            let jumpCont = (makeJumpCont lj)
            let labelst, stmtst = 
                convert exprt jumpCont
            let labelt = (lt, [], stmtst)
            let labelsf, stmtsf = 
                convert exprf jumpCont
            let labelf = (lf, [], stmtsf)
            let labels, stmts = cont fresh
            let labelJoin = (lj, [fresh], stmts)
            let restLabels = labels
            [labelt] @ labelst @ [labelf] @ labelsf @ [labelJoin] @ restLabels, [Transfer(If(var, lt, lf))])

    | Expr.App(Expr.Lambda(formals, body), args) ->
        convertMany args (fun vars ->
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
            convert expr (fun var -> 
                loop (var :: vars) rest)
        | [] ->
            cont (List.rev vars)
    loop [] exprs

let tope expr =
    let labels, stmts = convert expr (fun var -> [], [Transfer(Return var)])
    ("start", [], stmts) :: labels
    
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

let buildInterference labels liveAfterMap =
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

    let handleLabel (name, _, stmts) =
        let live = Map.find name liveAfterMap
        List.foldBack handleStmt stmts live
        |> ignore

    List.iter handleLabel labels

    graph