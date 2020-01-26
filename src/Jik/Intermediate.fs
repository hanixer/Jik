module Intermediate

open Core
open Graph
open Display
open Common
open Primitive
open Library

type Var = string

/// Intermediate language. It is a mix of SSA and CPS.
/// Blocks with arguments consist of statements.
/// The last statement in block is a transfer statement.
/// Other statements are declarations.
type CallCont =
    | NonTail of string
    | Tail

type Simple =
    | Prim of Prim * Var list
    | Int of int
    | Char of char
    | Bool of bool
    | EmptyList
    | Lambda of Var list * Var list * bool * Block list

and Transfer =
    | Return of Var
    | Jump of Var * Var list
    | Call of CallCont * Var * Var list
    | Apply of CallCont * Var * Var list
    | If of Var * Var * Var
    | ForeignCall of contVar : Var * foreignName : string * args : Var list

and Decl = Var * Simple

and Stmt =
    | Decl of Decl
    | Transfer of Transfer

and Block = Var * Var list * Stmt list

and Function =
    { Name : string
      Free : string list
      Args : string list
      IsDotted : bool
      Blocks : Block list }

type Program =
    { Procedures : Function list
      Main : Function
      Globals : string list
      GlobalsOriginal : string list // Contains original scheme names, for error reporting.
      ConstantsNames : string list
      Strings : (string * string) list }

let schemeEntryLabel = "schemeEntry"

let emptyFunction =
    { Name = ""
      Free = []
      Args = []
      IsDotted = false
      Blocks = [] }

let generalAccess blocks name f cn =
    match List.tryFind (fun (name2, _, _) -> name2 = name) blocks with
    | Some x -> f x
    | None -> failwithf "%s: name=%s" cn name

let getArgsOfBlock blocks name = generalAccess blocks name (fun (_, args, _) -> args) "getArgsOfBlock"
let getStmts blocks name = generalAccess blocks name (fun (_, _, stmts) -> stmts) "getStmts"

let getSuccs stmts =
    match List.tryLast stmts with
    | Some(Transfer(Jump(block, _))) -> [ block ]
    | Some(Transfer(If(_, blockt, blockf))) -> [ blockt; blockf ]
    | Some(Transfer(Call(NonTail block, _, _))) -> [ block ]
    | _ -> []

let getPredecessors blocks name =
    let fold preds (name2, _, stmts) =
        if name2 <> name && List.contains name (getSuccs stmts) then name2 :: preds
        else preds
    List.fold fold [] blocks

let getUsed =
    function
    | Transfer(Return v) -> Set.singleton v
    | Transfer(Jump(_, args)) -> Set.ofList args
    | Transfer(Call(_, func, args)) -> Set.ofList (func :: args)
    | Transfer(If(cond, _, _)) -> Set.singleton cond
    | Decl(_, Prim(_, args)) -> Set.ofList args
    | _ -> Set.empty

let getDefined =
    function
    | Decl(var, _) -> Set.singleton var
    | _ -> Set.empty

let getVars blocks =
    let handleBlock (name, args, stmts) =
        List.map getUsed stmts @ List.map getDefined stmts
        |> List.fold Set.union Set.empty
        |> Set.union (Set.ofList args)
    List.map handleBlock blocks |> List.fold Set.union Set.empty

let comma = iInterleave (iStr ", ")

let rec showBlock (name, vars, stmts) =
    let showDecl = function
        | var, s ->
            let s =
                match s with
                | Simple.EmptyList -> iStr "'()"
                | Simple.Int n -> iNum n
                | Simple.Char c -> iStr (sprintf "#\\%A" c)
                | Simple.Bool true -> iStr "#t"
                | Simple.Bool false -> iStr "#f"
                | Simple.Prim(op, vars) ->
                    iConcat [ (sprintf "%A" op |> iStr)
                              iStr " "
                              List.map iStr vars |> comma ]
                | Simple.Lambda(free, args, dotted, blocks) ->
                    let deff =
                        { Name = "lam"
                          Free = free
                          Args = args
                          IsDotted = dotted
                          Blocks = blocks }
                    iConcat [ iNewline
                              iStr "  "
                              showDef deff |> iIndent ]
            iConcat [ iStr var
                      iStr " = "
                      s ]

    let showTransf =
        function
        | Return var -> iAppend (iStr "return ") (iStr var)
        | Jump(var, vars) ->
            iConcat [ iStr "jump "
                      iStr var
                      iStr " "
                      comma (List.map iStr vars) ]
        | Call(tail, func, args) ->
            let tail =
                match tail with
                | NonTail lab -> iStr (" => " + lab)
                | _ -> iNil
            iConcat [ iStr ("call " + func + " ")
                      comma (List.map iStr args)
                      iStr " "
                      tail ]
        | Apply(tail, func, args) ->
            let tail =
                match tail with
                | NonTail lab -> iStr (" => " + lab)
                | _ -> iNil
            iConcat [ iStr ("apply " + func + " ")
                      comma (List.map iStr args)
                      iStr " "
                      tail ]
        | If(a, b, c) ->
            iConcat [ iStr "if "
                      iStr a
                      iStr " "
                      iStr b
                      iStr " "
                      iStr c ]
        | ForeignCall(contVar, foreignName, args) ->
            let tail = iStr (" => " + contVar)
            iConcat [ iStr ("foreign-call " + foreignName + " ")
                      comma (List.map iStr args)
                      iStr " "
                      tail ]

    let showStmt =
        function
        | Decl d -> showDecl d
        | Transfer t -> showTransf t

    iConcat [ iStr name
              iStr " ("
              comma (List.map iStr vars)
              iStr ")"
              iNewline
              iInterleave iNewline <| List.map showStmt stmts ]

and showDef (func : Function) =
    // (name, free, args, dotted, blocks)
    let dotted =
        if func.IsDotted then iStr " dotted "
        else iNil
    iConcat [ iStr "def "
              iStr func.Name
              iStr " <"
              comma (List.map iStr func.Free)
              iStr ">"
              iStr " ("
              comma (List.map iStr func.Args)
              iStr ")"
              dotted
              iNewline
              iInterleave iNewline (List.map showBlock func.Blocks)
              iNewline ]

let showProgram prog =
    iConcat [ List.map showDef prog.Procedures |> iInterleave iNewline
              iNewline
              iNewline
              showDef prog.Main ]

let blocksToString blocks =
    List.map showBlock blocks
    |> iInterleave (iStr "\n\n")
    |> iDisplay

let programToString (prog : Program) = showProgram prog |> iDisplay

let convertSimpleDecl varPrefix value cont =
    let var = freshLabel varPrefix
    let blocks, stmts = cont var
    blocks, Decl(var, value) :: stmts

let rec convertExpr expr (cont : Var -> (Block list * Stmt list)) =
    let makeJumpCont block var = [], [ Transfer(Jump(block, [ var ])) ]
    match expr with
    | Expr.Assign(_) -> failwith "Not Implemented Expr.Assign"
    | Expr.String _ -> failwith "string literals should be removed before this stage"
    | Expr.Symbol _ | Expr.Quote _ -> failwith "quotes should be removed before this stage"
    | Expr.Ref var -> cont var
    | Expr.EmptyList -> convertSimpleDecl "nil" EmptyList cont
    | Expr.Bool b -> convertSimpleDecl "b" (Bool b) cont
    | Expr.Int n -> convertSimpleDecl "n" (Int n) cont
    | Expr.Char c -> convertSimpleDecl "c" (Char c) cont
    | Expr.If(exprc, exprt, exprf) ->
        let join, fresh = freshLabel "LJ", freshLabel "v"
        let blocks, stmts = cont fresh
        let blockJoin = (join, [ fresh ], stmts)
        let blocks = blockJoin :: blocks
        convertIf exprc exprt exprf blocks join
    | Expr.Lambda(args, dotted, body) ->
        let var = freshLabel "lam"
        let blocks, stmts = cont var
        let lamBlocks, lamStmts = convertExprTail (Begin body)
        let body = (var, args, lamStmts) :: lamBlocks
        blocks, (Decl(var, Lambda([], args, dotted, body))) :: stmts
    | Expr.Begin(exprs) ->
        convertMany exprs (fun vars ->
            let last = List.last vars
            cont last)
    | Expr.App(Expr.Lambda(formals, dotted, body), args) ->
        convertMany args (fun vars ->
            let mapping = List.zip formals vars |> Map.ofList
            let body = replaceVars mapping (Begin body)
            convertExpr body cont)
    | Expr.PrimApp(Prim.Apply, args) ->
        convertMany args (fun result ->
            if List.isEmpty args then
                failwith "apply should have at least one argument"
            let func = List.head result
            let args = List.tail result
            let fresh = freshLabel "r"
            let blockName = freshLabel "LC"
            let blocks, stmts = cont fresh
            let block = (blockName, [ fresh ], stmts)
            block :: blocks, [ Transfer(Apply(NonTail blockName, func, args)) ])
    | Expr.PrimApp(op, args) ->
        convertMany args (fun vars ->
            let fresh = freshLabel "v"
            let blocks, stmts = cont fresh
            blocks, [ Decl(fresh, Prim(op, vars)) ] @ stmts)
    | Expr.App(func, args) ->
        convertExpr func (fun func ->
            convertMany args (fun args ->
                let fresh = freshLabel "r"
                let blockName = freshLabel "LC"
                let blocks, stmts = cont fresh
                let block = (blockName, [ fresh ], stmts)
                block :: blocks, [ Transfer(Call(NonTail blockName, func, args)) ]))
    | Expr.ForeignCall(foreignName, args) ->
        convertMany args (fun args ->
                let fresh = freshLabel "r"
                let blockName = freshLabel "LC"
                let blocks, stmts = cont fresh
                let block = (blockName, [ fresh ], stmts)
                block :: blocks, [ Transfer(ForeignCall(blockName, foreignName, args)) ])

and convertExprJoin expr (contVar : Var) =
    let jump var = Transfer(Jump(contVar, [ var ]))
    let jump2 var = [], [ Transfer(Jump(contVar, [ var ])) ]
    match expr with
    | Expr.Assign(_) -> failwith "Not Implemented Expr.Assign"
    | Expr.String _ -> failwith "string literals should be removed before this stage"
    | Expr.Symbol _ | Expr.Quote _ -> failwith "quotes should be removed before this stage"
    | Expr.EmptyList
    | Expr.Bool _
    | Expr.Int _
    | Expr.Char _
    | Expr.Ref _
    | Expr.Lambda _ -> convertExpr expr jump2
    | Expr.If(exprc, exprt, exprf) -> convertIf exprc exprt exprf [] contVar
    | Expr.Begin(exprs) ->
        let heads, tail = List.splitAt (List.length exprs - 1) exprs
        convertMany heads (fun _ -> convertExprJoin tail.Head contVar)
    | Expr.App(Expr.Lambda(formals, dotted, body), args) ->
        convertMany args (fun vars ->
            let mapping = List.zip formals vars |> Map.ofList
            let body = replaceVars mapping (Begin body)
            convertExprJoin body contVar)
    | Expr.App(func, args) ->
        convertExpr func (fun func ->
            convertMany args (fun args ->
                [], [ Transfer(Call(NonTail contVar, func, args)) ]))
    | Expr.PrimApp(Prim.Apply, (func :: args)) ->
        convertExpr func (fun func ->
            convertMany args (fun args ->
                [], [ Transfer(Apply(NonTail contVar, func, args)) ]))
    | Expr.PrimApp(op, args) ->
        convertMany args (fun vars ->
            let fresh = freshLabel "v"
            [],
            [ Decl(fresh, Prim(op, vars))
              Transfer(Jump(contVar, [ fresh ])) ])
    | Expr.ForeignCall(foreignName, args) ->
        convertMany args (fun args ->
            [], [ Transfer(ForeignCall(contVar, foreignName, args)) ])

and convertExprTail expr =
    let jump2 var = [], [ Transfer(Return var) ]
    match expr with
    | Expr.Assign(_) -> failwith "Not Implemented Expr.Assign"
    | Expr.String _ -> failwith "string literals should be removed before this stage"
    | Expr.Symbol _ | Expr.Quote _ -> failwith "quotes should be removed before this stage"
    | Expr.EmptyList
    | Expr.Bool _
    | Expr.Int _
    | Expr.Char _
    | Expr.Ref _
    | Expr.ForeignCall _
    | Expr.Lambda _ ->
        // Convert expression, generate a new variable and return that variable,
        // because for above cases we don't have nothing to do for tail call.
        convertExpr expr jump2
    | Expr.If(exprc, exprt, exprf) ->
        convertExpr exprc (fun var ->
            let lt, lf = freshLabel "LT", freshLabel "LF"
            let blockst, stmtst = convertExprTail exprt
            let blockt = (lt, [], stmtst)
            let blocksf, stmtsf = convertExprTail exprf
            let blockf = (lf, [], stmtsf)
            [ blockt ] @ blockst @ [ blockf ] @ blocksf, [ Transfer(If(var, lt, lf)) ])
    | Expr.Begin(exprs) ->
        let heads, tail = List.splitAt (List.length exprs - 1) exprs
        convertMany heads (fun _ -> convertExprTail tail.Head)
    | Expr.App(Expr.Lambda(formals, false, body), args) ->
        convertMany args (fun vars ->
            let mapping = List.zip formals vars |> Map.ofList
            let body = replaceVars mapping (Begin body)
            convertExprTail body)
    | Expr.App(func, args) ->
        convertExpr func (fun func ->
            convertMany args (fun args ->
                [], [ Transfer(Call(Tail, func, args)) ]))
    | Expr.PrimApp(Prim.Apply, (func :: args)) ->
        convertExpr func (fun func ->
            convertMany args (fun args ->
                [], [ Transfer(Apply(Tail, func, args)) ]))
    | Expr.PrimApp(op, args) ->
        convertMany args (fun vars ->
            let var = freshLabel "v"
            [],
            [ Decl(var, Prim(op, vars))
              Transfer(Return var) ])

and convertIf exprc exprt exprf blocks join =
    convertExpr exprc (fun var ->
        let lt, lf = freshLabel "LT", freshLabel "LF"
        let blockst, stmtst = convertExprJoin exprt join
        let blockt = (lt, [], stmtst)
        let blocksf, stmtsf = convertExprJoin exprf join
        let blockf = (lf, [], stmtsf)
        [ blockt ] @ blockst @ [ blockf ] @ blocksf @ blocks, [ Transfer(If(var, lt, lf)) ])

and convertMany exprs (cont : Var list -> (Block list * Stmt list)) =
    let rec loop vars =
        function
        | expr :: rest -> convertExpr expr (fun var -> loop (var :: vars) rest)
        | [] -> cont (List.rev vars)
    loop [] exprs

let convertMainExprs expr : Function =
    let blocks, stmts = convertExprTail expr
    { Name = schemeEntryLabel
      Free = []
      Args = []
      IsDotted = false
      Blocks = (schemeEntryLabel, [], stmts) :: blocks }

let convertProgram (prog : Core.Program) : Program =
    { Procedures = []
      Main = convertMainExprs (Begin prog.Main)
      Globals = prog.Globals
      GlobalsOriginal = prog.GlobalsOriginal
      ConstantsNames = prog.ConstantsNames
      Strings = prog.Strings }

let analyzeFreeVars (prog : Program) : Program =
    let stringNames = List.map fst prog.Strings
    let globals = Set.ofList (prog.Globals @ prog.ConstantsNames @ stringNames)

    let rec transformStmt stmt =
        match stmt with
        | Decl(var, Lambda(_, args, dotted, blocks)) ->
            let free, blocks = transformLambda (args, blocks)
            Set.singleton var, Set.remove var (Set.ofList free), Decl(var, Lambda(free, args, dotted, blocks))
        | Decl(var, _) -> Set.singleton var, Set.difference (getUsed stmt) globals, stmt
        | _ -> Set.empty, Set.difference (getUsed stmt) globals, stmt

    and transformBlock (name, args, stmts) =
        let defined, free, stmts =
            List.fold (fun (defined, free, stmts) stmt ->
                let defined1, free1, stmt = transformStmt stmt
                Set.union defined defined1, Set.union free free1, stmt :: stmts) (Set.empty, Set.empty, [])
                stmts
        Set.union defined (Set.ofList args), free, (name, args, List.rev stmts)

    and transformLambda (args, blocks) =
        let defined, free, blocks =
            List.fold (fun (defined, free, blocks) block ->
                let defined1, free1, block = transformBlock block
                Set.union defined defined1, (Set.union free (Set.ofSeq free1)), block :: blocks)
                (Set.empty, Set.empty, []) blocks

        let blocks = List.rev blocks
        let free = Set.difference free (Set.union defined (Set.ofList args)) |> Set.toList
        free, blocks

    and transformFunction (func : Function) =
        let free, blocks = transformLambda (func.Args, func.Blocks)
        { func with Free = free
                    Blocks = blocks }

    let procedures = List.map transformFunction prog.Procedures
    let main = transformFunction prog.Main

    if not main.Free.IsEmpty then
        printfn "%A" (programToString prog)
        failwithf "Names %A are not defined" main.Free

    { prog with Procedures = procedures
                Main = main }

let replaceClosureRef free var =
    match List.tryFindIndex ((=) var) free with
    | Some i ->
        let closRef = freshLabel "cr"
        let closIndex = freshLabel "ci"
        [ Decl(closIndex, Int i)
          Decl(closRef, Prim(ClosureRef, [ closIndex ])) ], closRef
    | _ -> [], var

let replaceClosureRefs free vars =
    List.foldBack (fun var (stmts, vars) ->
        let stmts1, var = replaceClosureRef free var
        stmts1 @ stmts, var :: vars) vars ([], [])

let closureConversion (prog : Program) : Program =
    let rec convertStmt free =
        function
        | Decl(var, Prim(op, args)) ->
            let stmts, args = replaceClosureRefs free args
            [], stmts @ [ Decl(var, Prim(op, args)) ]
        | Decl(var, Lambda(freeInner, args, dotted, blocks)) ->
            let proc = freshLabel "proc"
            let procs = convertLambda proc (freeInner, args, dotted, blocks)
            let stmts, freeInner = replaceClosureRefs free freeInner
            procs, stmts @ [ Decl(var, Prim(Prim.MakeClosure, proc :: freeInner)) ]
        | Decl(_) as decl -> [], [ decl ]
        | Transfer(Transfer.Return(var)) ->
            let stmts, var = replaceClosureRef free var
            [], stmts @ [ Transfer(Transfer.Return(var)) ]
        | Transfer(Transfer.Jump(block, args)) ->
            let stmts, args = replaceClosureRefs free args
            [], stmts @ [ Transfer(Transfer.Jump(block, args)) ]
        | Transfer(Transfer.Call(tail, func, args)) ->
            let stmts1, func = replaceClosureRef free func
            let stmts2, args = replaceClosureRefs free args
            [], stmts1 @ stmts2 @ [ Transfer(Transfer.Call(tail, func, args)) ]
        | Transfer(Transfer.Apply(tail, func, args)) ->
            let stmts1, func = replaceClosureRef free func
            let stmts2, args = replaceClosureRefs free args
            [], stmts1 @ stmts2 @ [ Transfer(Transfer.Apply(tail, func, args)) ]
        | Transfer(ForeignCall(contVar, foreignName, args)) ->
            let stmts2, args = replaceClosureRefs free args
            [], stmts2 @ [ Transfer(Transfer.ForeignCall(contVar, foreignName, args)) ]
        | Transfer(Transfer.If(cond, blockt, blockf)) ->
            let stmts, cond = replaceClosureRef free cond
            [], stmts @ [ Transfer(Transfer.If(cond, blockt, blockf)) ]

    and convertBlock free (name, args, stmts) =
        let procs, stmts =
            List.foldBack (fun stmt (procs, stmts) ->
                let procs1, stmts1 = convertStmt free stmt
                procs1 @ procs, stmts1 @ stmts) stmts ([], [])
        procs, (name, args, stmts)

    and foldBlocks free blocks =
        List.foldBack (fun block (procs, blocks) ->
            let procs1, block = convertBlock free block
            procs1 @ procs, block :: blocks) blocks ([], [])

    and convertLambda newName (free, args, dotted, blocks) =
        let blocks =
            match blocks with
            | (_, _, stmts) :: restBlocks -> ((newName, args, stmts) :: restBlocks)
            | _ -> failwith "convertLambda: wrong blocks format"

        let procs, blocks = foldBlocks free blocks

        let proc =
            { Name = newName
              Free = free
              Args = args
              Blocks = blocks
              IsDotted = dotted }
        proc :: procs

    let convertFunction (func : Function) =
        let procs, blocks = foldBlocks func.Free func.Blocks
        procs, { func with Blocks = blocks }

    let converted = List.map convertFunction prog.Procedures
    let procs = List.collect fst converted
    let defs = List.map snd converted
    let procs1, main = convertFunction prog.Main
    { prog with Procedures = defs @ procs @ procs1
                Main = main }

let allIntermediateTransformations =
    convertProgram
    >> analyzeFreeVars
    >> closureConversion