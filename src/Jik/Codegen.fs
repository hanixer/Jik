module Codegen

open Core
open Graph
open RuntimeConstants
open Intermediate
open System.IO
open System.Collections.Generic
open Display

/// Code generation.
/// Language resembles x86 instruction set.
/// After conversion from Intermediate, 'Var' operands created.
/// Later these operands are changed to stack locations or registers.
/// 
/// Vectors:
/// (make-vector n)
/// (vector expr ...) - sugar
/// (vector-ref vec pos)
/// (vector-set! vec pos value)
/// (vector-length vec)
/// (vector? vec)

type Register =
    | Rsp | Rbp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi 
    | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    | Al | Ah | Bl | Bh | Cl | Ch

type Var = string

type Operand =
    | Int of int
    | Reg of Register
    | ByteReg of Register
    | Deref of int * Register
    | Deref4 of int * Register * Register * int
    | Var of Var
    | Slot of int
    | GlobalValue of string

type Cc =
    | E
    | L
    | Le
    | G
    | Ge

type InstrName =
    | Add
    | Sub
    | Neg
    | Mov
    | Sar
    | Sal
    | And
    | Or
    | Call of string
    | CallIndirect
    | Push
    | Pop
    | Ret
    | Xor
    | Cmp
    | Set of Cc
    | Movzb
    | Jmp of string
    | JmpIndirect
    | JmpIf of Cc * string
    | Label of string
    | Lea of string
    // intermediate
    | RestoreStack

and Instr = InstrName * Operand list

type FunctionDef = 
    { Name : string
      Free : string list
      Args : string list
      StackArgs : string list
      Instrs : Instr list
      Vars : string list ref
      MaxStack : int
      InterfGraph : Graph<Operand>
      LiveBefore : Map<int, Set<Operand>>
      LiveAfter : Map<int, Set<Operand>>
      SlotsOccupied : int }

type Program = FunctionDef list * FunctionDef

let freePointer = "freePointer"

let allRegisters = 
    [Rsp; Rbp; Rax; Rbx; Rcx; Rdx; Rsi; Rdi; 
     R8; R9; R10; R11; R12; R13; R14; R15; 
     Al; Ah; Bl; Bh; Cl; Ch]

let registersForArgs = 
    [Rcx; Rdx; R8; R9]

let callerSave = 
    [R10; R11; R8; R9; Rcx; Rdi; Rdx; Rsi]

let calleeSave =
    [R12; R13; R14; R15; Rbp; Rbx]

let isArithm = function
    | Add | Sub | Neg | Sar | Sal -> true
    | _ -> false

let showInstr out (op, args) =
    let showOp op =
        match op with
        | Set cc ->
            fprintf out "set%s " ((sprintf "%A" cc).ToLower())
        | Label label ->
            fprintf out "%s:" label
        | JmpIf (E, label) ->
            fprintf out "je %s" label
        | Jmp label ->
            fprintf out "jmp %s" label
        | Call label ->
            fprintf out "call %s" label
        | _ ->
            let s = sprintf "%A" op
            fprintf out "%s" (s.ToLower() + "q ")

    let reg r = 
        let s = sprintf "%%%A" r
        s.ToLower()

    let showArg = function
        | Int n -> fprintf out "$%d" n
        | Reg(r) -> fprintf out "%s" (reg r)
        | Deref(n, r) -> fprintf out "%d(%s)" n (reg r)
        | Deref4(n, r1, r2, m) -> fprintf out "%d(%s, %s, %d)" n (reg r1) (reg r2) m
        | Var(v) -> fprintf out "[var %s]" v
        | ByteReg(r) -> fprintf out "%s" (reg r)
        | Slot(n) -> fprintfn out "{slot %d}" n
        | GlobalValue(v) -> fprintf out "%s(%%rip)" v
        
    let showArgs args =
        List.iteri (fun i arg -> 
            showArg arg
            if i <> List.length args - 1 then
                fprintf out ", ") args

    match op, args with
    | CallIndirect, [arg] ->
        fprintf out "call *"
        showArg arg
    | JmpIndirect, [arg] ->
        fprintf out "jmp *"
        showArg arg
    | Lea label, [arg] ->
        fprintf out "leaq %s(%%rip), " label
        showArg arg
    | _ ->
        showOp op
        showArgs args

let showInstrs out instrs =
    List.iter (fun instr ->
        fprintf out "    "
        showInstr out instr
        fprintfn out "") instrs

let instrsToString instrs =
    let out = new StringWriter()
    fprintfn out "    .globl %s" schemeEntryLabel
    fprintfn out "%s:" schemeEntryLabel
    showInstrs out instrs
    out.ToString()

let programToString (defs, main) =   
    let out = new StringWriter()  

    let handleDef def =
        fprintfn out "    .globl %s" def.Name
        showInstrs out def.Instrs
        fprintfn out "\n\n"

    List.iter handleDef defs
    handleDef { main with Name = schemeEntryLabel }
    out.ToString()

let countMaxArgs labels =
    let handleStmt = function
        | Transfer(Intermediate.Call(_, _, args)) -> List.length args
        | _ -> 0

    let handleLabel (_, _, stmts) =
        List.map handleStmt stmts
        |> List.max

    List.map handleLabel labels
    |> List.max

let countVars args labels =
    let handleStmt = function
        | Decl(var, _) -> [var]
        | _ -> []
    let handleLabel (_, args, stmts) = 
        args @ (List.collect handleStmt stmts)
    List.collect handleLabel labels @ args
    |> List.distinct
    |> List.length

let argumentToLocation (siStart, siMult) reg args =
    let fold (regs, index, pairs) arg =
        match regs with
        | argReg :: rest -> 
            rest, index, Map.add arg (Reg argReg) pairs
        | _ ->
            let offset = siStart + siMult * index
            [], index + 1, Map.add arg (Deref(offset, reg)) pairs

    let _, _, result = List.fold fold (registersForArgs, 0, Map.empty) args
    result

let selectInstructions (defs, labels) : Program =
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

    let vectorAddress vec index =
        [Mov, [Var vec; Reg R11]
         Mov, [Var index; Reg R12]
         Sar, [Int fixnumShift; Reg R12]
         Add, [Int 1; Reg R12]]

    let handleDecl (var, x) =
        match x with
        | Simple.Int n -> moveInt (convertNumber n) var
        | Simple.Bool true -> moveInt trueLiteral var
        | Simple.Bool false -> moveInt falseLiteral var
        | Simple.Prim(Prim.Add, [var1; var2]) ->
            [InstrName.Mov, [Var var1; Var var]
             InstrName.Add, [Var var2; Var var]]
        | Simple.Prim(Prim.Mul, [var1; var2]) ->
            [InstrName.Mov, [Var var1; Var var]
             InstrName.Add, [Var var2; Var var]] // TODO: rework!
        | Simple.Prim(Prim.Sub, [var1; var2]) ->
            [InstrName.Mov, [Var var1; Var var]
             InstrName.Sub, [Var var2; Var var]]
        | Simple.Prim(Prim.Sub, [var1]) ->
            [InstrName.Mov, [Var var1; Var var]
             InstrName.Neg, [Var var]]
        | Simple.Prim(Prim.Lt, [var1; var2]) ->
            comparison var1 var2 Cc.L (Some(Var var))
        | Simple.FunctionRef(funcRef) ->
            [Lea(funcRef), [Var var]]
        | Simple.Prim(Prim.Not, [var1]) ->
            [Cmp, [Operand.Int falseLiteral; Var var1]
             Set E, [Reg Al]
             Movzb, [Reg Al; Reg Rax]
             Sal, [Operand.Int boolBit; Reg Rax]
             Or, [Operand.Int falseLiteral; Reg Rax]
             Mov, [Reg Rax; Var var]]
        | Simple.Prim(Prim.MakeVector, [var1]) ->
            let shift = if wordSize = 8 then 3 else 2
            [Mov, [GlobalValue(freePointer); Reg R11]
             Mov, [Var var1; Deref(0, R11)]
             Or, [Int vectorTag; Reg R11]
             Mov, [Reg R11; Var var]
             Mov, [Var var1; Reg R11]
             Sar, [Int fixnumShift; Reg R11]
             Add, [Int 1; Reg R11]
             Sal, [Int shift; Reg R11]
             Add, [Reg R11; GlobalValue(freePointer)]]
        | Simple.Prim(Prim.VectorLength, [var1]) ->
            [Mov, [Var var1; Reg R11]
             Mov, [Deref(-vectorTag, R11); Var var]]
        | Simple.Prim(Prim.IsVector, [var1]) ->
            [And, [Int vectorMask; Var var1]
             Cmp, [Int vectorTag; Var var1]
             Set E, [Reg Al]
             Movzb, [Reg Al; Reg Rax]
             Sal, [Operand.Int boolBit; Reg Rax]
             Or, [Operand.Int falseLiteral; Reg Rax]
             Mov, [Reg Rax; Var var]]
        | Simple.Prim(Prim.VectorSet, [vec; index; value]) ->
            vectorAddress vec index @
            [Mov, [Var value; Deref4(-vectorTag, R11, R12, wordSize)]]
        | Simple.Prim(Prim.VectorRef, [vec; index]) ->
            vectorAddress vec index @
            [Mov, [Deref4(-vectorTag, R11, R12, wordSize); Var var]]
        | e -> failwithf "handleDecl: %s %A" var e

    let handleTransfer labels = function
        | Return var -> 
            [Mov, [Var var; Reg Rax]
             RestoreStack, []
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
             Cmp, [Operand.Int falseLiteral; Reg Rax]
             JmpIf (E, labelf), []]
        | Transfer.Call(NonTail label, func, args) ->
            let argToLoc = argumentToLocation (-2 * wordSize, -wordSize) Rsp args
            let moveToArgPositions =
                List.map (fun arg -> Mov, [Var arg; Map.find arg argToLoc]) args                        
            let argsOfLabel = getArgsOfLabel labels label
            let argOfLabel = List.head argsOfLabel
            if List.length argsOfLabel <> 1 then 
                failwith "handleTransfer: wrong number of vars"
            moveToArgPositions @ 
            [CallIndirect, [Var func]
             Mov, [Reg Rax; Var argOfLabel]
             InstrName.Jmp label, []]
        | Transfer.Call(Tail, func, args) ->
            let argToLoc = argumentToLocation (-2 * wordSize, -wordSize) Rsp args
            let moveToArgPositions =
                List.map (fun arg -> Mov, [Var arg; Map.find arg argToLoc]) args
            let moveStackArgs =
                if List.length args > List.length registersForArgs then
                    List.skip (List.length registersForArgs) moveToArgPositions 
                    |> List.mapi (fun i x ->
                        match x with
                        | Mov, [_; arg2] -> Mov, [arg2; Slot(i)]
                        | _ -> failwith "wrong")
                else []
            moveToArgPositions @ 
            moveStackArgs @ 
            [RestoreStack, []
             JmpIndirect, [Var func]]


    let handleStmt labels = function
        | Decl decl -> handleDecl decl
        | Transfer tran -> handleTransfer labels tran

    let handleLabel labels (name, _, stmts) =
        (InstrName.Label name, []) :: List.collect (handleStmt labels) stmts

    let saveArgs args instrs =
        let args = 
            if List.length args > List.length registersForArgs then
                List.take (List.length registersForArgs) args
            else args
        let argToLoc = argumentToLocation (wordSize, wordSize) Rbp args
        let saveInstrs = 
            List.map (fun arg -> Mov, [Map.find arg argToLoc; Var arg]) args
        match instrs with
        | head :: tail -> head :: saveInstrs @ tail
        | _ -> instrs

    let handleDef (name, free, args, labels) =
        let instrs =
            List.collect (handleLabel labels) labels
            |> saveArgs args
        let graph = makeGraph []
        let diff = List.length args - List.length registersForArgs
        let slots = if diff > 0 then diff else 0
        let stackArgs = if diff > 0 then List.skip (List.length registersForArgs) args else []
        { Name = name
          Free = free
          Args = args
          StackArgs = stackArgs
          Vars = ref []
          MaxStack = 0
          InterfGraph = graph
          Instrs = instrs
          LiveBefore = Map.empty
          LiveAfter = Map.empty
          SlotsOccupied = slots }

    let impl = freshLabel "schemeEntryImpl"
    let implLabels =
        match labels with
        | (_, [], stmts) :: rest ->
            (impl, [], stmts) :: rest
        | _ -> failwith "selectInstructions: wrong entry labels"
    let defs = (impl, [], [], implLabels) :: defs
    let main = handleDef (schemeEntryLabel, [], [], [])
    let main = {main with Instrs = [Label schemeEntryLabel, []; Call impl, []]}
    List.map handleDef defs, main

let collectVars instrs =
    List.map snd instrs
    |> List.map (fun args ->
        List.map (fun arg ->
            match arg with
            | Var var -> Set.singleton var
            | _ -> Set.empty) args
        |> Set.unionMany)
    |> Set.unionMany

let dbg func =
    (fun x -> let y = func x in printfn "%s" (instrsToString y); y)

let computePreds instrs = 
    let labelToIndex = 
        Seq.mapi (fun i (x, _) -> i, x) instrs
        |> Seq.collect (fun (i, x) ->
            match x with
            | Label s -> [s, i]
            | _ -> [])
        |> Map.ofSeq

    let handleInstr (mapping, i) (op, _) =
        let add index mapping =
            if index < Seq.length instrs then
                (Map.add index (Map.find index mapping |> Set.add i) mapping)
            else mapping

        match op with
        | InstrName.Jmp label -> 
            let index = Map.find label labelToIndex
            add index mapping, i + 1
        | InstrName.JmpIf(_, label) ->         
            let index = Map.find label labelToIndex
            add index mapping 
            |> add (i + 1), i + 1
        | _ -> 
            add (i + 1) mapping, i + 1

    let initial = 
        Seq.fold (fun (mapping, i) _ -> Map.add i Set.empty mapping, i + 1) (Map.empty, 0) instrs
        |> fst
    Seq.fold handleInstr (initial, 0) instrs
    |> fst

let rec computeLiveness (defs, main) =
    let isRegVar = 
        function 
        | Reg _ | Var _ -> true 
        | _ -> false

    let writtenBy (op, args)  = 
        match op with
        | Add | Sub | Mov | Sar | Sal | And | Or | Xor | Movzb | Cmp ->
            List.item 1 args|> Set.singleton
        | Neg | Set _ -> List.head args |> Set.singleton
        | Call _ | CallIndirect -> callerSave |> List.map Reg |> Set.ofList
        | Lea _ -> Set.ofList (Reg Rax :: args)
        | _ -> Set.empty
        |> Set.filter isRegVar

    let readBy (op, args) =
        match op with
        | Call _ -> registersForArgs |> Set.ofList |> Set.map Reg            
        | CallIndirect ->
            Set.ofList (callerSave) |> Set.map Reg
            |> Set.union (Set.ofList args)
        | Mov | Movzb -> List.head args |> Set.singleton
        | Set _ -> Set.empty
        | Ret -> Set.ofList (Rax :: calleeSave) |> Set.map Reg
        | _ -> Set.ofList args
        |> Set.filter isRegVar

    let updatePreds preds afterMap beforeMap index =
        Seq.fold (fun (todo, afterMap) pred ->
            let after = Map.find pred afterMap
            let beforeCurr = Map.find index beforeMap
            if Set.isSubset beforeCurr after then
                todo, afterMap
            else
                let afterMap = Map.add pred (Set.union after beforeCurr) afterMap
                Set.add pred todo, afterMap)
            (Set.empty, afterMap)
            preds

    let handleInstr instrs preds afterMap beforeMap index =
        let instr = Seq.item index instrs
        let w = writtenBy instr
        let r = readBy instr
        let after = Map.find index afterMap
        let before = Set.union (Set.difference after w) r
        let afterMap = Map.add index (Set.difference after w) afterMap
        let beforeMap = Map.add index before beforeMap
        let todo, afterMap = 
            updatePreds (Map.find index preds) afterMap beforeMap index
        todo, afterMap, beforeMap

    let rec loop instrs preds todo afterMap beforeMap =
        if Set.isEmpty todo then
            afterMap, beforeMap
        else
            let index = Set.maxElement todo
            let todo1 = Set.remove index todo
            let todo2, liveAfterMap, liveBeforeMap = 
                handleInstr instrs preds afterMap beforeMap index
            loop instrs preds (Set.union todo1 todo2) liveAfterMap liveBeforeMap

    let liveAfter instrs = 
        let preds = computePreds instrs
        let todo = [0..Seq.length instrs - 1] |> Set.ofList
        let liveAfterMap =
            Seq.fold (fun (map, i) _ -> Map.add i Set.empty map, i + 1) (Map.empty, 0) instrs 
            |> fst
        loop instrs preds todo liveAfterMap liveAfterMap

    let handleDef def =
        let afterMap, beforeMap = liveAfter def.Instrs
        { def with LiveAfter = afterMap; LiveBefore = beforeMap }

    List.map handleDef defs, handleDef main

let rec buildInterference (defs, main) =
    let registersForUse = 
        [Rbx; Rcx; Rdx; Rsi; Rdi; Rbp;
         R8; R9; R10; R11; R12; R13; R14; R15]
        |> Set.ofList
                          

    let filterAndAddEdges graph ignore live target =
        Set.difference live (Set.ofList ignore)
        |> Set.iter (fun x -> 
            addEdge graph target x)

    let isGoodArg = function
        | Var _ -> true
        | Reg Rax -> false
        | Reg reg -> Set.contains reg registersForUse
        | _ -> false

    let iter live graph index (op, args) =
        let live =            
            match Map.tryFind index live with
            | Some vl -> vl
            | None _ ->
                failwithf "buildInterference: iter: index=%d live=%A size=%d" index live (Map.count live)
        match op, args with
        | Call _, _ | CallIndirect, _ ->
            for reg in callerSave do
                filterAndAddEdges graph [] live (Reg reg)
        | _, [_; arg] when isGoodArg arg ->
            filterAndAddEdges graph [] live arg
        | _, [arg] when isGoodArg arg ->
            filterAndAddEdges graph [] live arg
        | _ -> ()

    let handleDef def =
        let vars = collectVars def.Instrs |> Set.map Var
        let regs = registersForUse |> Set.map Reg
        let graph = makeGraph (Set.union vars regs)
        Seq.iteri (iter def.LiveAfter graph) def.Instrs
        printDotFunc (function | Var var -> var | Reg reg -> reg.ToString().ToLower() | _ -> "ERROR") graph (__SOURCE_DIRECTORY__ + "/../../misc/graphs/" + def.Name + ".dot")
        {def with InterfGraph = graph}

    List.map handleDef defs, handleDef main

/// Takes interference graph and all used variables
/// and returns mapping from variables and registers to their colors
let assignColors graph uncolored initialMap =
    let banned = Dictionary<Operand, Set<int>>()
    let addBanned v c =
        banned.Item v <- banned.Item v |> Set.add c

    let initBanned () =
        banned.Clear()
        for var in vertices graph do
            banned.Add(var, Set.empty)
            for adj in adjacent graph var  do
                if Map.containsKey adj initialMap then
                    let color = Map.find adj initialMap
                    addBanned var color
                        

    let pickNode nodes =
        Seq.maxBy (fun node -> 
            if banned.ContainsKey node then
                banned.Item node |> Seq.length
            else
                for kv in banned do printf "%A :: %A ;;;; " kv.Key kv.Value
                printfn "\n\n"
                -1 
            ) nodes

    let rec findLowestColor bannedSet =
        Seq.initInfinite id
        |> Seq.find (fun x -> Seq.exists ((=) x) bannedSet |> not)

    let updateBanned node color =
        adjacent graph node
        |> Seq.iter (fun v -> 
            addBanned v color)

    let rec loop nodes map =
        if Set.isEmpty nodes then
            map
        else
            let node = pickNode nodes
            let lowest = findLowestColor (banned.Item node)
            let color = Map.add node lowest map
            updateBanned node lowest
            loop (Set.remove node nodes) color

    initBanned()
    loop (Set.ofSeq uncolored) initialMap

let isRegister = function | Reg _ -> true | _ -> false

let makeInitialColorMap graph stackArgs = 
    let regs =
        vertices graph
        |> Seq.filter isRegister
    let count = Seq.length regs
    let seq1 = Seq.mapi (fun i reg -> reg, i) regs
    let seq2 = Seq.mapi (fun i arg -> arg, i + count) stackArgs
    Seq.append seq1 seq2
    |> Map.ofSeq

let getSortedVars graph stackArgs =
    vertices graph
    |> Seq.filter (function 
        | Var var when Seq.contains var stackArgs -> false
        | Var _ -> true 
        | _ -> false)
    |> Seq.map (fun var -> 
        let l = adjacent graph var
        var, Seq.length l)
    |> Seq.sortBy snd
    |> Seq.rev
    |> Seq.map fst

let geconvertMainExprsrandToLocation operandToColor stackArgs =
    let result, _, slots =
        operandToColor
        |> Map.fold (fun (result, colorToLocation, slot) operand color ->
            match operand with
            | Var var when Seq.contains var stackArgs ->
                let index = Seq.findIndex ((=)var) stackArgs
                Map.add operand (Slot index) result, Map.add color (Slot index) colorToLocation, slot
            | Var _ ->
                match Map.tryFind color colorToLocation with
                | Some location ->
                    Map.add operand location result, colorToLocation, slot
                | None ->
                    let colorToLocation = 
                        Map.add color (Slot slot) colorToLocation
                    Map.add operand (Slot slot) result, colorToLocation, slot + 1
            | Reg _ ->
                Map.add operand operand result, Map.add color operand colorToLocation, slot
            | _ -> failwithf "geconvertMainExprsrandToLocation: wrong operand %A" operand) 
            (Map.empty, Map.empty, Seq.length stackArgs)
    result, slots

/// Takes a program with computed interference graph
/// and allocates location for variables in the
/// registers or in the stack
let allocateRegisters (defs, main) =
    let handleArg env arg =
        match Map.tryFind arg env with
        | Some other -> 
            other
        | _ -> arg

    let handleInstr env (op, args) =
        op, List.map (handleArg env) args

    let handleDef def =
        let vars = getSortedVars def.InterfGraph def.StackArgs
        let initial = makeInitialColorMap def.InterfGraph (Seq.map Var def.StackArgs)
        let operandToColor = assignColors def.InterfGraph vars initial
        let operandToLocation, slots = geconvertMainExprsrandToLocation operandToColor def.StackArgs
        {def with Instrs = List.map (handleInstr operandToLocation) def.Instrs
                  SlotsOccupied = def.SlotsOccupied + slots}

    List.map handleDef defs, handleDef main

let convertSlots (defs, main) =
    let handleArg slots arg =
        match arg with
        | Slot n ->
            Deref((slots - n - 1) * wordSize, Rsp)
        | _ -> arg

    let handleInstr slots (op, args) =
        op, List.map (handleArg slots) args

    let handleDef def =
        {def with Instrs = List.map (handleInstr def.SlotsOccupied) def.Instrs}

    List.map handleDef defs, handleDef main


let stackCorrections (defs, main) =
    let restoreStack n = function
        | RestoreStack, [] -> Add, [Operand.Int n; Reg Rsp]
        | x -> x

    let handleDef def =
        let h, rest = List.head def.Instrs, List.tail def.Instrs
        let n = def.SlotsOccupied * wordSize
        let rest = List.map (restoreStack n) rest
        { def with Instrs = h :: (Sub, [Operand.Int n; Reg Rsp]) :: rest }

    let handleMain def =
        match def.Instrs with
        | (Label _, []) as i  :: rest->
            let n = def.MaxStack * wordSize
            let instrs = 
                [i
                 Push, [Reg R15]
                 Mov, [Reg Rsp; Reg R15]
                 Mov, [Reg Rcx; Reg Rsp]
                 Push, [Reg Rbp]
                 Push, [Reg R15]
                 Push, [Reg R14]
                 Push, [Reg R13]
                 Push, [Reg R12]
                 Push, [Reg Rbx]] @
                rest @
                [Pop, [Reg Rbx]
                 Pop, [Reg R12]
                 Pop, [Reg R13]
                 Pop, [Reg R14]
                 Pop, [Reg R15]
                 Pop, [Reg Rbp]
                 Mov, [Reg R15; Reg Rsp]
                 Pop, [Reg R15]
                 Ret, []]
            {def with Instrs = instrs}
        | _ -> failwith "addFunction...: expected label"

    List.map handleDef defs, handleMain main
    
let rec patchInstr (defs, main) =
    let filterMoves =
        List.filter (function
            | Mov, [arg1; arg2] when arg1 = arg2 -> false
            | _ -> true)

    let transform (op, args) =
        match op, args with
        | Lea _, [Reg _] -> [op, args]
        | Lea s, [arg] ->
            [Lea s, [Reg Rax]
             Mov, [Reg Rax; arg]]
        | op, [arg; arg2] when isRegister arg |> not && isRegister arg2 |> not ->
            [Mov, [arg; Reg Rax];
             op, [Reg Rax; arg2]]
        | e -> [e]

    let handleDef def =
        {def with Instrs = List.collect transform def.Instrs |> filterMoves}

    List.map handleDef defs, handleDef main