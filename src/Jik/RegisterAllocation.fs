module RegisterAllocation

open Core
open Graph
open RuntimeConstants
open Intermediate
open Codegen
open System.IO
open System.Collections.Generic

let collectVars instrs =
    List.map snd instrs
    |> List.map (fun args ->
        List.map (fun arg ->
            match arg with
            | Var var -> Set.singleton var
            | _ -> Set.empty) args
        |> Set.unionMany)
    |> Set.unionMany

let rec computeLiveness (prog : Program) =
    let isRegVar =
        function
        | Reg _ | Var _ -> true
        | _ -> false

    let writtenBy (op, args)  =
        match op with
        | Add | Sub | Mov | Sar | Sal | And | Or | Xor | Movzb | Movb | Cmp ->
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
        | Mov | Movzb | Movb -> List.head args |> Set.singleton
        | Set _ -> Set.empty
        | Ret -> Set.ofList (Rax :: calleeSave) |> Set.map Reg
        | JmpIndirect -> Set.ofList (Rax :: calleeSave) |> Set.map Reg
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

    { prog with Procedures = List.map handleDef prog.Procedures
                Main = handleDef prog.Main }

let rec buildInterference (prog : Program) : Program =
    let registersForUse =
        [Rbx; Rcx; Rdx; Rdi; Rbp;
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

    { prog with Procedures = List.map handleDef prog.Procedures
                Main = handleDef prog.Main }

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
        |> Seq.find (fun x ->
            Seq.exists ((=) x) bannedSet |> not)

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
let allocateRegisters (prog : Program) =
    let handleArg env arg =
        match Map.tryFind arg env with
        | Some other ->
            other
        | _ -> arg

    let handleInstr env (op, args) =
        op, List.map (handleArg env) args

    let handleDef def =
        // def.Args referenced here should be only arguments that do not feet into registers
        // and go to stack
        let vars = getSortedVars def.InterfGraph def.Args
        let initial = makeInitialColorMap def.InterfGraph (Seq.map Var def.Args)
        let operandToColor = assignColors def.InterfGraph vars initial
        let operandToLocation, slots = geconvertMainExprsrandToLocation operandToColor def.Args
        {def with Instrs = List.map (handleInstr operandToLocation) def.Instrs
                  SlotsOccupied = def.SlotsOccupied + slots}

    { prog with Procedures = List.map handleDef prog.Procedures
                Main = handleDef prog.Main }
