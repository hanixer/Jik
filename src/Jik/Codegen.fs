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
/// Calling convention:
/// all arguments are passed via stack.
/// 
/// higher address
/// Caller:
/// return point
/// arg1                                               40(rsp)
/// arg2                                               32(rsp)
/// local1                                             24(rsp)
/// local2                                             16(rsp)
/// local3                                              8(rsp)
/// ---> place for return point <--- sp points here --- 0(rsp)
/// Callee-arg1                                        -8(rsp)
/// Callee-arg2                                       -16(rsp)
/// ...
/// lower address

/// N - arguments count
/// M - locals count
/// offset for local i(1..M) : (M - i + 1) * 8
/// offset for argument j(1..N) : (M + N - j + 1) * 8


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
    | Var of Var

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
    | Call
    | CallIndirect
    | Push
    | Pop
    | Ret
    | Xor
    | Cmp
    | Set of Cc
    | Movzb
    | Jmp of string
    | JmpIf of Cc * string
    | Label of string
    | Lea of string

and Instr = InstrName * Operand list

type FunctionDef = 
    { Name : string
      Args : string list
      Instrs : Instr list
      Vars : string list ref
      MaxStack : int
      InterfGraph : Graph<string>
      LiveBefore : Map<int, Set<Operand>>
      LiveAfter : Map<int, Set<Operand>> }

type Program = FunctionDef list * FunctionDef

let allRegisters = [Rsp; Rbp; Rax; Rbx; Rcx; Rdx; Rsi; Rdi; 
                    R8; R9; R10; R11; R12; R13; R14; R15; 
                    Al; Ah; Bl; Bh; Cl; Ch]

let isArithm = function
    | Add | Sub | Neg | Sar | Sal -> true
    | _ -> false

let showInstr out (op, args) =
    let showOp op =
        match op with
        | Set cc ->
            fprintf out "set%s " ((sprintf "%A" cc).ToLower())
        | Label label ->
            fprintfn out ""
            fprintf out "%s:" label
        | JmpIf (E, label) ->
            fprintf out "je %s" label
        | Jmp label ->
            fprintf out "jmp %s" label
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
        | Var(v) -> fprintf out "[var %s]" v
        | ByteReg(r) -> fprintf out "%s" (reg r)
        
    let showArgs args =
        List.iteri (fun i arg -> 
            showArg arg
            if i <> List.length args - 1 then
                fprintf out ", ") args

    match op, args with
    | CallIndirect, [arg] ->
        fprintf out "call *"
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
    let registersForArgs = [Rcx; Rdx; R8; R9]

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
        | e -> failwithf "handleDecl: %s %A" var e

    let handleTransfer labels = function
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
             Cmp, [Operand.Int falseLiteral; Reg Rax]
             JmpIf (E, labelf), []]
        | Transfer.Call(NonTail label, func, args) ->
            let argToLoc = argumentToLocation (-wordSize, -wordSize) Rsp args
            let moveToArgPositions =
                List.map (fun arg -> Mov, [Var arg; Map.find arg argToLoc]) args                        
            let afterCall =
                    let argsOfLabel = getArgsOfLabel labels label
                    let argOfLabel = List.head argsOfLabel
                    if List.length argsOfLabel <> 1 then 
                        failwith "handleTransfer: wrong number of vars"
                    [Mov, [Reg Rax; Var argOfLabel]
                     InstrName.Jmp label, []]
            moveToArgPositions @ [CallIndirect, [Var func]] @ afterCall
        | Transfer.Call(Tail, func, args) ->
            // let argToLoc = argumentToLocation (0, -wordSize) Rsp args
            // let moveToTempPositions =
            //     List.map (fun arg -> Mov, [Var arg; Map.find arg argToLoc]) args
            // let moveToArgPositions =
            //     List.mapi (fun i arg -> Mov, )
            failwith ""


    let handleStmt labels = function
        | Decl decl -> handleDecl decl
        | Transfer tran -> handleTransfer labels tran

    let handleLabel labels (name, _, stmts) =
        (InstrName.Label name, []) :: List.collect (handleStmt labels) stmts

    let saveArgs args instrs =
        let argToLoc = argumentToLocation (2 * wordSize, wordSize) Rbp args
        let saveInstrs = 
            List.map (fun arg -> Mov, [Map.find arg argToLoc; Var arg]) args
        match instrs with
        | head :: tail -> head :: saveInstrs @ tail
        | _ -> failwithf "saveArgs: wrong at least one instr expected"

    let handleDef (name, args, labels) =
        let maxArgs = countMaxArgs labels
        let varsCount = countVars args labels
        let instrs =
            List.collect (handleLabel labels) labels
        let graph = buildInterference labels

        printDot graph (__SOURCE_DIRECTORY__ + "/../../misc/graphs/" + name + ".dot")

        { Name = name
          Args = args
          Vars = ref []
          MaxStack = maxArgs
          InterfGraph = graph
          Instrs = instrs
          LiveBefore = Map.empty
          LiveAfter = Map.empty }

    List.map handleDef defs, handleDef (schemeEntryLabel, [], labels)

let collectVars instrs =
    List.map snd instrs
    |> List.map (fun args ->
        List.map (fun arg ->
            match arg with
            | Var var -> Set.singleton var
            | _ -> Set.empty) args
        |> Set.unionMany)
    |> Set.unionMany

/// Takes interference graph and all used variables
/// and returns mapping from variables to their colors
let colorGraph graph vars =
    let banned = Dictionary<string, Set<int>>()
    Seq.iter (fun var -> banned.Add(var, Set.empty)) vars

    let pickNode nodes =
        Seq.maxBy (fun node -> 
            banned.Item node |> Seq.length) nodes

    let rec findLowestColor bannedSet =
        Seq.initInfinite id
        |> Seq.find (fun x -> Seq.exists ((=) x) bannedSet |> not)

    let updateBanned node color =
        adjacent graph node
        |> Seq.iter (fun v -> 
            banned.Item v <- banned.Item v |> Set.add color)

    let rec loop nodes color =
        if Set.isEmpty nodes then
            color
        else
            let node = pickNode nodes
            let lowest = findLowestColor (banned.Item node)
            let color = Map.add node lowest color
            updateBanned node lowest
            loop (Set.remove node nodes) color
    
    loop (Set.ofSeq vars) Map.empty

let calculateVarToLocEnv graph instrs =
    let registers = []

    let rec foldRemaining (acc, remaining, stackIndex) color =
        match remaining with
        | reg :: rest ->
            (Map.add color (Reg reg) acc, rest, stackIndex)
        | _ ->
            let stackIndex = stackIndex - wordSize
            (Map.add color (Deref(stackIndex, Rbp)) acc, [], stackIndex)
    
    let getColorToLoc color =
        let colorSet = 
            Map.toList color 
            |> List.map snd 
            |> Set.ofList
        let toLoc, _, _ = 
            Set.fold foldRemaining (Map.empty, registers, 0) colorSet
        toLoc    
        
    let varToColor = colorGraph graph (collectVars instrs)
    let colorToLoc = getColorToLoc varToColor        
    let env = Map.map (fun _ color -> Map.find color colorToLoc) varToColor
    let numStackLocals = (Map.count colorToLoc) - (List.length registers)

    env, numStackLocals

/// Assign registers or stack locations
/// for variables using graph coloring
let allocateLocals (defs, main) =
    // let handleArg env = function
    //     | Var var -> Map.find var env
    //     | arg -> arg

    // let handleInstr env (op, args) =
    //     op, List.map (handleArg env) args

    // let handleFunctionDef (def : FunctionDef) =
    //     let env, numStackLocals = calculateVarToLocEnv def.InterfGraph def.Instrs
    //     let instrs = List.map (handleInstr env) def.Instrs
    //     let maxStack = def.MaxStack + numStackLocals
    //     {def with Instrs = instrs
    //               MaxStack = maxStack}

    // List.map handleFunctionDef defs, handleFunctionDef main
    failwith ""

let addFunctionBeginEnd (defs, main) =
    let functionBegin isMain maxStack =
        let n = maxStack * wordSize
        let additional =
            if isMain 
            then [Mov, [Reg Rsp; Reg R15]
                  Mov, [Reg Rcx; Reg Rsp]
                  Push, [Reg R15]
                  Mov, [Reg Rsp; Reg Rbp]] 
            else []

        [Push, [Reg Rbp]
         Mov, [Reg Rsp; Reg Rbp]
         Push, [Reg R15]
         Push, [Reg R14]
         Push, [Reg R13]
         Push, [Reg R12]
         Push, [Reg Rbx]] @
         additional @
         [Sub, [Operand.Int n; Reg Rsp]]

    let functionEnd isMain maxStack =
        let n = maxStack * wordSize
        let additional =
            if isMain 
            then [Pop, [Reg R15]
                  Mov, [Reg R15; Reg Rsp]]
            else []

        [Add, [Operand.Int n; Reg Rsp]] @
        additional @
        [Pop, [Reg Rbx]
         Pop, [Reg R12]
         Pop, [Reg R13]
         Pop, [Reg R14]
         Pop, [Reg R15]
         Pop, [Reg Rbp]
         Ret, []]

    let handleDef def =
        match def.Instrs with
        | (Label _, []) as i  :: rest->
            let isMain = def.Name = schemeEntryLabel
            let instrs = 
                [i] @
                (functionBegin isMain def.MaxStack) @ 
                rest @ 
                (functionEnd isMain def.MaxStack)
            {def with Instrs = instrs}
        | _ -> failwith "addFunction...: expected label"

    List.map handleDef defs, handleDef main
    
let rec patchInstr (defs, main) =
    let transform = function
        | op, [Deref(n, reg1); Deref(m, reg2)] ->
            [Mov, [Deref(n, reg1); Reg Rax];
             op, [Reg Rax; Deref(m, reg2)]]
        | e -> [e]

    let handleDef def =
        {def with Instrs = List.collect transform def.Instrs}

    List.map handleDef defs, handleDef main

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

let callerSave = [R10; R11]

let computeLiveAfter2 def =
    // let instrs = def.Instrs |> List.toArray
    // let todo = [0..instrs.Length - 1] |> Set.ofList
    // let preds = computePreds instrs

    // let liveIn = System.Collections.Generic.Dictionary()
    // let liveOut = System.Collections.Generic.Dictionary()

    // Array.iteri (fun i _ -> 
    //     liveIn.Add i (System.Collections.Generic.HashSet())
    //     liveOut.Add i (System.Collections.Generic.HashSet())) instrs

    // let written (op, args) = 
    //     match op with
    //     | Add | Sub | Mov | Sar | Sal | And | Or | Xor | Movzb | Cmp ->
    //         List.item 1 args
    //     | Neg | Set _ | Lea -> List.head args
    //     | Call | CallIndirect -> callerSave |> List.map Reg
    //     | _ -> []

    // let read (op, args) =

    // let rec loop todo =
    //     if Seq.isEmpty todo then
    //         ()
    //     else
    //         let curr = Seq.head todo
    //         let todo = Seq.tail todo



    // Array.rev
    failwith ""




let rec computeLiveAfter (defs, main) =
    let isRegVar = 
        function 
        | Reg _ | Var _ -> true 
        | _ -> false

    let writtenBy (op, args)  = 
        match op with
        | Add | Sub | Mov | Sar | Sal | And | Or | Xor | Movzb | Cmp ->
            List.item 1 args|> Set.singleton
        | Neg | Set _ | Lea _ -> List.head args |> Set.singleton
        | Call | CallIndirect -> callerSave |> List.map Reg |> Set.ofList
        | _ -> Set.empty
        |> Set.filter isRegVar

    let readBy (op, args) =
        match op with
        | Call | CallIndirect ->
            Set.ofList callerSave |> Set.map Reg
        | Mov | Movzb -> List.head args |> Set.singleton
        | Set _ -> Set.empty
        | Ret -> Set.singleton (Reg Rax)
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
        let todo, afterMap = updatePreds (Map.find index preds) afterMap beforeMap index
        todo, afterMap, beforeMap

    let rec loop instrs preds todo afterMap beforeMap =
        if Set.isEmpty todo then
            afterMap, beforeMap
        else
            let index = Set.maxElement todo
            let todo1 = Set.remove index todo
            let todo2, liveAfterMap, liveBeforeMap = handleInstr instrs preds afterMap beforeMap index
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
    let filterAndAddEdges graph ignore live target =
        Set.difference live (Set.ofList ignore)
        |> Set.iter (addEdge graph target)

    let iter live graph index (op, args) =
        let live =            
            match Map.tryFind index live with
            | Some vl -> vl
            | None _ ->
                failwithf "buildInterference: iter: index=%d live=%A size=%d" index live (Map.count live)                
        match args with
        | [_; Var _ as arg] | [_; Reg _ as arg] ->
            filterAndAddEdges graph [] live arg
        | [Var _ as arg] | [Reg _ as arg] ->
            filterAndAddEdges graph [] live arg
        | _ -> ()

    let handleDef def =
        let vars = collectVars def.Instrs |> Set.map Var
        let regs = Set.ofList allRegisters |> Set.map Reg
        let graph = makeGraph (Set.union vars regs)
        Seq.iteri (iter def.LiveAfter graph) def.Instrs
        printDotFunc (function | Var var -> var | Reg reg -> reg.ToString().ToLower() | _ -> "ERROR") graph (__SOURCE_DIRECTORY__ + "/../../misc/graphs/" + def.Name + ".dot")
        def

    List.map handleDef defs, handleDef main

/// Takes interference graph and all used variables
/// and returns mapping from variables to their colors
// let colorGraph graph vars =
//     let banned = Dictionary<string, Set<int>>()
//     Seq.iter (fun var -> banned.Add(var, Set.empty)) vars

//     let pickNode nodes =
//         Seq.maxBy (fun node -> 
//             banned.Item node |> Seq.length) nodes

//     let rec findLowestColor bannedSet =
//         Seq.initInfinite id
//         |> Seq.find (fun x -> Seq.exists ((=) x) bannedSet |> not)

//     let updateBanned node color =
//         adjacent graph node
//         |> Seq.iter (fun v -> 
//             banned.Item v <- banned.Item v |> Set.add color)

//     let rec loop nodes color =
//         if Set.isEmpty nodes then
//             color
//         else
//             let node = pickNode nodes
//             let lowest = findLowestColor (banned.Item node)
//             let color = Map.add node lowest color
//             updateBanned node lowest
//             loop (Set.remove node nodes) color
    
//     loop (Set.ofSeq vars) Map.empty

/// Assign registers or stack locations
/// for variables using graph coloring
// let allocateRegisters (instrs, graph) =
//     let color = colorGraph graph (collectVars instrs)
//     let registers = [Rbx]

//     let rec fold (acc, remaining, stackIndex) _ color =
//         match remaining with
//         | reg :: rest ->
//             (Map.add color (Reg reg) acc, rest, stackIndex)
//         | _ ->
//             let stackIndex = stackIndex - wordSize
//             (Map.add color (Deref(stackIndex, Rbp)) acc, [], stackIndex)

//     let colorToLocation, _, _ = 
//         Map.fold fold (Map.empty, registers, 0) color

//     let handleArg = function
//         | Var var ->
//             let colorValue = Map.find var color
//             Map.find colorValue colorToLocation
//         | arg -> arg

//     let handleInstr(op, args) =
//         op, List.map handleArg args

//     List.map handleInstr instrs


let allocateRegisters (defs, main) =
    let handleDef def =
        def
    List.map handleDef defs, handleDef main