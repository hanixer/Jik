module Codegen

open Core
open Graph
open RuntimeConstants
open Intermediate
open System.IO
open System.Collections.Generic
open Display
open Common
open Primitive

/// Code generation.
/// Language resembles x86 instruction set.
/// After conversion from Intermediate, 'Var' operands created.
/// Later these operands are changed to stack locations or registers.
///
/// Register %rsi is used as closure pointer.
/// It is saved and restored at each function call.
///
/// Calling function should pass number of arguments in %rcx,
/// so that called function could check if the number is correct.

type Register =
    | Rsp | Rbp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi
    | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    | Al | Ah | Bl | Bh | Cl | Ch

type Operand =
    | Int of int
    | Reg of Register
    | ByteReg of Register
    | Deref of int * Register
    | Deref4 of int * Register * Register * int
    | Var of string
    | Slot of int
    | RootStackSlot of int
    | GlobalValue of string

/// Condition for jump: Equal, Less than, Less than or equal, etc.
type Cc =
    | E
    | Ne
    | L
    | Le
    | G
    | Ge
    | S // if sign

type InstrName =
    | Add
    | Sub
    | Neg
    | Mov
    | IMul
    | IDiv
    | Cqto
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
    | Movb
    | Movzb
    | Jmp of string
    | JmpIndirect
    | JmpIf of Cc * string
    | Label of string
    | Lea of string
    // intermediate
    | RestoreStack
    | SpliceSlot of slot : int * argsCount : int

and Instr = InstrName * Operand list

type FunctionDef =
    { Name : string
      Free : string list
      Args : string list
      IsDotted : bool
      Instrs : Instr list
      InterfGraph : Graph<Operand>
      LiveBefore : Map<int, Set<Operand>>
      LiveAfter : Map<int, Set<Operand>>
      SlotsOccupied : int
      RootStackSlots : int }

type Program =
    { Procedures : FunctionDef list
      Main : FunctionDef
      Globals : string list
      GlobalsOriginal : string list // Contains original scheme names, for error reporting.
      ConstantsNames : string list
      ErrorHandler : Instr list
      Entry : string
      Strings : (string * string) list }

let emptyFuncDef =
    { Name = ""
      Free = []
      Args = []
      IsDotted = false
      InterfGraph = makeGraph []
      Instrs = []
      LiveBefore = Map.empty
      LiveAfter = Map.empty
      SlotsOccupied = 0
      RootStackSlots = 0 }

let allRegisters =
    [Rsp; Rbp; Rax; Rbx; Rcx; Rdx; Rsi; Rdi;
     R8; R9; R10; R11; R12; R13; R14; R15;
     Al; Ah; Bl; Bh; Cl; Ch]

let registersForArgs =
    [Rcx; Rdx; R8; R9]

let callerSave =
    [R10; R11; R8; R9; Rcx; Rdi; Rdx]

let calleeSave =
    [R12; R13; R14; R15; Rbp; Rbx]

let isArithm = function
    | Add | Sub | Neg | Sar | Sal -> true
    | _ -> false

let isRegister = function | Reg _ -> true | _ -> false


let getCar reg dest = Mov, [Deref(-pairTag + wordSize, reg); dest]
let getCdr reg dest = Mov, [Deref(-pairTag + 2 * wordSize, reg); dest]

/// Applies transformations to instructions.
let transformInstructions handleInstrs (prog : Program) =
    let handleDef def =
        {def with Instrs = handleInstrs def.Instrs}

    { prog with Procedures = List.map handleDef prog.Procedures
                Main = handleDef prog.Main }

let revealGlobals (prog : Program) =
    let convertArg = function
        | Var var when List.contains var prog.Globals ->
            GlobalValue var
        | arg -> arg

    let convertInstr (op, args) =
        op, List.map convertArg args

    let handleInstrs instrs = List.map convertInstr instrs

    transformInstructions handleInstrs prog

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

/// Splice the last slot to support apply function.
let spliceSlot stackOffset argsCount =
    // r8 - counts number of elements in list
    // r9 - points to stack position to put an element
    let loopBegin = freshLabel "loopBegin"
    let loopEnd = freshLabel "loopEnd"
    [Mov, [Reg Rsp; Reg R9]
     Add, [Int stackOffset; Reg R9]
     Mov, [Deref(0, R9); Reg Rdx]
     Mov, [Int 0; Reg R8]
     Label loopBegin, []
     Mov, [Int nilLiteral; Reg Rax]
     Cmp, [Reg Rdx; Reg Rax]
     JmpIf(E, loopEnd), []
     getCar Rdx (Reg Rax)
     Add, [Int 1; Reg R8] // Change argument counter
     Mov, [Reg Rax; Deref(0, R9)]
     Sub, [Int wordSize; Reg R9] // Go to next stack slot
     getCdr Rdx (Reg Rdx)
     Jmp(loopBegin), []
     Label(loopEnd), []
     Mov, [Int (argsCount - 1); Reg Rcx]
     Add, [Reg R8; Reg Rcx]]

let convertSlots (prog : Program) =
    let handleArg slots rootSlots arg =
        match arg with
        | Slot n ->
            Deref((slots - n) * wordSize, Rsp)
        | RootStackSlot n ->
            Deref(-n * wordSize, R15)
        | _ -> arg

    let handleInstr slots rootSlots (op, args) =
        op, List.map (handleArg slots rootSlots) args

    let splice slots instr =
        match instr with
        | SpliceSlot(slot, argsCount), _ ->
            let stackOffset = (slots - slot) * wordSize
            spliceSlot stackOffset argsCount
        | _ -> [instr]

    let handleInstrs slots rootSlots instrs =
        let instrs' = List.collect (splice slots) instrs
        List.map (handleInstr slots rootSlots) instrs'

    let handleDef def =
        {def with Instrs = handleInstrs def.SlotsOccupied def.RootStackSlots def.Instrs}

    { prog with Procedures = List.map handleDef prog.Procedures
                Main = handleDef prog.Main }

let allocateCons car cdr dest =
     [Mov, [GlobalValue(freePointer); Reg R11]
      Mov, [Int 0; Deref(0, R11)]
      Mov, [car; Deref(wordSize, R11)]
      Mov, [cdr; Deref(2 * wordSize, R11)]
      Mov, [Reg R11; dest]
      Or, [Int pairTag; dest]
      Add, [Int (3 * wordSize); GlobalValue(freePointer)]]

let constructDottedArgument args =
    let loopStart = freshLabel "loopStart"
    let loopEnd = freshLabel "loopEnd"
    let finalPos = -wordSize * (List.length args)

    // r9 - points to argument on stack
    // r11 - points to allocated cell
    // r10 - previous cell or nil
    // rcx - number of remaining arguments
    [Mov, [Reg Rcx; Reg Rax]
     Sub, [Int (List.length args - 1); Reg Rcx]
     JmpIf(S, errorHandlerLabel), []

     // Initialize registers.
     IMul, [Int wordSize; Reg Rax]
     Mov, [Reg Rsp; Reg R9]
     Sub, [Reg Rax; Reg R9] // point to the last argument
     Mov, [Int nilLiteral; Reg R10]

     // Loop start.
     Label(loopStart), []
     Cmp, [Int 0; Reg Rcx]
     JmpIf(E, loopEnd), []] @

    // Allocate cons.
    allocateCons (Deref(0, R9)) (Reg R10) (Reg R10) @

    // Decrement arg counter, increment pointer to next arg.
    [Sub, [Int 1; Reg Rcx]
     Add, [Int wordSize; Reg R9]

     // Repeat.
     Jmp(loopStart), []
     // loop end
     Label(loopEnd), []
     Mov, [Reg R10; Deref(finalPos, Rsp)]]

/// If function has variable arity (isDotted = true)
/// then construct last argument as list,
/// otherwise just check for number of arguments.
/// Allocate stack space for local variables.
let argumentCheckAndStackAlloc args isDotted space rootStack =
    if isDotted then
        constructDottedArgument args @
        [Sub, [Int space; Reg Rsp]
         Add, [Int rootStack; Reg R15]]
    else
        [Cmp, [Int (List.length args); Reg Rcx]
         JmpIf(Ne, errorHandlerLabel), []
         Sub, [Int space; Reg Rsp]
         Add, [Int rootStack; Reg R15]]

/// Add code in the beginning and the end of functions:
/// stack corrections, number of arguments checking,
/// dotted arguments construction.
let addFuncPrologAndEpilog (prog : Program) =
    let restoreStack stack rootStack = function
        | RestoreStack, [] ->
            [Add, [Int stack; Reg Rsp]
             Sub, [Int rootStack; Reg R15]]

        | x -> [x]

    let handleDef def =
        let firstInstr, rest = List.head def.Instrs, List.tail def.Instrs
        let stack = (def.SlotsOccupied + 1) * wordSize
        let rootStack = def.RootStackSlots * wordSize
        let rest = List.collect (restoreStack stack def.RootStackSlots) rest
        let instrs = argumentCheckAndStackAlloc def.Args def.IsDotted stack rootStack
        { def with Instrs = firstInstr :: instrs @ rest }

    { prog with Procedures = List.map handleDef prog.Procedures }

let rec patchInstr (prog : Program) =
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
        | op, [Int _; _] -> [op, args]
        | Movzb, [arg; arg2] when isRegister arg2 |> not ->
            [Movzb, [arg; Reg Rax]
             Mov, [Reg Rax; arg2]]
        | op, [arg; arg2] when isRegister arg |> not && isRegister arg2 |> not ->
            [Mov, [arg; Reg Rax]
             op, [Reg Rax; arg2]]
        | e -> [e]

    let handleInstrs instrs = List.collect transform instrs |> filterMoves

    transformInstructions handleInstrs prog

let convertVarsToSlots (prog : Program) =
    let handleArg env arg =
        match arg with
        | Var var ->
            match Map.tryFind var env with
            | Some(slot) -> Slot slot
            | _ ->
                failwithf "var %s was not found in env %A" var env
        | _ -> arg

    let handleInstr env (op, args) =
        op, List.map (handleArg env) args

    let collectVars instrs =
        let vars = seq {
            for (_, operands) in instrs do
                yield! Seq.choose (function Var n -> Some n | _ -> None) operands
        }
        Set.ofSeq vars

    let makeEnv args vars =
        let addToEnv (env, i) var =
            if Map.containsKey var env then
                env, i
            else
                Map.add var i env, i + 1

        let initial = Seq.mapi (fun i arg -> arg, i) args |> Map.ofSeq
        let env, _ = Seq.fold addToEnv (initial, initial.Count) vars
        env

    let handleDef def =
        let vars = collectVars def.Instrs
        let env = makeEnv def.Args vars
        let used = def.SlotsOccupied + env.Count
        let slots = if used % 2 <> 0 then used + 1 else used // To preserve alignment.
        {def with Instrs = List.map (handleInstr env) def.Instrs
                  SlotsOccupied = slots }

    { prog with Procedures = List.map handleDef prog.Procedures
                Main = handleDef prog.Main }

let createMainModule constants globals globOriginal entryPoints =
    let initGlobal name =
        [Mov, [Int undefinedLiteral; GlobalValue name]]

    let callEntryPoint entryPoint =
        [Mov, [Int 0; Reg Rcx]
         Call entryPoint, []]

    let calls = List.collect callEntryPoint entryPoints
    let initGlobals = Seq.collect initGlobal globals |> Seq.toList

    let instrs =
        [Label(schemeEntryLabel), []
         Push, [Reg R15]
         Mov, [Reg Rsp; Reg R15]
         Mov, [Reg Rcx; Reg Rsp]
         Push, [Reg Rbp]
         Push, [Reg R15]
         Push, [Reg R14]
         Push, [Reg R13]
         Push, [Reg R12]
         Push, [Reg Rbx]
         Mov, [GlobalValue rootStackBegin; Reg R15]] @
        initGlobals @
        calls @
        [Pop, [Reg Rbx]
         Pop, [Reg R12]
         Pop, [Reg R13]
         Pop, [Reg R14]
         Pop, [Reg R15]
         Pop, [Reg Rbp]
         Mov, [Reg R15; Reg Rsp]
         Pop, [Reg R15]
         Ret, []]

    let funcDef =
        { emptyFuncDef with
            Name = schemeEntryLabel
            Instrs = instrs }

    { Procedures = []
      Main = funcDef
      Globals = List.ofSeq globals
      GlobalsOriginal = List.ofSeq globOriginal
      ConstantsNames = constants
      ErrorHandler = []
      Entry = schemeEntryLabel
      Strings = [] }
