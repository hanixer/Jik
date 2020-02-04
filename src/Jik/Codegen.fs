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
      RootStackSlots : int
      RootStackVars : string list }

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
      RootStackSlots = 0
      RootStackVars = [] }

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

let alignStackPointer = [And, [Int -16; Reg Rsp]]

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
            Deref(-(n + 1) * wordSize, R15) // + 1 because closure pointer is store at 0(r15).
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

let allocateCons car cdr dest restoreStack =
    [Mov, [Int (3 * wordSize); Reg Rdx]
     Mov, [Reg Rsi; Deref(0, R15)]
     Mov, [Reg R15; Reg Rcx]
     Call("allocate"), []] @
    restoreStack @
    [Mov, [Deref(0, R15); Reg Rsi]
     Mov, [Reg Rax; Reg R11]
     Mov, [Int 0; Deref(0, R11)]
     Mov, [car; Deref(wordSize, R11)]
     Mov, [cdr; Deref(2 * wordSize, R11)]
     Or, [Int pairTag; Reg R11]
     Mov, [Reg R11; dest]]

/// Construct a list from arguments that belongs to the dotted argument,
/// (lambda (one . dotted) ...)
let constructDottedArgument args =
    let loopStart = freshLabel "loopStart"
    let loopEnd = freshLabel "loopEnd"
    let finalPos = -wordSize * (List.length args)
    // Stack is aligned. RSP is set to point before arguments,
    // so that call to allocate() won't clobber them.
    // Root stack is extended to store intermediate result list (R13),
    // because GC could remove that list.

    // R12 - points to argument on stack
    // R13 - previous cell or nil
    // R14 - number of remaining arguments
    // RBP - holds RSP.
    [Mov, [Reg Rcx; Reg Rax]
     Mov, [Reg Rcx; Reg R14]
     Sub, [Int (List.length args - 1); Reg R14]
     JmpIf(S, wrongArgCountHandler), []

     // Initialize registers.
     IMul, [Int wordSize; Reg Rax]
     Mov, [Reg Rsp; Reg R12]
     Sub, [Reg Rax; Reg R12] // point to the last argument
     Mov, [Int nilLiteral; Reg R13]
     Mov, [Reg Rsp; Reg Rbp]
     Sub, [Reg Rax; Reg Rsp]
     Sub, [Int wordSize; Reg Rsp]
     And, [Int -32; Reg Rsp]
     Sub, [Int (4*wordSize); Reg Rsp]
     Add, [Int wordSize; Reg R15]

     // Loop start.
     Label(loopStart), []
     Cmp, [Int 0; Reg R14]
     JmpIf(E, loopEnd), []
     Mov, [Reg R13; Deref(-wordSize, R15)]] @ // Save intermediate list.

    // Allocate cons.
    [Mov, [Int (3 * wordSize); Reg Rdx]
     Mov, [Reg Rsi; Deref(0, R15)]
     Mov, [Reg R15; Reg Rcx]
     Call("allocate"), []] @
    [Mov, [Deref(0, R15); Reg Rsi]
     Mov, [Deref(-wordSize, R15); Reg R13] // Restore intermediate list.
     Mov, [Reg Rax; Reg R11]
     Mov, [Int 0; Deref(0, R11)] // First cell of block.
     Mov, [(Deref(0, R12)); Deref(wordSize, R11)] // car
     Mov, [(Reg R13); Deref(2 * wordSize, R11)] // cdr
     Or, [Int pairTag; Reg R11]
     Mov, [Reg R11; Reg R13]

     Sub, [Int 1; Reg R14]  // Decrement arg counter.
     Add, [Int wordSize; Reg R12] // Increment pointer to next arg.

     // Repeat.
     Jmp(loopStart), []

     // Loop end.
     Label(loopEnd), []
     Sub, [Int wordSize; Reg R15]
     Mov, [Reg Rbp; Reg Rsp]
     Mov, [Reg R13; Deref(finalPos, Rsp)]]

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
         JmpIf(Ne, wrongArgCountHandler), []
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
        let stack = (def.SlotsOccupied + 1) * wordSize // What is + 1? Maybe for closure pointer rsi.
        let rootStack = (def.RootStackSlots + 1) * wordSize // + 1 because closure pointer is stored at 0(r15).
        let rest = List.collect (restoreStack stack rootStack) def.Instrs
        let instrs = argumentCheckAndStackAlloc def.Args def.IsDotted stack rootStack
        { def with Instrs = instrs @ rest }

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

/// Convert local variables and arguments to stack and root stack slots.
///
/// All arguments are moved to root stack slots.
/// Local variables that belongs to def.RootStackVars
/// are also stored on  root stack.
/// All other variables are stored on the normal stack.
/// Root count of stack vars also is set here.
let convertVarsToSlots (prog : Program) =
    let handleArg env arg =
        match arg with
        | Var var ->
            match Map.tryFind var env with
            | Some(slot) -> slot
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

    let makeEnv args rootStackVars vars =
        let addToEnv (env, stackI, rootI) var =
            if Map.containsKey var env then
                env, stackI, rootI
            else if Seq.contains var rootStackVars then
                Map.add var (RootStackSlot rootI) env, stackI, rootI + 1
            else
                Map.add var (Slot stackI) env, stackI + 1, rootI

        let initial = Seq.mapi (fun i arg -> arg, RootStackSlot i) args |> Map.ofSeq
        let env, _, rootCount = Seq.fold addToEnv (initial, Seq.length args, Seq.length args) vars
        env, rootCount

    let handleDef def =
        let vars = collectVars def.Instrs
        let env, rootCount = makeEnv def.Args def.RootStackVars vars
        let used = def.SlotsOccupied + env.Count // Probably, this can be just env.Count.
        let slots = if used % 2 <> 0 then used + 1 else used // To preserve alignment.
        let moveArgs = List.mapi (fun i _ -> Mov, [Slot i; RootStackSlot i]) def.Args
        let instrs = List.map (handleInstr env) def.Instrs
        {def with Instrs = moveArgs @ instrs
                  SlotsOccupied = slots
                  RootStackSlots = rootCount }

    { prog with Procedures = List.map handleDef prog.Procedures
                Main = handleDef prog.Main }

/// Removes saving values of global variables to temporary variables,
/// replace by direct access.
/// This is necessary for garbage collection.
/// Whitout it we will have broken pointers on the stack.
let convertGlobalRefs (prog : Program) =
    let env = System.Collections.Generic.Dictionary<string, string>()

    let handleArg arg =
        match arg with
        | Var var ->
            if env.ContainsKey(var) then
                GlobalValue(env.[var])
            else
                Var var
        | _ -> arg

    let handleInstr (instr : Instr) =
        match instr with
        | Mov, [GlobalValue(glob); Var dest] ->
            env.Add(dest, glob)
            []
        | op, args ->
            [op, List.map handleArg args]

    transformInstructions (List.collect handleInstr) prog

let createMainModule constants globals globOriginal entryPoints =
    let initGlobal name =
        [Mov, [Int undefinedLiteral; GlobalValue name]]

    let callEntryPoint entryPoint =
        [Mov, [Int 0; Reg Rcx]
         Call entryPoint, []]

    let calls = List.collect callEntryPoint entryPoints
    let initGlobals = Seq.collect initGlobal globals |> Seq.toList

    let instrs =
        [Push, [Reg R15]
         Mov, [Reg Rsp; Reg R15]
         Mov, [Reg Rcx; Reg Rsp]
         Push, [Reg Rbp]
         Push, [Reg R15]
         Push, [Reg R14]
         Push, [Reg R13]
         Push, [Reg R12]
         Push, [Reg Rbx]
         Mov, [GlobalValue rootStackBegin; Reg R15]
         Mov, [Int 0; Reg Rsi]] @
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
