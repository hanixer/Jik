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
/// Register %rsi is used as closure pointer.
/// It is saved and restored at each function call.

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
    | IMul
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
      SlotsOccupied : int }

type Program =
    { Procedures : FunctionDef list
      Main : FunctionDef
      Globals : string list
      ErrorHandler : Instr list }

let freePointer = "freePointer"

let errorHandlerLabel = ".L_errorHandler"

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
        | Movb ->
            fprintf out "movb "
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

let programToString (prog : Program) =
    let out = new StringWriter()

    let printGlobal globl =
        fprintfn out "    .globl %s" globl
        fprintfn out "    .bss"
        fprintfn out "    .align %d" wordSize
        fprintfn out "%s:" globl
        fprintfn out "    .space %d" wordSize

    let handleDef def =
        fprintfn out "    .text"
        fprintfn out "    .globl %s" def.Name
        showInstrs out def.Instrs
        fprintfn out "\n\n"

    List.iter printGlobal prog.Globals
    List.iter handleDef prog.Procedures
    handleDef { prog.Main with Name = schemeEntryLabel }
    showInstrs out prog.ErrorHandler
    out.ToString()

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

let convertSlots (prog : Program) =
    let handleArg slots arg =
        match arg with
        | Slot n ->
            Deref((slots - n) * wordSize, Rsp)
        | _ -> arg

    let handleInstr slots (op, args) =
        op, List.map (handleArg slots) args

    let handleInstrs instrs = List.map handleInstr instrs

    let handleDef def =
        {def with Instrs = List.map (handleInstr def.SlotsOccupied) def.Instrs}

    { prog with Procedures = List.map handleDef prog.Procedures
                Main = handleDef prog.Main }

let stackCorrections (prog : Program) =
    let restoreStack n = function
        | RestoreStack, [] -> Add, [Operand.Int n; Reg Rsp]
        | x -> x

    let handleDef def =
        let h, rest = List.head def.Instrs, List.tail def.Instrs
        let n = (def.SlotsOccupied + 1) * wordSize
        let rest = List.map (restoreStack n) rest
        { def with Instrs = h :: (Sub, [Operand.Int n; Reg Rsp]) :: rest }

    let handleMain def =
        match def.Instrs with
        | (Label _, []) as i  :: rest->
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

    { prog with Procedures = List.map handleDef prog.Procedures
                Main = handleMain prog.Main }

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
        | op, [Int _; arg2] -> [op, args]
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
        {def with Instrs = List.map (handleInstr env) def.Instrs
                  SlotsOccupied = def.SlotsOccupied + env.Count }

    { prog with Procedures = List.map handleDef prog.Procedures
                Main = handleDef prog.Main }