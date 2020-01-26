module CodePrinter

open Common
open Core
open RuntimeConstants
open Intermediate
open System.IO
open Codegen

let showInstr out (op, args) =
    let showOp op =
        match op with
        | Set cc ->
            fprintf out "set%s " ((sprintf "%A" cc).ToLower())
        | Label label ->
            fprintf out "%s:" label
        | JmpIf (E, label) ->
            fprintf out "je %s" label
        | JmpIf (Ne, label) ->
            fprintf out "jne %s" label
        | JmpIf (S, label) ->
            fprintf out "js %s" label
        | Jmp label ->
            fprintf out "jmp %s" label
        | Call label ->
            fprintf out "call %s" label
        | Movb ->
            fprintf out "movb "
        | Cqto ->
            fprintf out "cqto "
        | IDiv ->
            fprintf out "idivq "
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

let printGlobalOriginals out originals globals =
    let printAsciiName i (label, orig) =
        fprintfn out "%s:" label
        fprintfn out "    .ascii \"%s\\0\"" orig

    let printTableEntry (label, glob) =
        // Entry consists of global address and ascci label
        fprintfn out "    .quad %s" glob
        fprintfn out "    .quad %s" label

    let labels = List.map (fun _ -> freshLabel ".LC") originals

    fprintfn out "    .globl %s" globVarNameTable
    fprintfn out "    .section .rdata,\"dr\""
    Seq.iteri printAsciiName (Seq.zip labels originals)
    fprintfn out "    .data"
    fprintfn out "    .align 16"
    fprintfn out "%s:" globVarNameTable
    Seq.iter printTableEntry (Seq.zip labels globals)
    fprintfn out "    .quad 0" // Put zeros to know where table ends.
    fprintfn out "    .quad 0"

let escapeString chars =
    let rec loop (acc : char list) chars =
        match chars with
        | [] ->
            let list = List.rev acc
            System.String.Concat(list)
        | '\\' :: cs -> loop ('\\' :: '\\' :: acc) cs
        | c :: cs -> loop (c :: acc) cs

    loop [] (Seq.toList chars)

let programToString writeGlobals (prog : Program) =
    let out = new StringWriter()

    let printGlobal globl =
        fprintfn out "    .globl %s" globl
        fprintfn out "    .bss"
        fprintfn out "    .align %d" wordSize
        fprintfn out "%s:" globl
        fprintfn out "    .space %d" wordSize

    let printConstant globl =
        fprintfn out "    .bss"
        fprintfn out "    .align %d" wordSize
        fprintfn out "%s:" globl
        fprintfn out "    .space %d" wordSize

    let printStringConst (name, literal : string) =
        let firstField = literal.Length <<< fixnumShift
        fprintfn out "    .section .rdata,\"dr\""
        fprintfn out "    .align %d" wordSize
        fprintfn out "%s:" name
        fprintfn out "    .quad %d" firstField
        fprintfn out "    .ascii \"%s\"" (escapeString literal)

    let handleDef def =
        fprintfn out "    .text"
        fprintfn out "    .globl %s" def.Name
        showInstrs out def.Instrs
        fprintfn out "\n\n"

    List.iter printStringConst prog.Strings
    List.iter printConstant prog.ConstantsNames

    if writeGlobals then
        printGlobalOriginals out prog.GlobalsOriginal prog.Globals
        List.iter printGlobal prog.Globals

    List.iter handleDef prog.Procedures

    if prog.Main.Name.Length > 0 then
        handleDef prog.Main
    showInstrs out prog.ErrorHandler
    out.ToString()

