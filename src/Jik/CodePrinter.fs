module CodePrinter

open Common
open Core
open RuntimeConstants
open Intermediate
open System.IO
open Codegen

let showInstr (out : TextWriter) (op, args) =
    // let showCc cc =
    //     match cc with
    //     | L -> out.Write("l")
    //     | L -> out.Write("l")

    let showOp op =
        match op with
        | Set cc ->
            out.Write("set")
            out.Write(cc.ToString().ToLower())
            out.Write(" ")
            // out
            // fprintf out "set%s " ((sprintf "%A" cc).ToLower())
        | Label label ->
            out.Write(label)
            out.Write(":")
            // fprintf out "%s:" label
        | JmpIf (E, label) ->
            out.Write("je ")
            out.Write(label)
        | JmpIf (Ne, label) ->
            out.Write("jne ")
            out.Write(label)
        | JmpIf (S, label) ->
            out.Write("js ")
            out.Write(label)
        | JmpIf (L, label) ->
            out.Write("jl ")
            out.Write(label)
        | Jmp label ->
            out.Write("jmp ")
            out.Write(label)
        | Call label ->
            out.Write("call ")
            out.Write(label)
        | Movb ->
            out.Write("movb ")
        | Cqto ->
            out.Write("cqto ")
        | IDiv ->
            out.Write("idivq ")
        | _ ->
            out.Write(op.ToString().ToLower())
            out.Write("q ")

    let reg r =
        out.Write("%")
        out.Write(r.ToString().ToLower())

    let showArg = function
        | Int n ->
            out.Write("$")
            out.Write(n)
        | Reg(r) -> reg r
        | Deref(n, r) ->
            out.Write(n)
            out.Write("(")
            reg r
            out.Write(")")
        | Deref4(n, r1, r2, m) ->
            out.Write(n)
            out.Write("(")
            reg r1
            out.Write(", ")
            reg r2
            out.Write(", ")
            out.Write(m)
            out.Write(")")
        | Var(v) ->
            out.Write("[var " + v + "]")
        | ByteReg(r) -> reg r
        | Slot(n) ->
            out.Write("{slot " + n.ToString() + "}")
        | RootStackSlot(n) ->
            out.Write("{root slot " + n.ToString() + "}")
        | GlobalValue(v) ->
            out.Write(v)
            out.Write("(%rip)")

    let showArgs args =
        List.iteri (fun i arg ->
            showArg arg
            if i <> List.length args - 1 then
                out.Write(", ")) args

    match op, args with
    | CallIndirect, [arg] ->
        out.Write("call *")
        showArg arg
    | JmpIndirect, [arg] ->
        out.Write("jmp *")
        showArg arg
    | Lea label, [arg] ->
        out.Write("leaq ")
        out.Write(label)
        out.Write("(%rip), ")
        showArg arg
    | _ ->
        showOp op
        showArgs args

let showInstrs (out : TextWriter) instrs =
    List.iter (fun instr ->
        out.Write("    ")
        showInstr out instr
        out.WriteLine()) instrs

let instrsToString instrs =
    let out = new StringWriter()
    fprintfn out "    .globl %s" schemeEntryLabel
    out.Write("    .globl ")
    out.WriteLine(schemeEntryLabel)
    out.Write(schemeEntryLabel)
    out.WriteLine(":")
    showInstrs out instrs
    out.ToString()

let printLabel (out : TextWriter) (label : string) =
        out.Write(label)
        out.WriteLine(":")

let printGlobal (out : TextWriter) (glob : string) =
        out.Write("    .globl ")
        out.WriteLine(glob)

let printReadOnlySec (out : TextWriter) =
    out.WriteLine("    .section .rdata,\"dr\"")

let printAlign  (out : TextWriter) (n : int) =
    out.Write("    .align ")
    out.WriteLine(n)

let printGlobalOriginals (out : TextWriter) originals globals =
    let printAsciiName i (label, orig : string) =
        printLabel out label
        out.Write("    .ascii \"")
        out.Write(orig)
        out.WriteLine("\\0\"")

    let printTableEntry (label : string, glob : string) =
        // Entry consists of global address and ascci label
        out.Write("    .quad ")
        out.WriteLine(glob)
        out.Write("    .quad ")
        out.WriteLine(label)

    let labels = List.map (fun _ -> freshLabel ".LC") originals

    printGlobal out globVarNameTable
    printReadOnlySec out
    Seq.iteri printAsciiName (Seq.zip labels originals)
    out.WriteLine("    .data")
    printAlign out 16
    printLabel out globVarNameTable
    Seq.iter printTableEntry (Seq.zip labels globals)
    out.WriteLine("    .quad 0") // Put zeros to know where table ends.
    out.WriteLine("    .quad 0")

let printGlobalRoots (out : TextWriter) constants globals =
    let printEntry (thing : string) =
        out.Write("    .quad ")
        out.WriteLine(thing)

    let label = freshLabel ".LC"
    printGlobal out globRootsTable
    printReadOnlySec out
    printLabel out label

    List.iter printEntry constants
    List.iter printEntry globals
    out.WriteLine("    .quad 0") // Put zeros to know where table ends.
    printLabel out globRootsTable
    out.Write("    .quad ")
    out.WriteLine(label)


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

    let printGlobal1 globl =
        printGlobal out globl
        out.WriteLine("    .bss")
        printAlign out wordSize
        printLabel out globl
        out.Write("    .space ")
        out.WriteLine(wordSize)

    let printStringConst (name, literal : string) =
        let firstField = literal.Length <<< fixnumShift
        fprintfn out "    .section .rdata,\"dr\""
        printReadOnlySec out

        printAlign out wordSize
        fprintfn out "%s:" name
        fprintfn out "    .quad %d" firstField
        fprintfn out "    .ascii \"%s\"" (escapeString literal)

    let handleDef def =
        out.WriteLine("    .text")
        printGlobal out def.Name
        printLabel out def.Name
        showInstrs out def.Instrs
        out.WriteLine()
        out.WriteLine()

    List.iter printStringConst prog.Strings

    if writeGlobals then
        printGlobalRoots out prog.ConstantsNames prog.Globals
        printGlobalOriginals out prog.GlobalsOriginal prog.Globals
        List.iter printGlobal1 prog.Globals
        List.iter printGlobal1 prog.ConstantsNames

    List.iter handleDef prog.Procedures

    if prog.Main.Name.Length > 0 then
        handleDef prog.Main
    showInstrs out prog.ErrorHandler
    out.ToString()

