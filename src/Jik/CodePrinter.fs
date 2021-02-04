module CodePrinter

open Common
open Core
open RuntimeConstants
open Intermediate
open System.IO
open Codegen
open System


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

let showInstr (out : TextWriter) (op, args) =
    let showCc cc =
        match cc with
        | L -> out.Write("l")
        | G -> out.Write("g")
        | E -> out.Write("e")
        | Ne -> out.Write("ne")
        | Le -> out.Write("le")
        | Ge -> out.Write("ge")
        | S -> out.Write("s")
        | A -> out.Write("a")
        | Ae -> out.Write("ae")
        | B -> out.Write("b")
        | Be -> out.Write("be")

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
        | Add -> out.Write("addq ")
        | Sub -> out.Write("subq ")
        | Neg -> out.Write("negq ")
        | Mov -> out.Write("movq ")
        | Movzb -> out.Write("movzbq ")
        | IMul -> out.Write("imulq ")
        | Sar -> out.Write("sarq ")
        | Sal -> out.Write("salq ")
        | And -> out.Write("andq ")
        | Or -> out.Write("orq ")
        | Push -> out.Write("pushq ")
        | Pop -> out.Write("popq ")
        | Ret -> out.Write("retq ")
        | Xor -> out.Write("xorq ")
        | Cmp -> out.Write("cmpq ")
        | Pxor -> out.Write("pxor ")
        | ConvertFloatToInt -> out.Write("cvttsd2siq ")
        | ConvertIntToFloat -> out.Write("cvtsi2sdq ")
        | FloatCompare -> out.Write("ucomisd ")
        | Movsd -> out.Write("movsd ")
        | Addsd -> out.Write("addsd ")
        | Subsd -> out.Write("subsd ")
        | Mulsd -> out.Write("mulsd ")
        | Divsd -> out.Write("divsd ")
        | RestoreStack -> out.Write("restorestack ")
        | SpliceSlot(_, _) -> out.Write("spliceslot ")
        | aaa ->
            failwithf "showOp: this instruction should not be handled here %A" aaa
        // | _ ->
            // out.Write(op.ToString().ToLower())
            // out.Write("q ")

    let reg r =
        out.Write("%")
        let s =
            match r with
            | Rax -> "rax"
            | Rbx -> "rbx"
            | Rsp -> "rsp"
            | Rbp -> "rbp"
            | Rcx -> "rcx"
            | Rdx -> "rdx"
            | Rsi -> "rsi"
            | Rdi -> "rdi"
            | R8 -> "r8"
            | R9 -> "r9"
            | R10 -> "r10"
            | R11 -> "r11"
            | R12 -> "r12"
            | R13 -> "r13"
            | R14 -> "r14"
            | R15 -> "r15"
            | Al -> "al"
            | Ah -> "ah"
            | Bl -> "bl"
            | Bh -> "bh"
            | Cl -> "cl"
            | Ch -> "ch"
            | Xmm0 -> "xmm0"
            | Xmm1 -> "xmm1"
            | Xmm2 -> "xmm2"
            | Xmm3 -> "xmm3"

        out.Write(s)

    let showArg = function
        | Int n ->
            out.Write("$")
            out.Write(n)
        | Int64 n ->
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
        | FloatNumber n ->
            out.Write("[float " + n.ToString() + "]")

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
    printGlobal out schemeEntryLabel
    out.Write(schemeEntryLabel)
    out.WriteLine(":")
    showInstrs out instrs
    out.ToString()

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
        | '\r' :: '\n' :: cs
        | '\n' :: cs -> loop ('n'  :: '\\' :: acc) cs
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

    let printStringConst (name, constant) =
        printReadOnlySec out
        printAlign out wordSize
        printLabel out name
        match constant with
        | StringConst literal ->
            let escaped = escapeString literal
            let firstField = (escaped.Length <<< stringSizeShift) ||| stringTag
            fprintfn out "    .quad %d" firstField
            // printfn "original: %A" literal
            // printfn "length: %A" literal.Length
            // printfn "escaped: %A" escaped
            // printfn "length: %A" escaped.Length
            // printfn "\n"
            fprintfn out "    .ascii \"%s\"" escaped
        | FloatConst value ->
            let i64 = BitConverter.DoubleToInt64Bits(value)
            let hi = (i64 >>> 62) &&& (int64 0x03)
            let lo = (i64 <<< 2) ||| (int64 flonumTag)
            // fprintfn out "    .quad %d" hi
            fprintfn out "    .quad %d" lo

    let handleDef def =
        out.WriteLine("    .text")
        printGlobal out def.Name
        printLabel out def.Name
        showInstrs out def.Instrs
        out.WriteLine()
        out.WriteLine()

    List.iter printStringConst prog.Constants

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

