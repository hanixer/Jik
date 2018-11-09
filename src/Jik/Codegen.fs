module Codegen

open Core
open Graph
open RuntimeConstants
open Intermediate
open System.IO
open System.Collections.Generic

/// Code generation.
/// Language resembles x86 instruction set.
/// After conversion from Intermediate, 'Var' operands created.
/// Later these operands are changed to stack locations or registers.

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

and Instr = InstrName * Operand list

let isArithm = function
    | Add | Sub | Neg | Sar | Sal -> true
    | _ -> false

let showInstrs out instrs =
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

    List.iter (fun (op, args) ->
        fprintf out "    "
        showOp op
        showArgs args
        fprintfn out "") instrs

let instrsToString instrs =
    let out = new StringWriter()
    fprintfn out "    .globl schemeEntry"
    fprintfn out "schemeEntry:"
    showInstrs out instrs
    out.ToString()

let selectInstructions labels =
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

    let handleDecl = function
        | var, Simple.Int n -> moveInt (convertNumber n) var
        | var, Simple.Bool true -> moveInt trueLiteral var
        | var, Simple.Bool false -> moveInt falseLiteral var
        | var, Simple.Prim(Prim.Add, [var1; var2]) ->
            [InstrName.Mov, [Var var1; Var var]
             InstrName.Add, [Var var2; Var var]]
        | var, Simple.Prim(Prim.Sub, [var1]) ->
            [InstrName.Mov, [Var var1; Var var]
             InstrName.Neg, [Var var]]
        | var, Simple.Prim(Prim.Lt, [var1; var2]) ->
            comparison var1 var2 Cc.L (Some(Var var))
        | var, e -> failwithf "handleDecl: %s %A" var e

    let handleTransfer = function
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

    let handleStmt = function
        | Decl decl -> handleDecl decl
        | Transfer tran -> handleTransfer tran

    let handleLabel (name, vars, stmts) =
        let l = [InstrName.Label name, []]
        l @ List.collect handleStmt stmts

    List.collect handleLabel labels

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

/// Assign registers or stack locations
/// for variables using graph coloring
let allocateRegisters (instrs, graph) =
    let color = colorGraph graph (collectVars instrs)
    let registers = [Rbx; Rcx]

    let rec fold (acc, remaining, stackIndex) color =
        match remaining with
        | reg :: rest ->
            (Map.add color (Reg reg) acc, rest, stackIndex)
        | _ ->
            let stackIndex = stackIndex - wordSize
            (Map.add color (Deref(stackIndex, Rbp)) acc, [], stackIndex)
    
    let colorToLocation, _, _ = 
        Set.fold fold (Map.empty, registers, 0) (color |> Map.toList |> List.map snd |> Set.ofList)

    let handleArg = function
        | Var var ->
            let colorValue = Map.find var color
            Map.find colorValue colorToLocation
        | arg -> arg

    let handleInstr(op, args) =
        op, List.map handleArg args

    List.map handleInstr instrs
    
let rec patchInstr instrs =
    let transform = function
        | op, [Deref(n, Rbp); Deref(m, Rbp)] ->
            [Mov, [Deref(n,  Rbp); Reg Rax];
             op, [Reg Rax; Deref(m,  Rbp)]]
        | e -> [e]

    instrs
    |> List.collect (transform)

let dbg func =
    (fun x -> let y = func x in printfn "%s" (instrsToString y); y)
    
let compile2 s =
    let labels = tope (stringToExpr s) 
    let live = computeLiveAfter labels
    let graph = buildInterference labels live
    let instrs = 
        allocateRegisters (selectInstructions labels, graph)
        |> patchInstr

    let out = instrsToString instrs

    printfn "%s" (labelsToString labels)
    printfn "%s\n" out

    out
