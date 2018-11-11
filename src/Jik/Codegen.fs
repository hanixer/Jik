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
      InterfGraph : Graph }

type Program = FunctionDef list * FunctionDef

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

    let showInstr (op, args) =
        fprintf out "    "
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
        fprintfn out ""

    List.iter showInstr instrs

let instrsToString instrs =
    let out = new StringWriter()
    fprintfn out "    .globl schemeEntry"
    fprintfn out "schemeEntry:"
    showInstrs out instrs
    out.ToString()

let programToString (defs, main) =   
    let out = new StringWriter()  

    let handleDef def =
        fprintfn out "    .globl %s" def.Name
        showInstrs out def.Instrs
        fprintfn out "\n\n"

    List.iter handleDef defs
    handleDef {main with Name = "schemeEntry"}
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

let selectInstructions (defs, labels) : Program =
    let convertNumber n = n <<< fixnumShift
    let moveInt n var = [Mov, [Operand.Int n; Operand.Var var]]

    let registersForArgs = [Rcx; Rdx; R8; R9]

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
        | var, Simple.FunctionRef(funcRef) ->
            [Lea(funcRef), [Var var]]
        | var, e -> failwithf "handleDecl: %s %A" var e

    let handleTransfer labels = function
        | Return var -> 
            [Mov, [Var var; Reg Rax]]
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
            let _, _, argToLoc = 
                List.fold (fun (regs, index, pairs) arg ->
                    match regs with
                    | reg :: rest -> 
                        rest, index, Map.add arg (Reg reg) pairs
                    | _ ->
                        [], index - wordSize, Map.add arg (Deref(index, Rsp)) pairs
                    ) (registersForArgs, 0, Map.empty) args
            let instrs =
                List.map (fun arg -> Mov, [Var arg; Map.find arg argToLoc]) args
            let argsOfLabel = getArgsOfLabel labels label
            if List.length argsOfLabel <> 1 then 
                failwith "handleTransfer: wrong number of vars"
            let argOfLabel = List.head argsOfLabel
            instrs @ [CallIndirect, [Var func]
                      Mov, [Reg Rax; Var argOfLabel]
                      InstrName.Jmp label, []]
        | Transfer.Call(_, _, _) -> failwith "Not Implemented"

    let handleStmt labels = function
        | Decl decl -> handleDecl decl
        | Transfer tran -> handleTransfer labels tran

    let handleLabel labels (name, _, stmts) =
        (InstrName.Label name, []) :: List.collect (handleStmt labels) stmts

    let handleDef (name, args, labels) =
        let maxArgs = countMaxArgs labels
        { Name = name
          Args = args
          Vars = ref []
          MaxStack = maxArgs
          InterfGraph = buildInterference labels
          Instrs = List.collect (handleLabel labels) labels }

    List.map handleDef defs, handleDef ("start", [], labels)

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
    let registers = [Rbx; Rcx]

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
    let handleArg env = function
        | Var var -> Map.find var env
        | arg -> arg

    let handleInstr env (op, args) =
        op, List.map (handleArg env) args

    let handleFunctionDef (def : FunctionDef) =
        let env, numStackLocals = calculateVarToLocEnv def.InterfGraph def.Instrs
        let instrs = List.map (handleInstr env) def.Instrs
        let maxStack = def.MaxStack + numStackLocals
        {def with Instrs = instrs
                  MaxStack = maxStack}

    List.map handleFunctionDef defs, handleFunctionDef main

let addFunctionBeginEnd (defs, main) =
    let functionBegin maxStack =
        let n = maxStack * wordSize
        [Push, [Reg Rbp]
         Mov, [Reg Rsp; Reg Rbp]
         Push, [Reg R15]
         Push, [Reg R14]
         Push, [Reg R13]
         Push, [Reg R12]
         Push, [Reg Rbx]
         Sub, [Operand.Int 0; Reg Rsp]]

    let functionEnd maxStack =
        let n = maxStack * wordSize
        [Add, [Operand.Int n; Reg Rsp]
         Pop, [Reg Rbx]
         Pop, [Reg R12]
         Pop, [Reg R13]
         Pop, [Reg R14]
         Pop, [Reg R15]
         Pop, [Reg Rbp]
         Ret, []]

    let handleDef def =
        let instrs = 
            (functionBegin def.MaxStack) @ 
            def.Instrs @ 
            (functionEnd def.MaxStack)
        {def with Instrs = instrs}
    
    List.map handleDef defs, handleDef main
    
let rec patchInstr (defs, main) =
    let transform = function
        | op, [Deref(n, Rbp); Deref(m, Rbp)] ->
            [Mov, [Deref(n,  Rbp); Reg Rax];
             op, [Reg Rax; Deref(m,  Rbp)]]
        | e -> [e]

    let handleDef def =
        {def with Instrs = List.collect transform def.Instrs}

    List.map handleDef defs, handleDef main

let dbg func =
    (fun x -> let y = func x in printfn "%s" (instrsToString y); y)
    
// let compile2 s =
//     let labels = tope (stringToExpr s) 
//     let live = computeLiveAfter labels
//     let graph = buildInterference labels live
//     let instrs = 
//         allocateLocals (selectInstructions labels, graph)
//         |> patchInstr

//     let out = instrsToString instrs

//     printfn "%s" (labelsToString labels)
//     printfn "%s\n" out

//     out

// let compile3 s =
//     let prog = stringToProgram s
//     let out = prog |> selectInstructions |> allocateRegisters |> programToString

