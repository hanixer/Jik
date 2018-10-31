module Codegen

open Cps
open Core
open Graph
open RuntimeConstants
open System.IO
open System.Collections.Generic

type Register =
    | Rsp | Rbp | Rax | Rbx | Rcx | Rdx | Rsi | Rdi 
    | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    | Al | Ah | Bl | Bh | Cl | Ch

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
    | IfTemp of string * Instr list * Instr list

and Instr = InstrName * Operand list

let isArithm = function
    | Add | Sub | Neg | Sar | Sal -> true
    | _ -> false

let showInstrs out instrs =
    let showOp op =
        match op with
        | Set cc ->
            fprintf out "set%s " ((sprintf "%A" cc).ToLower())
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

let rec selectInstr cps =
    let fixNumber n = n <<< fixnumShift
    let opToOp op = if op = Prim.Add then Add else Sub
    let moveInt n = [Mov, [Int n; Reg Rax ]]

    let comparison var1 var2 cc dest =
        let instrs =
            [Mov, [Var var1; Reg Rax]
             Cmp, [Var var2; Reg Rax]
             Set cc, [Reg Al]
             Movzb, [Reg Al; Reg Rax]
             Sal, [Int boolBit; Reg Rax]
             Or, [Int falseLiteral; Reg Rax]]
        match dest with
        | None ->
            instrs
        | Some dest ->
            instrs @ [Mov, [Reg Rax; dest]]

    let prim dest (op, args) = 
        match (op, args) with
        | Prim.Add, [var1; var2]
        | Prim.Sub, [var1; var2] ->
            let op = opToOp op
            match dest with
            | None ->
                [Mov, [Var var1; Reg Rax]
                 op, [Var var2; Reg Rax]]
            | Some dest ->
                [Mov, [Var var1; dest]
                 op, [Var var2; dest]]
        | Prim.Sub, [var] ->
            match dest with
            | None ->
                [Neg, [Var var]]
            | Some dest ->
                [Mov, [Var var; dest]
                 Neg, [dest]]
        | Prim.Lt, [var1; var2] -> comparison var1 var2 L dest
        | Prim.Gt, [var1; var2] -> comparison var1 var2 G dest
        | Prim.Ge, [var1; var2] -> comparison var1 var2 Ge dest
        | Prim.Le, [var1; var2] -> comparison var1 var2 Le dest
        | Prim.Eq, [var1; var2] -> comparison var1 var2 E dest
        | e -> failwithf "prim: %A %A" e (cpsToString cps)

    match cps with
    | Cps.Int n -> moveInt (fixNumber n)
    | Cps.Bool false -> moveInt falseLiteral
    | Cps.Bool true -> moveInt trueLiteral
    | PrimCall(op, args) -> prim None (op, args)
    | LetVal((var, Cps.Int n), body) ->
        let body = selectInstr body
        [Mov, [fixNumber n |> Int; Var var]] @ body
    | LetVal((var, PrimCall(op, args)), body) ->
        let rhs = prim (Some (Var var)) (op, args)
        let body = selectInstr body
        rhs @ body
    | LetVal((var, rhs), body) ->
        let rhs = selectInstr rhs
        let body = selectInstr body
        rhs @ [Mov, [Reg Rax; Var var]] @ body
    | Return var ->
        [Mov, [Var var; Reg Rax]
         Ret, []]
    | Cps.If(var, thn, els) ->
        let elseLabel = freshLabel "L"
        let endLabel = freshLabel "L"
        let thns = selectInstr thn
        let elss = selectInstr thn
        [Mov, [Var var; Reg Rax]
         Cmp, [Int falseLiteral; Reg Rax]
         JmpIf (E, elseLabel), []] @
        thns @
        [Jmp endLabel, []
         Label elseLabel, []] @
        elss @
        [Label endLabel, []] 
    | Cps.LetCont((k, args, contBody), body) ->
        selectInstr body
    | e -> failwithf "selectInstr: not implemented %A" e

let rec computeLiveAfter instrs =
    let writtenBy (op, args)  = 
        match op with
        | Add | Sub | Mov -> 
            match args with
            | [_; Var var] -> Set.singleton var
            | _ -> Set.empty
        | Neg | Pop ->
            match args with
            | [Var var] -> Set.singleton var
            | _ -> Set.empty
        | _ -> Set.empty

    let readBy (op, args) =
        match args with
        | [Var var; _] -> Set.singleton var
        | [Var var] -> 
            match op with
            | Sub -> Set.empty
            | _ -> Set.singleton var
        | _ -> Set.empty

    let folder instr (after, liveAfter) =
        let w = writtenBy instr
        let r = readBy instr
        let before = Set.union (Set.difference after w) r
        before, after :: liveAfter

    let liveAfter = 
        List.foldBack folder instrs (Set.empty, [])
        |> snd

    instrs, liveAfter

let collectVars instrs =
    List.map snd instrs
    |> List.map (fun args ->
        List.map (fun arg ->
            match arg with
            | Var var -> Set.singleton var
            | _ -> Set.empty) args
        |> Set.unionMany)
    |> Set.unionMany

let rec buildInterference (instrs, liveAfter) =
    let vars = collectVars instrs
    let graph = makeGraph vars
    Seq.iter (fun x -> printfn "buildInterference: %A" x) liveAfter

    let filterAndAddEdges ignore live target =
        Set.difference live (Set.ofList ignore)
        |> Set.iter (addEdge graph target)

    let iter ((op, args), live) = 
        match op with
        | Mov ->
            match args with
            | [Var var1; Var var2] ->
                filterAndAddEdges [var1; var2] live var2
            | [_; Var var2] ->
                filterAndAddEdges [var2] live var2
            | _ -> ()
        | op ->
            match args with
            | [_; Var var2] ->
                filterAndAddEdges [var2] live var2
            | _ -> ()

    List.zip instrs liveAfter
    |> List.iter iter

    instrs, graph

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
    let registers = []

    let rec fold (acc, remaining, stackIndex) _ color =
        match remaining with
        | reg :: rest ->
            (Map.add color (Reg reg) acc, rest, stackIndex)
        | _ ->
            let stackIndex = stackIndex - wordSize
            (Map.add color (Deref(stackIndex, Rbp)) acc, [], stackIndex)

    let colorToLocation, _, _ = 
        Map.fold fold (Map.empty, registers, 0) color

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

let compile s =
    let cps = stringToCps2 s
    let instrs = 
        cps 
        |> dbg selectInstr 
        |> computeLiveAfter 
        |> buildInterference 
        |> allocateRegisters 
        |> dbg patchInstr    

    let out = instrsToString instrs

    printfn "%s" (cpsToString cps)
    printfn "----"
    printfn "%s" (out.ToString())
    printfn "----"
    printfn "----"

    out.ToString()