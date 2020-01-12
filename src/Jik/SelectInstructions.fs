module SelectInstructions

open Graph
open Core
open Intermediate
open RuntimeConstants
open Codegen

let argumentToLocation (siStart, siMult) reg args =
    let fold (regs, index, pairs) arg =
        match regs with
        | argReg :: rest ->
            rest, index, Map.add arg (Reg argReg) pairs
        | _ ->
            let offset = siStart + siMult * index
            [], index + 1, Map.add arg (Deref(offset, reg)) pairs

    let _, _, result = List.fold fold (registersForArgs, 0, Map.empty) args
    result

let moveClosureArgs args =
    List.mapi (fun i arg ->
        Mov, [Var arg; Deref((i + 1) * wordSize, R11)]) args

let convertNumber n = n <<< fixnumShift

let moveInt n var = [Mov, [Operand.Int n; Operand.Var var]]

let isOfTypeInstrs var var1 mask tag =
    [Mov, [Var var1; Reg Rax]
     And, [Int mask; Reg Rax]
     Cmp, [Int tag; Reg Rax]
     Set E, [Reg Al]
     Movzb, [Reg Al; Reg Rax]
     Sal, [Operand.Int boolBit; Reg Rax]
     Or, [Operand.Int falseLiteral; Reg Rax]
     Mov, [Reg Rax; Var var]]

let comparisonInstrs var1 var2 cc dest =
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

let setOnEqualInstrs var =
     [Set E, [Reg Al]
      Movzb, [Reg Al; Reg Rax]
      Sal, [Operand.Int boolBit; Reg Rax]
      Or, [Operand.Int falseLiteral; Reg Rax]
      Mov, [Reg Rax; Var var]]

let makeVectorInstrs size var =
    let shift = if wordSize = 8 then 3 else 2
    [Mov, [GlobalValue(freePointer); Reg R11]
     Mov, [size; Deref(0, R11)]
     Or, [Int vectorTag; Reg R11]
     Mov, [Reg R11; Var var]
     Mov, [size; Reg R11]
     Sar, [Int fixnumShift; Reg R11]
     Add, [Int 1; Reg R11]
     Sal, [Int shift; Reg R11]
     Add, [Reg R11; GlobalValue(freePointer)]]

let vectorAddress vec index =
    [Mov, [Var vec; Reg R11]
     Mov, [index; Reg R12]
     Sar, [Int fixnumShift; Reg R12]
     Add, [Int 1; Reg R12]]

let rec declToInstrs (var, x) =
    match x with
    | Simple.EmptyList -> moveInt nilLiteral var
    | Simple.Int n -> moveInt (convertNumber n) var
    | Simple.Bool true -> moveInt trueLiteral var
    | Simple.Bool false -> moveInt falseLiteral var
    | Simple.Prim(Prim.Add, [var1; var2]) ->
        [InstrName.Mov, [Var var1; Var var]
         InstrName.Add, [Var var2; Var var]]
    | Simple.Prim(Prim.Mul, [var1; var2]) ->
        [InstrName.Mov, [Var var1; Var var]
         InstrName.IMul, [Var var2; Var var]
         InstrName.Sar, [Int fixnumShift; Var var]]
    | Simple.Prim(Prim.Sub, [var1; var2]) ->
        [InstrName.Mov, [Var var1; Var var]
         InstrName.Sub, [Var var2; Var var]]
    | Simple.Prim(Prim.Sub, [var1]) ->
        [InstrName.Mov, [Var var1; Var var]
         InstrName.Neg, [Var var]]
    | Simple.Prim(Prim.Lt, [var1; var2]) ->
        comparisonInstrs var1 var2 Cc.L (Some(Var var))
    | Simple.Prim(Prim.Not, [var1]) ->
        [Cmp, [Operand.Int falseLiteral; Var var1]] @
        setOnEqualInstrs var
    | Simple.Prim(Prim.Eq, [var1; var2]) ->
        [Cmp, [Var var1; Var var2]] @
        setOnEqualInstrs var
    | Simple.Prim(Prim.IsFixnum, [var1]) ->
        [Mov, [Var var1; Var var]
         And, [Int fixnumMask; Var var]
         Cmp, [Int fixnumTag; Var var]] @
         setOnEqualInstrs var
    | Simple.Prim(Prim.Cons, [var1; var2]) ->
        [Mov, [GlobalValue(freePointer); Reg R11]
         Mov, [Var var1; Deref(0, R11)]
         Mov, [Var var2; Deref(wordSize, R11)]
         Or, [Int pairTag; Reg R11]
         Mov, [Reg R11; Var var]
         Add, [Int (2 * wordSize); GlobalValue(freePointer)]]
    | Simple.Prim(Prim.IsPair, [var1]) ->
        isOfTypeInstrs var var1 pairMask pairTag
    | Simple.Prim(Prim.IsNull, [var1]) ->
        isOfTypeInstrs var var1 nilMask nilLiteral
    | Simple.Prim(Prim.Car, [pair]) ->
        [Mov, [Var pair; Reg R11]
         Mov, [Deref(-pairTag, R11); Var var]]
    | Simple.Prim(Prim.Cdr, [pair]) ->
        [Mov, [Var pair; Reg R11]
         Mov, [Deref(-pairTag + wordSize, R11); Var var]]
    | Simple.Prim(Prim.SetCar, [pair; value]) ->
        [Mov, [Var pair; Reg R11]
         Mov, [Var value; Deref(-pairTag, R11)]
         Mov, [Var value; Var var]]
    | Simple.Prim(Prim.SetCdr, [pair; value]) ->
        [Mov, [Var pair; Reg R11]
         Mov, [Var value; Deref(-pairTag + wordSize, R11)]
         Mov, [Var value; Var var]]
    | Simple.Prim(Prim.MakeVector, [var1]) ->
        makeVectorInstrs (Var var1) var
    | Simple.Prim(Prim.VectorLength, [var1]) ->
        [Mov, [Var var1; Reg R11]
         Mov, [Deref(-vectorTag, R11); Var var]]
    | Simple.Prim(Prim.IsVector, [var1]) ->
        isOfTypeInstrs var var1 vectorMask vectorTag
    | Simple.Prim(Prim.VectorSet, [vec; index; value]) ->
        vectorAddress vec (Var index) @
        [Mov, [Var value; Deref4(-vectorTag, R11, R12, wordSize)]]
    | Simple.Prim(Prim.VectorRef, [vec; index]) ->
        vectorAddress vec (Var index) @
        [Mov, [Deref4(-vectorTag, R11, R12, wordSize); Var var]]
    | Simple.Prim(Prim.MakeClosure, label :: args) ->
        let offset = (List.length args + 1) * wordSize
        [Mov, [GlobalValue(freePointer); Reg R11]
         Lea(label), [Deref(0, R11)]] @
        moveClosureArgs args @
        [Mov, [Int offset; Reg R12]
         Add, [Reg R12; GlobalValue(freePointer)]
         Or, [Int closureTag; Reg R11]
         Mov, [Reg R11; Var var]]
    | Simple.Prim(Prim.ClosureRef, [var1]) ->
        [Mov, [Var var1; Reg Rax]
         Sar, [Int fixnumShift; Reg Rax]
         Mov, [Deref4(-closureTag + wordSize, Rsi, Rax, wordSize); Var var]]
    | Simple.Prim(Prim.IsProcedure, [var1]) ->
        isOfTypeInstrs var var1 closureMask closureTag
    | Simple.Prim(Prim.BoxCreate, [var1]) ->
        makeVectorInstrs (Int (1 <<< fixnumShift)) var @
        vectorAddress var (Int 0) @
        [Mov, [Var var1; Deref4(-vectorTag, R11, R12, wordSize)]
         Mov, [Var var; Var var1]]
    | Simple.Prim(Prim.BoxRead, [var1]) ->
        vectorAddress var1 (Int 0) @
        [Mov, [Deref4(-vectorTag, R11, R12, wordSize); Var var]]
    | Simple.Prim(Prim.BoxWrite, [box; value]) ->
        vectorAddress box (Int 0) @
        [Mov, [Var value; Deref4(-vectorTag, R11, R12, wordSize)]]
    | Simple.Prim(Prim.GlobalSet, [glob; value]) ->
        [Mov, [Var value; GlobalValue(glob)]]
    | Simple.Prim(Prim.GlobalRef, [glob]) ->
        [Mov, [GlobalValue(glob); Var var]]
    | Simple.Prim(Prim.IsZero, [var1]) ->
        [Cmp, [Int 0; Var var1]
         Set E, [Reg Al]
         Movzb, [Reg Al; Reg Rax]
         Sal, [Operand.Int boolBit; Reg Rax]
         Or, [Operand.Int falseLiteral; Reg Rax]
         Mov, [Reg Rax; Var var]]
    | e -> failwithf "handleDecl: %s %A" var e

let transferToInstrs blocks = function
    | Return var ->
        [Mov, [Var var; Reg Rax]
         RestoreStack, []
         Ret, []]
    | Transfer.Jump(label, vars) ->
        let args = getArgsOfBlock blocks label
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
        let argToLoc = argumentToLocation (-2 * wordSize, -wordSize) Rsp args
        let moveToArgPositions =
            List.map (fun arg -> Mov, [Var arg; Map.find arg argToLoc]) args
        let argsOfBlock = getArgsOfBlock blocks label
        let argOfBlock = List.head argsOfBlock
        if List.length argsOfBlock <> 1 then
            failwith "handleTransfer: wrong number of vars"
        moveToArgPositions @
        [Mov, [Reg Rsi; Deref(0, Rsp)]
         Mov, [Var func; Reg Rsi]
         Mov, [Deref(-closureTag, Rsi); Reg Rax]
         CallIndirect, [Reg Rax]
         Mov, [Deref(0, Rsp); Reg Rsi]
         Mov, [Reg Rax; Var argOfBlock]
         InstrName.Jmp label, []]
    | Transfer.Call(Tail, func, args) ->
        let argToLoc = argumentToLocation (-2 * wordSize, -wordSize) Rsp args
        let moveToArgPositions =
            List.map (fun arg -> Mov, [Var arg; Map.find arg argToLoc]) args
        let moveStackArgs =
            if List.length args > List.length registersForArgs then
                List.skip (List.length registersForArgs) moveToArgPositions
                |> List.mapi (fun i x ->
                    match x with
                    | Mov, [_; arg2] -> Mov, [arg2; Slot(i)]
                    | _ -> failwith "wrong")
            else []
        moveToArgPositions @
        moveStackArgs @
        [Mov, [Var func; Reg Rsi]
         Mov, [Deref(-closureTag, Rsi); Reg Rax]
         RestoreStack, []
         JmpIndirect, [Reg Rax]]

let saveArgs args instrs =
    let args =
        if List.length args > List.length registersForArgs then
            List.take (List.length registersForArgs) args
        else args
    let argToLoc = argumentToLocation (wordSize, wordSize) Rbp args
    let saveInstrs =
        List.map (fun arg -> Mov, [Map.find arg argToLoc; Var arg]) args
    match instrs with
    | head :: tail -> head :: saveInstrs @ tail
    | _ -> instrs

let selectInstructions (prog : Intermediate.Program) : Program =
    let handleStmt blocks = function
        | Decl decl -> declToInstrs decl
        | Transfer tran -> transferToInstrs blocks tran

    let handleBlock blocks (name, _, stmts) =
        (InstrName.Label name, []) :: List.collect (handleStmt blocks) stmts

    let handleDef proc =
        let instrs =
            List.collect (handleBlock proc.Blocks) proc.Blocks
            |> saveArgs proc.Args
        let graph = makeGraph []
        let diff = List.length proc.Args - List.length registersForArgs
        let slots = if diff > 0 then diff else 0
        let stackArgs = if diff > 0 then List.skip (List.length registersForArgs) proc.Args else []
        { Name = proc.Name
          Free = proc.Free
          Args = proc.Args
          IsDotted = proc.IsDotted
          StackArgs = stackArgs
          Vars = ref []
          MaxStack = 0
          InterfGraph = graph
          Instrs = instrs
          LiveBefore = Map.empty
          LiveAfter = Map.empty
          SlotsOccupied = slots }

    let impl = freshLabel "schemeEntryImpl"
    let entryImplFunc =
        match prog.Main.Blocks with
        | (_, args, stmts) :: restBlocks ->
            { prog.Main with
                Name = impl
                Blocks = (impl, args, stmts) :: restBlocks }
        | _ -> failwith "selectInstructions: wrong main function"
    let procs = entryImplFunc :: prog.Procedures
    let main = handleDef { emptyFunction with Name = schemeEntryLabel }
    let main = { main with Instrs = [Label schemeEntryLabel, []; Call impl, []] }
    { Procedures = List.map handleDef procs
      Main = main
      Globals = prog.Globals }

