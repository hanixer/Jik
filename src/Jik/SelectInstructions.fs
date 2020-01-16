module SelectInstructions

open Graph
open Core
open Intermediate
open RuntimeConstants
open Codegen

let foreignFuncPrefix = "s_"

let stackArgsCount args = List.length args - List.length registersForArgs

let moveClosureArgs args =
    List.mapi (fun i arg ->
        Mov, [Var arg; Deref((i + 1) * wordSize, R11)]) args

let argumentToLocation (siStart, siMult) reg args =
    let handleArg index arg =
        let offset = siStart + siMult * index
        arg, Deref(offset, reg)

    Map.ofSeq (List.mapi handleArg args)

/// Generate instruction to prepare arguments for call.
let moveArgsForCall args =
    let argToLoc = argumentToLocation (-2 * wordSize, -wordSize) Rsp args
    List.map (fun arg -> Mov, [Var arg; Map.find arg argToLoc]) args

let argumentToLocationFF (siStart, siMult) reg args =
    let fold (regs, index, pairs) arg =
        match regs with
        | argReg :: rest ->
            rest, index, Map.add arg (Reg argReg) pairs
        | _ ->
            let offset = siStart + siMult * index
            [], index + 1, Map.add arg (Deref(offset, reg)) pairs

    let _, _, result = List.fold fold (registersForArgs, 0, Map.empty) args
    result

/// Prepare arguments for foreign call. First move them to corresponding registers (registersForArgs).
/// If registers are not enough then move to stack locations.
let moveArgsForCallFF args =
    let stackArgsCount = stackArgsCount args
    let argToLoc = argumentToLocationFF (-stackArgsCount * wordSize, wordSize) Rsp args
    List.map (fun arg -> Mov, [Var arg; Map.find arg argToLoc]) args

let getCallResultVar label blocks =
    let argsOfBlock = getArgsOfBlock blocks label
    if List.length argsOfBlock <> 1 then
        failwith "handleTransfer: wrong number of vars"
    List.head argsOfBlock

let makeIndirectCall func =
    [Mov, [Reg Rsi; Deref(0, Rsp)] // Save current closure pointer
     Mov, [Var func; Reg Rsi]
     Mov, [Deref(-closureTag, Rsi); Reg Rax]
     CallIndirect, [Reg Rax]
     Mov, [Deref(0, Rsp); Reg Rsi]] // Restore closure pointer

let convertNumber n = n <<< fixnumShift

let moveInt n var = [Mov, [Operand.Int n; Operand.Var var]]

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

let setOnEqualInstrs dest =
    [Set E, [Reg Al]
     Movzb, [Reg Al; Reg Rax]
     Sal, [Operand.Int boolBit; Reg Rax]
     Or, [Operand.Int falseLiteral; Reg Rax]
     Mov, [Reg Rax; Var dest]]

let isOfTypeInstrs dest var1 mask tag =
    [Mov, [Var var1; Reg Rax]
     And, [Int mask; Reg Rax]
     Cmp, [Int tag; Reg Rax]]
    @ setOnEqualInstrs dest

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

let makeStringInstrs size dest =
    [Mov, [GlobalValue(freePointer); Reg R11]
     Mov, [size; Deref(0, R11)]
     Or, [Int stringTag; Reg R11]
     Mov, [Reg R11; Var dest]
     Mov, [size; Reg R11]
     Sar, [Int fixnumShift; Reg R11]
     Add, [Int wordSize; Reg R11]
     Add, [Int (wordSize - 1); Reg R11]
     And, [Int (-wordSize); Reg R11]
     Add, [Reg R11; GlobalValue(freePointer)]]

let vectorAddress vec index =
    [Mov, [Var vec; Reg R11]
     Mov, [index; Reg R12]
     Sar, [Int fixnumShift; Reg R12]
     Add, [Int 1; Reg R12]]


let getCar reg dest = Mov, [Deref(-pairTag, reg); dest]
let getCdr reg dest = Mov, [Deref(-pairTag + wordSize, reg); dest]

let rec declToInstrs (dest, x) =
    match x with
    | Simple.EmptyList -> moveInt nilLiteral dest
    | Simple.Int n -> moveInt (convertNumber n) dest
    | Simple.Char c -> moveInt (((int c) <<< charShift) ||| charTag) dest
    | Simple.Bool true -> moveInt trueLiteral dest
    | Simple.Bool false -> moveInt falseLiteral dest
    | Simple.Prim(Prim.Add, [var1; var2]) ->
        [InstrName.Mov, [Var var1; Reg Rax]
         InstrName.Add, [Var var2; Reg Rax]
         InstrName.Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Mul, [var1; var2]) ->
        [InstrName.Mov, [Var var1; Reg Rax]
         InstrName.IMul, [Var var2; Reg Rax]
         InstrName.Sar, [Int fixnumShift; Reg Rax]
         InstrName.Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Sub, [var1; var2]) ->
        [InstrName.Mov, [Var var1; Reg Rax]
         InstrName.Sub, [Var var2; Reg Rax]
         InstrName.Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Sub, [var1]) ->
        [InstrName.Mov, [Var var1; Reg Rax]
         InstrName.Neg, [Reg Rax]
         InstrName.Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Lt, [var1; var2]) ->
        comparisonInstrs var1 var2 Cc.L (Some(Var dest))
    | Simple.Prim(Prim.Not, [var1]) ->
        [Cmp, [Operand.Int falseLiteral; Var var1]] @
        setOnEqualInstrs dest
    | Simple.Prim(Prim.Eq, [var1; var2]) ->
        [Cmp, [Var var1; Var var2]] @
        setOnEqualInstrs dest
    | Simple.Prim(Prim.IsFixnum, [var1]) ->
        [Mov, [Var var1; Var dest]
         And, [Int fixnumMask; Var dest]
         Cmp, [Int fixnumTag; Var dest]] @
         setOnEqualInstrs dest
    | Simple.Prim(Prim.IsZero, [var1]) ->
        [Cmp, [Int 0; Var var1]]
        @ setOnEqualInstrs dest
    | Simple.Prim(Prim.IsChar, [var1]) ->
        isOfTypeInstrs dest var1 charMask charTag
    | Simple.Prim(Prim.IsBoolean, [var1]) ->
        [Mov, [Var var1; Reg Rax]
         And, [Int boolTag; Reg Rax]
         Cmp, [Int boolTag; Reg Rax]]
        @ setOnEqualInstrs dest
    | Simple.Prim(Prim.CharToNumber, [var1]) ->
        [Mov, [Var var1; Reg Rax]
         Sar, [Int charShift; Reg Rax]
         Movzb, [Reg Al; Reg Rax]
         Sal, [Int fixnumShift; Reg Rax]
         Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.NumberToChar, [var1]) ->
        [Mov, [Var var1; Reg Rax]
         Sar, [Int fixnumShift; Reg Rax]
         Movzb, [Reg Al; Reg Rax]
         Sal, [Int charShift; Reg Rax]
         Or, [Int charTag; Reg Rax]
         Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Cons, [var1; var2]) ->
        [Mov, [GlobalValue(freePointer); Reg R11]
         Mov, [Var var1; Deref(0, R11)]
         Mov, [Var var2; Deref(wordSize, R11)]
         Or, [Int pairTag; Reg R11]
         Mov, [Reg R11; Var dest]
         Add, [Int (2 * wordSize); GlobalValue(freePointer)]]
    | Simple.Prim(Prim.IsPair, [var1]) ->
        isOfTypeInstrs dest var1 pairMask pairTag
    | Simple.Prim(Prim.IsNull, [var1]) ->
        isOfTypeInstrs dest var1 nilMask nilLiteral
    | Simple.Prim(Prim.Car, [pair]) ->
        [Mov, [Var pair; Reg R11]
         getCar R11 (Var dest)]
    | Simple.Prim(Prim.Cdr, [pair]) ->
        [Mov, [Var pair; Reg R11]
         getCdr R11 (Var dest)]
    | Simple.Prim(Prim.SetCar, [pair; value]) ->
        [Mov, [Var pair; Reg R11]
         Mov, [Var value; Deref(-pairTag, R11)]
         Mov, [Var value; Var dest]]
    | Simple.Prim(Prim.SetCdr, [pair; value]) ->
        [Mov, [Var pair; Reg R11]
         Mov, [Var value; Deref(-pairTag + wordSize, R11)]
         Mov, [Var value; Var dest]]
    // Vectors.
    | Simple.Prim(Prim.MakeVector, [var1]) ->
        makeVectorInstrs (Var var1) dest
    | Simple.Prim(Prim.VectorLength, [var1]) ->
        [Mov, [Var var1; Reg R11]
         Mov, [Deref(-vectorTag, R11); Var dest]]
    | Simple.Prim(Prim.IsVector, [var1]) ->
        isOfTypeInstrs dest var1 vectorMask vectorTag
    | Simple.Prim(Prim.VectorSet, [vec; index; value]) ->
        vectorAddress vec (Var index) @
        [Mov, [Var value; Deref4(-vectorTag, R11, R12, wordSize)]]
    | Simple.Prim(Prim.VectorRef, [vec; index]) ->
        vectorAddress vec (Var index) @
        [Mov, [Deref4(-vectorTag, R11, R12, wordSize); Var dest]]
    // Strings.
    | Simple.Prim(Prim.MakeString, [var1]) ->
        makeStringInstrs (Var var1) dest
    | Simple.Prim(Prim.StringLength, [var1]) ->
        [Mov, [Var var1; Reg R11]
         Mov, [Deref(-stringTag, R11); Var dest]]
    | Simple.Prim(Prim.IsString, [var1]) ->
        isOfTypeInstrs dest var1 stringMask stringTag
    | Simple.Prim(Prim.StringSet, [instance; index; value]) ->
        [Mov, [Var instance; Reg R11]
         Mov, [Var index; Reg R12]
         Sar, [Int fixnumShift; Reg R12]
         Add, [Int wordSize; Reg R12]
         Mov, [Var value; Reg Rax]
         Sar, [Int charShift; Reg Rax]
         Movb, [Reg Al; Deref4(-stringTag, R11, R12, 1)]]
    | Simple.Prim(Prim.StringRef, [instance; index]) ->
        [Mov, [Var instance; Reg R11]
         Mov, [Var index; Reg R12]
         Sar, [Int fixnumShift; Reg R12]
         Add, [Int wordSize; Reg R12]
         Movzb, [Deref4(-stringTag, R11, R12, 1); Var dest]
         Sal, [Int charShift; Var dest]
         Or, [Int charTag; Var dest]]
    // Closures.
    | Simple.Prim(Prim.MakeClosure, label :: args) ->
        let offset = (List.length args + 1) * wordSize
        [Mov, [GlobalValue(freePointer); Reg R11]
         Lea(label), [Deref(0, R11)]] @
        moveClosureArgs args @
        [Mov, [Int offset; Reg R12]
         Add, [Reg R12; GlobalValue(freePointer)]
         Or, [Int closureTag; Reg R11]
         Mov, [Reg R11; Var dest]]
    | Simple.Prim(Prim.ClosureRef, [var1]) ->
        [Mov, [Var var1; Reg Rax]
         Sar, [Int fixnumShift; Reg Rax]
         Mov, [Deref4(-closureTag + wordSize, Rsi, Rax, wordSize); Var dest]]
    | Simple.Prim(Prim.IsProcedure, [var1]) ->
        isOfTypeInstrs dest var1 closureMask closureTag
    | Simple.Prim(Prim.GlobalSet, [glob; value]) ->
        [Mov, [Var value; GlobalValue(glob)]]
    | Simple.Prim(Prim.GlobalRef, [glob]) ->
        [Mov, [GlobalValue(glob); Var dest]]
    | Simple.Prim(Prim.Apply, [func; argsList]) ->
        let loopBegin = freshLabel "loopBegin"
        let loopEnd = freshLabel "loopEnd"
        [Mov, [Var func; Reg Rcx]
         Mov, [Var argsList; Reg Rdx]
         Mov, [Int 0; Reg R8]
         Label loopBegin, []
         Mov, [Int nilLiteral; Reg Rax]
         Cmp, [Reg Rdx; Reg Rax]
         JmpIf(E, loopEnd), []
         getCar Rdx (Reg Rax)
         Sub, [Int 1; Reg R8] // Change argument counter
         Mov, [Reg Rax; Deref4(-wordSize, Rsp, R8, wordSize)] // Save argument to its pos in stack
         getCdr Rdx (Reg Rdx)
         Jmp(loopBegin), []
         Label(loopEnd), []] @
        makeIndirectCall func @
        [Mov, [Reg Rax; Var dest]]

    | e -> failwithf "handleDecl: %s %A" dest e

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
        let resultVar = getCallResultVar label blocks
        moveArgsForCall args @
        makeIndirectCall func @
        [Mov, [Reg Rax; Var resultVar]
         InstrName.Jmp label, []]
    | Transfer.Call(Tail, func, args) ->
        let moveToArgPositions = moveArgsForCall args
        let moveStackArgs =
            moveToArgPositions
            |> List.mapi (fun i x ->
                match x with
                | Mov, [_; arg2] -> Mov, [arg2; Slot(i)]
                | _ -> failwith "wrong")

        moveToArgPositions @
        [Mov, [Var func; Reg Rsi]] @
        moveStackArgs @
        [Mov, [Deref(-closureTag, Rsi); Reg Rax]
         RestoreStack, []
         JmpIndirect, [Reg Rax]]
    | ForeignCall(label, foreignName, args) ->
        let moveToArgPositions = moveArgsForCallFF args
        let resultVar = getCallResultVar label blocks
        let name = foreignFuncPrefix + foreignName
        let stackArgsCount = args.Length - registersForArgs.Length
        let spChange = (stackArgsCount + 4) * wordSize // 4 is 'shadow space' which caller must allocate
        moveToArgPositions @
        [Sub, [Int spChange; Reg Rsp]
         Call name, []
         Add, [Int spChange; Reg Rsp]
         Mov, [Reg Rax; Var resultVar]
         InstrName.Jmp label, []]

/// This was used for approach with register allocation.
let saveArgs args instrs =
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
        let graph = makeGraph []
        let slots = List.length proc.Args
        { Name = proc.Name
          Free = proc.Free
          Args = proc.Args
          IsDotted = proc.IsDotted
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

