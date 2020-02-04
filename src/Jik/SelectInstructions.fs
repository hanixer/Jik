module SelectInstructions

open Graph
open Core
open Intermediate
open RuntimeConstants
open Primitive
open Codegen
open Common

let stackArgsCount args = List.length args - List.length registersForArgs

let moveClosureArgs args =
    List.mapi (fun i arg ->
        // Start at offset 2 * wordsize:
        // One word for forward bit,
        // other word for label - procedure address.
        Mov, [Var arg; Deref((i + 2) * wordSize, R11)]) args

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

let checkForClosureTag procName =
    [Mov, [Int closureMask; Reg Rax]
     And, [Reg Rsi; Reg Rax]
     Cmp, [Int closureTag; Reg Rax]
     Lea(procName), [Reg R10]
     JmpIf(Ne, procErrorHandler), []]

let makeIndirectCall procName func =
    [Mov, [Reg Rsi; Deref(0, R15)] // Save current closure pointer
     Mov, [Var func; Reg Rsi]] @
    checkForClosureTag procName @
    [CallIndirect, [Deref(-closureTag + wordSize, Rsi)]
     Mov, [Deref(0, R15); Reg Rsi]] // Restore closure pointer

/// Main function for indirect non-tail call.
let compileCall procName label blocks func args =
    let resultVar = getCallResultVar label blocks
    moveArgsForCall args @
    [Mov, [Int args.Length; Reg Rcx]] @
    makeIndirectCall procName func @
    [Mov, [Reg Rax; Var resultVar]
     Jmp label, []]

let compileApply procName label blocks func args =
    let resultVar = getCallResultVar label blocks
    let total = List.length args
    let stackOffset = (-1 - total) * wordSize // points to last argument
    moveArgsForCall args @
    spliceSlot stackOffset total @
    makeIndirectCall procName func @
    [Mov, [Reg Rax; Var resultVar]
     Jmp(label), []]

/// Tail call compilation. Handles simple tail calls and tail 'apply'.
/// shouldSplice - if true then the last argument is spliced for 'apply' support.
let compileTailCall procName shouldSplice func args =
    let shiftArg i instr =
        match instr with
        | Mov, [_; arg2] -> Mov, [arg2; Slot(i)]
        | _ -> failwith "wrong"

    let moveToArgPositions = moveArgsForCall args
    let shiftStackArgs = List.mapi shiftArg moveToArgPositions

    let restoreStack =
        if shouldSplice then
            [SpliceSlot(List.length args - 1, List.length args), []
             RestoreStack, []]
         else
            [Mov, [Int (List.length args); Reg Rcx]
             RestoreStack, []]

    moveToArgPositions @
    [Mov, [Var func; Reg Rsi]] @
    shiftStackArgs @
    restoreStack @
    checkForClosureTag procName @
    [JmpIndirect, [Deref(-closureTag + wordSize, Rsi)]]

let compileForeignCall label blocks foreignName args =
    let moveToArgPositions = moveArgsForCallFF args
    let resultVar = getCallResultVar label blocks
    let name = foreignName
    let stackArgsCount = System.Math.Max(args.Length - registersForArgs.Length, 0)
    let spChange = (stackArgsCount + 4) * wordSize // 4 is 'shadow space' which caller must allocate
    moveToArgPositions @
    [Sub, [Int spChange; Reg Rsp]
     Call name, []
     Add, [Int spChange; Reg Rsp]
     Mov, [Reg Rax; Var resultVar]
     Jmp label, []]

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

let compileSetOnEqual dest =
    [Set E, [Reg Al]
     Movzb, [Reg Al; Reg Rax]
     Sal, [Operand.Int boolBit; Reg Rax]
     Or, [Operand.Int falseLiteral; Reg Rax]
     Mov, [Reg Rax; Var dest]]

let compileIsOfType dest var1 mask tag =
    [Mov, [Var var1; Reg Rax]
     And, [Int mask; Reg Rax]
     Cmp, [Int tag; Reg Rax]]
    @ compileSetOnEqual dest

let callRuntime func =
    alignStackPointer @
    [Sub, [Int (4 * wordSize); Reg Rsp]
     Call(func), []
     Add, [Int (4 * wordSize); Reg Rsp]]

let callAllocate size dest =
    [Mov, [size; Reg Rdx] // Size to allocate.
     Mov, [Reg Rsi; Deref(0, R15)]
     Mov, [Reg R15; Reg Rcx] // Root stack pointer.
     Mov, [Reg Rsp; Reg Rbp]
     Sub, [Int wordSize; Reg Rsp]
     And, [Int -32; Reg Rsp]
     Sub, [Int (4*wordSize); Reg Rsp]
     Call("allocate"), []
     Mov, [Deref(0, R15); Reg Rsi]
     Mov, [Reg Rbp; Reg Rsp]
     Mov, [Reg Rax; dest]]

let compileMakeVector size dest =
    [Mov, [size; Reg Rax]
     Sar, [Int fixnumShift; Reg Rax]
     Add, [Int 1; Reg Rax]
     IMul, [Int wordSize; Reg Rax]
     Mov, [Reg Rax; Reg Rdx]] @
    callAllocate (Reg Rax) (Reg R11) @
    [Mov, [size; Deref(0, R11)]
     Or, [Int vectorTag; Reg R11]
     Mov, [Reg R11; Var dest]]

let compileMakeString size dest =
    [Mov, [size; Reg Rax]
     Sar, [Int fixnumShift; Reg Rax]
     Add, [Int wordSize; Reg Rax]] @
    callAllocate (Reg Rax) (Reg R11) @
    [Mov, [size; Deref(0, R11)]
     Or, [Int stringTag; Reg R11]
     Mov, [Reg R11; Var dest]]

let vectorAddress vec index =
    [Mov, [Var vec; Reg R11]
     Mov, [index; Reg R12]
     Sar, [Int fixnumShift; Reg R12]
     Add, [Int 1; Reg R12]]

let rec declToInstrs (dest, x) =
    match x with
    | Simple.EmptyList -> moveInt nilLiteral dest
    | Simple.Int n -> moveInt (convertNumber n) dest
    | Simple.RawInt n -> moveInt n dest
    | Simple.Char c -> moveInt (((int c) <<< charShift) ||| charTag) dest
    | Simple.Bool true -> moveInt trueLiteral dest
    | Simple.Bool false -> moveInt falseLiteral dest
    | Simple.Prim(Prim.Add, [var1; var2]) ->
        [Mov, [Var var1; Reg Rax]
         Add, [Var var2; Reg Rax]
         Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Sar, [var1; var2]) ->
        [Mov, [Var var1; Reg Rax]
         Mov, [Var var2; Reg Rcx]
         Sar, [Reg Cl; Reg Rax]
         Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Mul, [var1; var2]) ->
        [Mov, [Var var1; Reg Rax]
         IMul, [Var var2; Reg Rax]
         Sar, [Int fixnumShift; Reg Rax]
         Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Sub, [var1; var2]) ->
        [Mov, [Var var1; Reg Rax]
         Sub, [Var var2; Reg Rax]
         Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Quotient, [var1; var2]) ->
        [Mov, [Var var1; Reg Rax]
         Cqto, []
         IDiv, [Var var2]
         Sal, [Int fixnumShift; Reg Rax]
         Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Remainder, [var1; var2]) ->
        [Mov, [Var var1; Reg Rax]
         Cqto, []
         IDiv, [Var var2]
         Mov, [Reg Rdx; Var dest]]
    | Simple.Prim(Prim.Sub, [var1]) ->
        [Mov, [Var var1; Reg Rax]
         Neg, [Reg Rax]
         Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Lt, [var1; var2]) ->
        comparisonInstrs var1 var2 Cc.L (Some(Var dest))
    | Simple.Prim(Prim.Le, [var1; var2]) ->
        comparisonInstrs var1 var2 Cc.Le (Some(Var dest))
    | Simple.Prim(Prim.Ge, [var1; var2]) ->
        comparisonInstrs var1 var2 Cc.Ge (Some(Var dest))
    | Simple.Prim(Prim.Gt, [var1; var2]) ->
        comparisonInstrs var1 var2 Cc.G (Some(Var dest))
    | Simple.Prim(Prim.Not, [var1]) ->
        [Cmp, [Operand.Int falseLiteral; Var var1]] @
        compileSetOnEqual dest
    | Simple.Prim(Prim.Eq, [var1; var2]) ->
        [Cmp, [Var var1; Var var2]] @
        compileSetOnEqual dest
    | Simple.Prim(Prim.IsFixnum, [var1]) ->
        [Mov, [Var var1; Var dest]
         And, [Int fixnumMask; Var dest]
         Cmp, [Int fixnumTag; Var dest]] @
         compileSetOnEqual dest
    | Simple.Prim(Prim.IsZero, [var1]) ->
        [Cmp, [Int 0; Var var1]]
        @ compileSetOnEqual dest
    | Simple.Prim(Prim.IsChar, [var1]) ->
        compileIsOfType dest var1 charMask charTag
    | Simple.Prim(Prim.IsBoolean, [var1]) ->
        [Mov, [Var var1; Reg Rax]
         And, [Int boolTag; Reg Rax]
         Cmp, [Int boolTag; Reg Rax]]
        @ compileSetOnEqual dest
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
        let restoreStack = [Mov, [Reg Rbp; Reg Rsp]]
        [Mov, [Reg Rsp; Reg Rbp]
         Sub, [Int wordSize; Reg Rsp]
         And, [Int -32; Reg Rsp]
         Sub, [Int (4*wordSize); Reg Rsp]] @
        allocateCons (Var var1) (Var var2) (Var dest) restoreStack
    | Simple.Prim(Prim.IsPair, [var1]) ->
        compileIsOfType dest var1 pairMask pairTag
    | Simple.Prim(Prim.IsNull, [var1]) ->
        compileIsOfType dest var1 nilMask nilLiteral
    | Simple.Prim(Prim.Car, [pair]) ->
        [Mov, [Var pair; Reg R11]
         getCar R11 (Var dest)]
    | Simple.Prim(Prim.Cdr, [pair]) ->
        [Mov, [Var pair; Reg R11]
         getCdr R11 (Var dest)]
    | Simple.Prim(Prim.SetCar, [pair; value]) ->
        [Mov, [Var pair; Reg R11]
         Mov, [Var value; Deref(-pairTag + wordSize, R11)]
         Mov, [Var value; Var dest]]
    | Simple.Prim(Prim.SetCdr, [pair; value]) ->
        [Mov, [Var pair; Reg R11]
         Mov, [Var value; Deref(-pairTag + 2 * wordSize, R11)]
         Mov, [Var value; Var dest]]
    // Vectors.
    | Simple.Prim(Prim.MakeVector, [var1]) ->
        compileMakeVector (Var var1) dest
    | Simple.Prim(Prim.VectorLength, [var1]) ->
        [Mov, [Var var1; Reg R11]
         Mov, [Deref(-vectorTag, R11); Var dest]]
    | Simple.Prim(Prim.IsVector, [var1]) ->
        compileIsOfType dest var1 vectorMask vectorTag
    | Simple.Prim(Prim.VectorSet, [vec; index; value]) ->
        vectorAddress vec (Var index) @
        [Mov, [Var value; Deref4(-vectorTag, R11, R12, wordSize)]]
    | Simple.Prim(Prim.VectorRef, [vec; index]) ->
        vectorAddress vec (Var index) @
        [Mov, [Deref4(-vectorTag, R11, R12, wordSize); Var dest]]
    // Strings.
    | Simple.Prim(Prim.MakeString, [var1]) ->
        compileMakeString (Var var1) dest
    | Simple.Prim(Prim.StringLength, [var1]) ->
        [Mov, [Var var1; Reg R11]
         Mov, [Deref(-stringTag, R11); Var dest]]
    | Simple.Prim(Prim.IsString, [var1]) ->
        compileIsOfType dest var1 stringMask stringTag
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
    | Simple.Prim(Prim.StringInit, [stringData]) ->
        [Lea stringData, [Var dest]
         Or, [Int stringTag; Var dest]]
    // Closures.
    | Simple.Prim(Prim.MakeClosure, label :: args) ->
        let offset = (List.length args + 2) * wordSize
        let size = (List.length args + 1) <<< fixnumShift
        callAllocate (Int offset) (Reg R11) @
        [Mov, [Int size; Deref(0, R11)] // The first cell is for size and forwarding bit.
         Lea(label), [Deref(wordSize, R11)]] @ // The second is for label.
        moveClosureArgs args @ // Remaining cells are for free variables.
        [Or, [Int closureTag; Reg R11]
         Mov, [Reg R11; Var dest]]

    | Simple.Prim(Prim.ClosureRef, [var1]) ->
        [Mov, [Var var1; Reg Rax]
         Sar, [Int fixnumShift; Reg Rax]
         Mov, [Deref4(-closureTag + 2 * wordSize, Rsi, Rax, wordSize); Var dest]]
    | Simple.Prim(Prim.IsProcedure, [var1]) ->
        compileIsOfType dest var1 closureMask closureTag
    // Globals.
    | Simple.Prim(Prim.GlobalSet, [glob; value]) ->
        [Mov, [Var value; GlobalValue(glob)]]
    | Simple.Prim(Prim.GlobalRef, [glob]) ->
        // Save address of global var in rcx for C error handler.
        [Mov, [GlobalValue(glob); Reg Rax]
         Cmp, [Int undefinedLiteral; Reg Rax]
         Lea(glob), [Reg Rcx]
         JmpIf(E, globVarErrorHandler), []
         Mov, [GlobalValue(glob); Var dest]]
    | Simple.Prim(Prim.GlobalRefUncheck, [glob]) ->
         [Mov, [GlobalValue(glob); Var dest]]
    // Symbols.
    | Simple.Prim(Prim.MakeSymbol, [var1]) ->
        [Mov, [Var var1; Var dest]
         Sub, [Int stringTag; Var dest]
         Or, [Int symbolTag; Var dest]]
    | Simple.Prim(Prim.SymbolString, [var1]) ->
        [Mov, [Var var1; Var dest]
         Sub, [Int symbolTag; Var dest]
         Or, [Int stringTag; Var dest]]
    | Simple.Prim(Prim.IsSymbol, [var1]) ->
        compileIsOfType dest var1 symbolMask symbolTag

    | Simple.Prim(Prim.EofObject, []) -> moveInt eofTag dest
    | Simple.Prim(Prim.IsEofObject, [var1]) ->
        compileIsOfType dest var1 eofMask eofTag

    | Simple.Prim(Prim.CheckFreePointer, [size]) ->
        [Mov, [Var size; Reg Rax]
         Sar, [Int fixnumShift; Reg Rax]
         IMul, [Int wordSize; Reg Rax]
         Add, [GlobalValue freePointer; Reg Rax]
         Cmp, [GlobalValue fromSpaceEnd; Reg Rax]
         Set L, [Reg Al]
         Movzb, [Reg Al; Reg Rax]
         Sal, [Operand.Int boolBit; Reg Rax]
         Or, [Operand.Int falseLiteral; Reg Rax]
         Mov, [Reg Rax; Var dest]]
    | Simple.Prim(Prim.Collect, [size]) ->
        [Mov, [Var size; Reg Rdx]
         Sar, [Int 2; Reg Rdx]
         Mov, [Reg R15; Reg Rcx]
         Mov, [Reg Rsi; Deref(0, R15)]
         Call(collectFunction), []
         Mov, [Deref(0, R15); Reg Rsi]]
    | e -> failwithf "declToInstrs: %s %A" dest e

let transferToInstrs procName blocks = function
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
         JmpIf (E, labelf), []
         Jmp(labelt), []]
    | Transfer.Call(NonTail label, func, args) ->
        compileCall procName label blocks func args
    | Transfer.Call(Tail, func, args) ->
        compileTailCall procName false func args
    | Transfer.Apply(NonTail label, func, args) ->
        compileApply procName label blocks func args
    | Transfer.Apply(Tail, func, args) ->
        compileTailCall procName true func args
    | ForeignCall(label, foreignName, args) ->
        compileForeignCall label blocks foreignName args

/// This was used for approach with register allocation.
let saveArgs args instrs =
    let argToLoc = argumentToLocation (wordSize, wordSize) Rbp args
    let saveInstrs =
        List.map (fun arg -> Mov, [Map.find arg argToLoc; Var arg]) args
    match instrs with
    | head :: tail -> head :: saveInstrs @ tail
    | _ -> instrs

let saveToRootIfNeeded rootsCount decl =
    ()

let doPrimitiveUseHeap (_, simple) =
    let primitives =
        [ Cons
          Car
          Cdr
          MakeVector
          VectorRef
          MakeString
          StringInit
          MakeClosure
          ClosureRef
          MakeSymbol ]


    match simple with
    | Simple.Prim(p, _) -> List.contains p primitives
    | _ -> false

let selectInstructions (prog : Intermediate.Program) : Program =
    let mutable rootStackSlots = 0
    let rootStackVars = System.Collections.Generic.HashSet<string>()

    let moveBlockArgToRootStack arg =
        rootStackSlots <- rootStackSlots + 1
        Mov, [Var arg; RootStackSlot(rootStackSlots - 1)]

    let handleStmt procName blocks = function
        | Decl decl ->
            // check type of primitive.
            // if primitive allocates heap memory, copy to root stack.
            let instrs = declToInstrs decl
            if doPrimitiveUseHeap decl then
                let dest, _ = decl
                rootStackVars.Add(dest) |> ignore
            instrs

        | Transfer tran -> transferToInstrs procName blocks tran

    let handleBlock procName blocks (name, args, stmts) =
        let stmts = List.collect (handleStmt procName blocks) stmts
        if procName <> name then
            // Add args to the root stack unless current block is function entry block.
            // Arguments of the procedure is handled differently.
            // See convertVarsToSlots().
            rootStackVars.UnionWith(args) |> ignore
            (Label name, []) :: stmts
        else
            /// Do not add label to the function entry block.
            /// It will be added in CodePrinter.
            stmts

    let errorHandler =
        [Label(errorHandlerLabel), []
         Sub, [Int wordSize; Reg Rsp]] @
        callRuntime "asmError" @
        [Label(globVarErrorHandler), []] @
        callRuntime "globVarError" @
        [Label(wrongArgCountHandler), []] @
        callRuntime "wrongArgCountError" @
        [Label(procErrorHandler), []
         Mov, [Reg R10; Reg Rcx]] @ // Pass address of the caller.
        callRuntime "procError"

    let entryPointLabel = freshLabel "entryPoint"

    let handleDef (proc : Intermediate.Function) =
        let instrs =
            List.collect (handleBlock proc.Name proc.Blocks) proc.Blocks
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
          SlotsOccupied = slots
          RootStackSlots = rootStackSlots
          RootStackVars = Seq.toList rootStackVars }

    let entryPointDef =
        match prog.Main.Blocks with
        | (_, args, stmts) :: restBlocks ->
            { prog.Main with
                Name = entryPointLabel
                Blocks = (entryPointLabel, args, stmts) :: restBlocks }
        | _ -> failwith "selectInstructions: wrong main function"
    let procs = entryPointDef :: prog.Procedures

    { Procedures = List.map handleDef procs
      Main = emptyFuncDef
      Globals = prog.Globals
      GlobalsOriginal = prog.GlobalsOriginal
      ConstantsNames = prog.ConstantsNames
      ErrorHandler = errorHandler
      Entry = entryPointLabel
      Strings = prog.Strings }
