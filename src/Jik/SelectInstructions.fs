module SelectInstructions

open Graph
open Core
open Intermediate
open RuntimeConstants
open Primitive
open Codegen
open Common
open System

let convertToFixnum n = n <<< fixnumShift

let moveInt n var = [Mov, [Operand.Int n; Operand.Var var]]

let setVoid dest = moveInt voidTag dest

let stackArgsCount args = List.length args - List.length registersForArgs

let moveClosureArgs args =
    List.mapi (fun i arg ->
        // Start at offset 2 * wordsize:
        // One word for forward bit,
        // other word for label - procedure address.
        Mov, [Var arg; Deref((i + 2) * wordSize - closureTag, R11)]) args

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
     Mov, [Reg R15; GlobalValue("rootStackCurr")]
     Call name, []
     Add, [Int spChange; Reg Rsp]
     Mov, [Reg Rax; Var resultVar]
     Jmp label, []]

let convertConditionFlagToBool cc dest =
    [Set cc, [Reg Al]
     Movzb, [Reg Al; Reg Rax]
     Sal, [Operand.Int boolBit; Reg Rax]
     Or, [Operand.Int falseLiteral; Reg Rax]
     Mov, [Reg Rax; dest]]

let comparisonInstrs var1 var2 cc dest =
    [Mov, [Var var1; Reg Rax]
     Cmp, [Var var2; Reg Rax]] @
    convertConditionFlagToBool cc dest

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

let readCell object tag dest =
    [Mov, [object; Reg R11]
     Mov, [Deref(-tag, R11); dest]]

let compileIsOfTypeComplex dest var1 mask tag =
    let comparEnd = freshLabel ".L"
    [Mov, [Var var1; Reg Rax]
     And, [Int typedObjectMask; Reg Rax]
     Cmp, [Int typedObjectTag; Reg Rax]
     JmpIf(Ne, comparEnd), []] @
    readCell (Var var1) typedObjectTag (Var dest) @
    [And, [Int mask; Var dest]
     Cmp, [Int tag; Var dest]] @
    [Label(comparEnd), []] @
    compileSetOnEqual dest

let callAllocate size dest =
    [Mov, [size; Reg Rdx] // Size to allocate.
     Mov, [Reg Rsi; Deref(0, R15)]
     Mov, [Reg R15; Reg Rcx] // Root stack pointer.
     Mov, [Reg Rsp; Reg Rbp]
     Sub, [Int wordSize; Reg Rsp]] @
    alignStackPointer @
    [Sub, [Int stackShadowSpace; Reg Rsp]
     Call("allocate"), []
     Mov, [Deref(0, R15); Reg Rsi]
     Mov, [Reg Rbp; Reg Rsp]
     Mov, [Reg Rax; dest]]

let compileMakeVector size dest =
    [Mov, [size; Reg Rcx]] @
    callRuntime "allocateVector" @
    [Mov, [Reg Rax; Var dest]]

/// Very strange: why do we move size to RAX after allocation?
let compileMakeString size dest =
    [Mov, [size; Reg Rcx]] @
    callRuntime "allocateString" @
    [Mov, [Reg Rax; Var dest]]

let compileMakePair car cdr dest =
    callRuntime "allocatePair" @
    [Mov, [Reg Rax; Reg R11]
     Mov, [car; Deref(-pairTag + wordSize, R11)]
     Mov, [cdr; Deref(-pairTag + 2 * wordSize, R11)]
     Mov, [Reg R11; dest]]

let computeVecElementOffset vec index =
    [Mov, [Var vec; Reg R11]
     Mov, [index; Reg R12]
     Sar, [Int fixnumShift; Reg R12]
     Add, [Int 1; Reg R12]]

let floatNumberSize = 16

let compileMakeFloat initOp initVal dest =
    [Mov, [Int floatNumberSize; Reg Rax]] @
    callAllocate (Reg Rax) (Reg R11) @
    [Mov, [Int 0; Deref(0, R11)]
     initOp, [initVal; Deref(wordSize, R11)]
     Or, [Int flonumTag; Reg R11]
     Mov, [Reg R11; dest]]

let floatConstant n dest =
    let i64 = BitConverter.DoubleToInt64Bits n
    compileMakeFloat Mov (Operand.Int64 i64) (Reg R11) @
    [Mov, [Reg R11; Var dest]]

let getFlonumData var dest =
    [Mov, [Var var; Reg R11]
     Mov, [Deref(-flonumTag + wordSize, R11); dest]]

let compileFlonumComparison cc var1 var2 dest =
    [Mov, [Var var1; Reg R11]
     Movsd, [Deref(-flonumTag + wordSize, R11); Reg Xmm0]
     Mov, [Var var2; Reg R11]
     FloatCompare, [Deref(-flonumTag + wordSize, R11); Reg Xmm0]] @
    convertConditionFlagToBool cc dest

let compileFlonumArithmetic op var1 var2 dest=
    [Mov, [Var var1; Reg R11]
     Movsd, [Deref(-flonumTag + wordSize, R11); Reg Xmm0]
     Mov, [Var var2; Reg R11]
     op, [Deref(-flonumTag + wordSize, R11); Reg Xmm0]] @
    compileMakeFloat Movsd (Reg Xmm0) (Var dest)

let rec declToInstrs (dest, x) =
    match x with
    | Simple.EmptyList -> moveInt nilLiteral dest
    | Simple.Int n -> moveInt (convertToFixnum n) dest
    | Simple.RawInt n -> moveInt n dest
    | Simple.FloatNumber n -> floatConstant n dest
    | Simple.Void -> setVoid dest
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
        comparisonInstrs var1 var2 Cc.L (Var dest)
    | Simple.Prim(Prim.Le, [var1; var2]) ->
        comparisonInstrs var1 var2 Cc.Le (Var dest)
    | Simple.Prim(Prim.Ge, [var1; var2]) ->
        comparisonInstrs var1 var2 Cc.Ge (Var dest)
    | Simple.Prim(Prim.Gt, [var1; var2]) ->
        comparisonInstrs var1 var2 Cc.G (Var dest)
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
         And, [Int boolMask; Reg Rax]
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
        compileMakePair (Var var1) (Var var2) (Var dest)
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
         Mov, [Var value; Deref(-pairTag + wordSize, R11)]] @
        setVoid dest
    | Simple.Prim(Prim.SetCdr, [pair; value]) ->
        [Mov, [Var pair; Reg R11]
         Mov, [Var value; Deref(-pairTag + 2 * wordSize, R11)]] @
        setVoid dest
    // Vectors.
    | Simple.Prim(Prim.MakeVector, [var1]) ->
        compileMakeVector (Var var1) dest
    | Simple.Prim(Prim.VectorLength, [var1]) ->
        [Mov, [Var var1; Reg R11]
         Mov, [Deref(-typedObjectTag, R11); Var dest]
         Sar, [Int (vectorSizeShift - fixnumShift); Var dest]]
    | Simple.Prim(Prim.IsVector, [var1]) ->
        compileIsOfTypeComplex dest var1 vectorMask vectorTag
    | Simple.Prim(Prim.VectorSet, [vec; index; value]) ->
        computeVecElementOffset vec (Var index) @
        [Mov, [Var value; Deref4(-typedObjectTag, R11, R12, wordSize)]] @
        setVoid dest
    | Simple.Prim(Prim.VectorRef, [vec; index]) ->
        computeVecElementOffset vec (Var index) @
        [Mov, [Deref4(-typedObjectTag, R11, R12, wordSize); Var dest]]
    // Strings.
    | Simple.Prim(Prim.MakeString, [var1]) ->
        compileMakeString (Var var1) dest
    | Simple.Prim(Prim.StringLength, [var1]) ->
        [Mov, [Var var1; Reg R11]
         Mov, [Deref(-typedObjectTag, R11); Var dest]
         Sar, [Int (stringSizeShift - fixnumShift); Var dest]]
    | Simple.Prim(Prim.IsString, [var1]) ->
        compileIsOfTypeComplex dest var1 stringMask stringTag
    | Simple.Prim(Prim.StringSet, [instance; index; value]) ->
        [Mov, [Var instance; Reg R11]
         Mov, [Var index; Reg R12]
         Sar, [Int fixnumShift; Reg R12]
         Add, [Int wordSize; Reg R12]
         Mov, [Var value; Reg Rax]
         Sar, [Int charShift; Reg Rax]
         Movb, [Reg Al; Deref4(-typedObjectMask, R11, R12, 1)]] @
        setVoid dest
    | Simple.Prim(Prim.StringRef, [instance; index]) ->
        [Mov, [Var instance; Reg R11]
         Mov, [Var index; Reg R12]
         Sar, [Int fixnumShift; Reg R12]
         Add, [Int wordSize; Reg R12]
         Movzb, [Deref4(-typedObjectMask, R11, R12, 1); Var dest]
         Sal, [Int charShift; Var dest]
         Or, [Int charTag; Var dest]]
    | Simple.Prim(Prim.StringInit, [stringData]) ->
        // TODO: rework this case.
        [Lea stringData, [Var dest]
         Or, [Int typedObjectTag; Var dest]]
    // Closures.
    | Simple.Prim(Prim.MakeClosure, label :: args) ->
        let bytes = (List.length args + 2) * wordSize
        let cellsCount = convertToFixnum (List.length args + 1)
        [Mov, [Int (List.length args); Reg Rcx]
         Lea(label), [Reg Rdx]] @
        callRuntime "allocateClosure" @
        [Mov, [Reg Rax; Reg R11]] @
        moveClosureArgs args @ // Remaining cells are for free variables.
        [Mov, [Reg R11; Var dest]]
    | Simple.Prim(Prim.ClosureRef, [var1]) ->
        [Mov, [Var var1; Reg Rax]
         Sar, [Int fixnumShift; Reg Rax]
         Mov, [Deref4(-closureTag + 2 * wordSize, Rsi, Rax, wordSize); Var dest]]
    | Simple.Prim(Prim.IsProcedure, [var1]) ->
        compileIsOfType dest var1 closureMask closureTag
    // Globals.
    | Simple.Prim(Prim.GlobalSet, [glob; value]) ->
        [Mov, [Var value; GlobalValue(glob)]] @
        setVoid dest
    | Simple.Prim(Prim.GlobalRef, [glob]) ->
        // Save address of global var in rcx for C error handler.
        [Mov, [GlobalValue(glob); Reg Rax]
         Cmp, [Int unboundLiteral; Reg Rax]
         Lea(glob), [Reg Rcx]
         JmpIf(E, globVarErrorHandler), []
         Mov, [GlobalValue(glob); Var dest]]
    | Simple.Prim(Prim.GlobalRefUncheck, [glob]) ->
         [Mov, [GlobalValue(glob); Var dest]]
    // Symbols.
    | Simple.Prim(Prim.MakeSymbol, [var1]) ->
        let allocatedSize = 2 * wordSize
        let savedSize = convertToFixnum 1
        callAllocate (Int allocatedSize) (Reg R11) @
        [Mov, [Int savedSize; Deref(0, R11)] // The first cell is for size and forwarding bit.
         Mov, [Var var1; Deref(wordSize, R11)]] @ // The second is for string.
        [Or, [Int symbolTag; Reg R11]
         Mov, [Reg R11; Var dest]]
    | Simple.Prim(Prim.SymbolString, [var1]) ->
        // String lives in the second cell of the object.
        [Mov, [Var var1; Reg R11]
         Mov, [Deref(-symbolTag + wordSize, R11); Var dest]]
    | Simple.Prim(Prim.IsSymbol, [var1]) ->
        compileIsOfType dest var1 symbolMask symbolTag

    | Simple.Prim(Prim.EofObject, []) -> moveInt eofLiteral dest
    | Simple.Prim(Prim.IsEofObject, [var1]) ->
        compileIsOfType dest var1 eofMask eofLiteral

    | Simple.Prim(Prim.FlonumToFixnum, [var]) ->
        [Mov, [Var var; Reg R11]
         Movsd, [Deref(-flonumTag + wordSize, R11); Reg Xmm0]
         ConvertFloatToInt, [Reg Xmm0; Var dest]
         Sal, [Int fixnumShift; Var dest]]
    | Simple.Prim(Prim.FixnumToFlonum, [var]) ->
        [Mov, [Var var; Reg Rax]
         Sar, [Int fixnumShift; Reg Rax]
         Pxor, [Reg Xmm0; Reg Xmm0]
         ConvertIntToFloat, [Reg Rax; Reg Xmm0]] @
        compileMakeFloat Movsd (Reg Xmm0) (Var dest)
    | Simple.Prim(Prim.FlonumEq, [var1; var2]) ->
        [Mov, [Var var1; Reg R11]
         Mov, [Deref(-flonumTag + wordSize, R11); Reg R11]
         Mov, [Var var2; Reg R12]
         Mov, [Deref(-flonumTag + wordSize, R12); Reg R12]
         Cmp, [Reg R11; Reg R12]] @
         compileSetOnEqual dest
    | Simple.Prim(Prim.FlonumLt, [var1; var2]) ->
        compileFlonumComparison B var1 var2 (Var dest)
    | Simple.Prim(Prim.FlonumLe, [var1; var2]) ->
        compileFlonumComparison Be var1 var2 (Var dest)
    | Simple.Prim(Prim.FlonumGt, [var1; var2]) ->
        compileFlonumComparison A var1 var2 (Var dest)
    | Simple.Prim(Prim.FlonumGe, [var1; var2]) ->
        compileFlonumComparison Ae var1 var2 (Var dest)
    | Simple.Prim(Prim.FlonumAdd, [var1; var2]) ->
        compileFlonumArithmetic Addsd var1 var2 dest
    | Simple.Prim(Prim.FlonumSub, [var1; var2]) ->
        compileFlonumArithmetic Subsd var1 var2 dest
    | Simple.Prim(Prim.FlonumMul, [var1; var2]) ->
        compileFlonumArithmetic Mulsd var1 var2 dest
    | Simple.Prim(Prim.FlonumDiv, [var1; var2]) ->
        compileFlonumArithmetic Divsd var1 var2 dest
    | Simple.Prim(Prim.IsFlonum, [var1]) ->
        compileIsOfType dest var1 flonumMask flonumTag

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

let doesVarAllocatedOnHeap (_, simple) =
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
          MakeSymbol
          FlonumAdd
          FlonumSub
          FlonumMul
          FlonumDiv
          FixnumToFlonum ]


    match simple with
    | Simple.Prim(p, _) -> List.contains p primitives
    | Simple.FloatNumber _ -> true
    | _ -> false

let selectInstructions (prog : Intermediate.Program) : Program =
    let mutable rootStackSlots = 0
    let rootStackVars = System.Collections.Generic.HashSet<string>()

    let handleStmt procName blocks = function
        | Decl decl ->
            // check type of primitive.
            // if primitive allocates heap memory, copy to root stack.
            let instrs = declToInstrs decl
            if doesVarAllocatedOnHeap decl then
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
      Constants = prog.Constants }
