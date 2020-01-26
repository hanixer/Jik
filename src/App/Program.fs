open SExpr
open Util
open Core
open Graph
open TestDriver
open RuntimeConstants
open Display
open Intermediate
open Codegen
open RegisterAllocation
open SelectInstructions
open System.IO
open System.Text
open TestCases

let saveToFile filename str = System.IO.File.WriteAllText(filename, str)
let miscPath = getPathRelativeToRoot "misc\\"

let printIr s =
    (fun prog ->
    prog
    |> Intermediate.programToString
    |> saveToFile (miscPath + s)
    prog)

let testInterf s =
    let ds, m =
        s
        |> stringToProgram
        |> fixArithmeticPrims
        |> alphaRename
        |> convertProgram
        |> printIr "test.ir"
        |> selectInstructions
        |> computeLiveness
        |> buildInterference
        |> failwithf "nothing more %A"

    use fileOut = new StreamWriter(miscPath)

    let handleLiv out live =
        Map.iter (fun k v ->
            fprintf out "%d) " k
            Seq.iter (function
                | Var var -> fprintf out "%s, " var
                | Reg reg -> fprintf out "%%%s, " (reg.ToString().ToLower())
                | _ -> ()) v
            fprintfn out "") live
        fprintfn out "\n"

    let handleDef def =
        let out = new StringWriter()
        Seq.iteri (fun i instr ->
            fprintf out "%d) " i
            showInstr out instr
            fprintfn out "") def.Instrs
        let out1 = new StringWriter()
        handleLiv out1 def.LiveBefore
        let out2 = new StringWriter()
        handleLiv out2 def.LiveAfter
        fprintfn fileOut "%s" ((out.ToString()) + (out1.ToString()) + (out2.ToString()))

    List.iter handleDef ds
    handleDef m

let testAllStages s =
    s
    |> stringToProgram
    |> fixArithmeticPrims
    |> convertGlobalRefs
    |> alphaRename
    |> assignmentConvert
    |> convertProgram
    |> Intermediate.analyzeFreeVars
    |> printIr "test2.ir"
    |> Intermediate.closureConversion
    |> printIr "test.ir"
    |> selectInstructions
    |> revealGlobals
    |> computeLiveness
    |> buildInterference
    |> allocateRegisters
    |> convertSlots
    |> addFuncPrologAndEpilog
    |> patchInstr
    |> Codegen.programToString

let testCodegen s =
    s
    |> stringToProgram
    |> fixArithmeticPrims
    |> convertGlobalRefs
    |> alphaRename
    |> assignmentConvert
    |> convertProgram
    |> Intermediate.analyzeFreeVars
    |> printIr "test2.ir"
    |> Intermediate.closureConversion
    |> printIr "test.ir"
    |> selectInstructions
    |> revealGlobals
    |> convertVarsToSlots
    |> convertSlots
    |> addFuncPrologAndEpilog
    |> patchInstr
    |> Codegen.programToString

let testLambda str =
    let r = stringToProgram str
    let r = fixArithmeticPrims r
    let r = convertGlobalRefs r
    let r = alphaRename r
    let r = assignmentConvert r
    let r = Intermediate.convertProgram r
    let r = Intermediate.analyzeFreeVars r
    let r = Intermediate.closureConversion r
    printIr "test.ir" r |> ignore

let runTestString source expected =
    let exe = Util.getPathRelativeToRoot ("misc/a.exe")
    Compile.compileSchemeStringToBinary true source exe
    let res = runCompiled()
    if res <> expected then
        printfn "got: %s" res

[<EntryPoint>]
let main argv =
    // runTestWithLib "(remainder 5 2)" "1\n"

    let s = "(lambda x
    (define a 1)
    (define b 2)
    a
    (define c 1)
    (define e 2)
    e)"
    let s = "(quote (and 1 2 3))"


    // let expr = stringToSExpr s
    // let desug = Desugar.desugar2 [] expr
    // printf "result: %s" (sexprToString desug)

    // runTestGroup true "writeInt" writeInt
    // runTestGroup false "quotientTests" quotientRemainderTests
    // runTestGroup true "eofTests" eofTests
    // runTestGroup false "exit" exitTest
    // runTestGroupWithLib "symbols" symbols
    // runTestGroupWithLib "apply nontail" applyNonTail
    // runTestGroupWithLib "apply tail" applyTail
    // runTestGroup false "complexConstants" complexConstants
    // runTestGroup false "variable arity without rest arguments" variableArity
    // runTestGroup false "variable arity using rest arguments" variableArityUsingRest
    // runTestGroup false "calls" callFailure
    // runTestGroup false "deep procs" deeplyProcedureTests
    // runTestGroup false "lambda" lambdaTests
    // runTestGroup false "boolean" booleanTests
    // runTestGroup false "vector" vectorTests
    // runTestGroup false "assignment" assignmentTests
    // runTestGroup false "pair" pairTests
    // runTestGroup false "setCarCdr" setCarCdrTests
    // runTestGroup false "cond" condTests
    // runTestGroup false "list" listTests
    // runTestGroup false "num -> char" numcharTests
    // runTestGroup false "char?" isCharTests
    // runTestGroup false "string" stringTests
    // runTestGroup false "foreign-call" foreignCallTests
    runTestGroup false "letLoop" letLoop
    // runTestGroup false "whenUnless" whenUnlessTests
    // runTestGroup false "andOr" andOrTests
    runTestGroup false "letrec" letrecTests
    // runTestGroup false "basic" basicTests

    // runTest false "(let f () 12)" "12\n"
    // runTest false "(letrec ([f (lambda () 12)]) (f))" "12\n"

    0
