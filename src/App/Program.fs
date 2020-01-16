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
let miscPath = __SOURCE_DIRECTORY__ + "\\..\\..\\misc\\"

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
    |> stackCorrections
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
    |> stackCorrections
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

[<EntryPoint>]
let main argv =
    // runTestsWithName testCodegen "calls" callFailure
    // runTestsWithName testCodegen "deep procs" deeplyProcedureTests
    // runTestsWithName testCodegen "lambda" lambdaTests
    // runTestsWithName testCodegen "basic" basicTests
    // runTestsWithName testCodegen "boolean" booleanTests
    // runTestsWithName testCodegen "vector" vectorTests
    // runTestsWithName testCodegen "assignment" assignmentTests
    // runTestsWithName testCodegen "andOr" andOrTests
    // runTestsWithName testCodegen "pair" pairTests
    // runTestsWithName testCodegen "setCarCdr" setCarCdrTests
    // runTestsWithName testCodegen "whenUnless" whenUnlessTests
    // runTestsWithName testCodegen "cond" condTests
    // runTestsWithName testCodegen "letrec" letrecTests
    // runTestsWithName testCodegen "list" listTests
    // runTestsWithName testCodegen "num -> char" numcharTests
    // runTestsWithName testCodegen "char?" isCharTests
    // runTestsWithName testCodegen "string" stringTests
    // runTestsWithName testCodegen "foreign-call" foreignCallTests

    // runTestsWithName testAllStages "basic" basicTests
    // runTestsWithName testAllStages "boolean" booleanTests
    // runTestsWithName testAllStages "vector" vectorTests
    // runTestsWithName testAllStages "lambda" lambdaTests
    // runTestsWithName testAllStages "assignment" assignmentTests
    // runTestsWithName testAllStages "andOr" andOrTests
    // runTestsWithName testAllStages "pair" pairTests
    // runTestsWithName testAllStages "setCarCdr" setCarCdrTests
    // runTestsWithName testAllStages "whenUnless" whenUnlessTests
    // runTestsWithName testAllStages "cond" condTests
    // runTestsWithName testAllStages "letrec" letrecTests
    // runTestsWithName testAllStages "list" listTests
    // runTestsWithName testAllStages "num -> char" numcharTests
    // runTestsWithName testAllStages "char?" isCharTests
    // runTestsWithName testAllStages "string" stringTests
    // runTestsWithName testAllStages "foreign-call" foreignCallTests
    // runSingleTest testAllStages "(define x 10) x" "x"
    // runTestsWithName testCodegen "b" basicTests


    // runSingleTest testCodegen "(let ((n 12)) (let ((f (lambda (m) (+ n m)))) (f 100)))" "112"

    testCodegen "(lambda (x y . z) x)" |> printfn "%s"

    0
