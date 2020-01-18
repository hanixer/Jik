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
    Compile.compileSchemeString true source exe
    let res = runCompiled()
    if res <> expected then
        printfn "got: %s" res

[<EntryPoint>]
let main argv =
    let c = Compile.stringToAsmForm
    // runSingleTest c "(letrec ([f (lambda (x) (+ x 1))]) (f 1))" "2\n"
    runTestGroup c "letLoop" letLoop
    // runTestStringWithLibrary "(memq 1 '())" "#f\n"
    // runTestGroup c "variable arity without rest arguments" variableArity
    // runTestGroup c "variable arity using rest arguments" variableArityUsingRest
    // runTestGroup c "calls" callFailure
    // runTestGroup c "deep procs" deeplyProcedureTests
    // runTestGroup c "lambda" lambdaTests
    // runTestGroup c "basic" basicTests
    // runTestGroup c "boolean" booleanTests
    // runTestGroup c "vector" vectorTests
    // runTestGroup c "assignment" assignmentTests
    // runTestGroup c "andOr" andOrTests
    // runTestGroup c "pair" pairTests
    // runTestGroup c "setCarCdr" setCarCdrTests
    // runTestGroup c "whenUnless" whenUnlessTests
    // runTestGroup c "cond" condTests
    // runTestGroup c "letrec" letrecTests
    // runTestGroup c "list" listTests
    // runTestGroup c "num -> char" numcharTests
    // runTestGroup c "char?" isCharTests
    // runTestGroup c "string" stringTests
    // runTestGroup c "foreign-call" foreignCallTests


    0
