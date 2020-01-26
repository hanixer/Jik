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

    // runTestGroupWithLib "symbols" symbols
    // runTestGroupWithLib "apply nontail" applyNonTail
    // runTestGroupWithLib "apply tail" applyTail
    // runTestGroup true "eofTests" eofTests
    runTestGroup true "writeInt" writeInt
    // runTestGroup false "defineTests" defineTests
    // runTestGroup false "quotientTests" quotientRemainderTests
    // runTestGroup false "complexConstants" complexConstants
    // runTestGroup false "cond" condTests
    // runTestGroup false "variable arity without rest arguments" variableArity
    // runTestGroup false "variable arity using rest arguments" variableArityUsingRest
    // runTestGroup false "deep procs" deeplyProcedureTests
    // runTestGroup false "lambda" lambdaTests
    // runTestGroup false "boolean" booleanTests
    // runTestGroup false "vector" vectorTests
    // runTestGroup false "assignment" assignmentTests
    // runTestGroup false "pair" pairTests
    // runTestGroup false "setCarCdr" setCarCdrTests
    // runTestGroup false "list" listTests
    // runTestGroup false "num -> char" numcharTests
    // runTestGroup false "char?" isCharTests
    // runTestGroup false "string" stringTests
    // runTestGroup false "foreign-call" foreignCallTests
    // runTestGroup false "letLoop" letLoop
    // runTestGroup false "whenUnless" whenUnlessTests
    // runTestGroup false "andOr" andOrTests
    // runTestGroup false "letrec" letrecTests
    // runTestGroup false "basic" basicTests

    // runTest false "\"one way\"" "1\n"

    0
