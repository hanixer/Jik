module TestDriver


open System
open System.IO
open Compile

let cygwin = @"C:\cygwin64\bin\"

let root = Util.getPathRelativeToRoot ""



let runCompiled () =
    let res = Util.executeProcess("./a.exe", "")
    res.stdout

let mutable testCases : List<string * (List<string * string>)> = []

let private runTestImpl useLib input =
    let outFile = root + "/misc/a.exe"
    compileSchemeStringToBinary useLib input outFile
    Util.executeProcess(outFile, "").stdout

let addTests str tests =
    testCases <- (str, tests) :: testCases

let singleTestWrapper action input expected =
    try
        let output = action input
        if output <> expected then
            printfn "-----------------------------------"
            printfn "FAIL!"
            printfn "expected:\n%s" expected
            printfn "got:\n%s" output
            printfn "input:\n%s" input
            printfn "-----------------------------------"
            printfn ""
            Environment.Exit(111)
        with
        | e ->
            printfn "-----------------------------------"
            printfn "FAIL!"
            printfn "exception:\n%s" e.Message
            printfn "stack trace:\n%s" e.StackTrace
            printfn "input:\n%s" input
            printfn "-----------------------------------"
            printfn ""

let runTest useLib input expected =
    let action input = runTestImpl useLib input
    singleTestWrapper action input expected

let runTestGroup useLib testName tests =
    let fold (passed, i) (input, expected) =
        try
            let output = runTestImpl useLib input
            if output <> expected then
                printfn "-----------------------------------"
                printfn "FAIL! '%s' #%d" testName i
                printfn "expected:\n%s" expected
                printfn "got:\n%s" output
                printfn "input:\n%s" input
                printfn "-----------------------------------"
                printfn ""
                passed, i + 1
            else
                passed + 1, i + 1
        with
        | e ->
            printfn "-----------------------------------"
            printfn "FAIL! '%s' #%d" testName i
            printfn "exception:\n%s" e.Message
            printfn "stack trace:\n%s" e.StackTrace
            printfn "input:\n%s" input
            printfn "-----------------------------------"
            printfn ""
            passed, i + 1

    let n = List.fold fold (0, 1) tests |> fst
    printfn "Test \"%s\": completed %d / %d" testName n tests.Length

let runTestGroupWithLib =
    runTestGroup true

let runTestWithLib source expected =
    let action input =
        let exe = Util.getPathRelativeToRoot ("misc/a.exe")
        Compile.compileSchemeStringToBinary true source exe
        Util.executeProcess(exe, "").stdout
    singleTestWrapper action source expected