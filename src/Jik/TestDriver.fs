module TestDriver

open System
open System.IO

let cygwin = @"C:\cygwin64\bin\"

let sd = __SOURCE_DIRECTORY__
let root = sd + "/../../"
let miscPath = root + "misc/"

let getTempFile () =
    System.IO.Path.Combine(miscPath, Guid.NewGuid().ToString() + ".s")

let gccCompile filename =
    let res = Util.executeProcess("gcc", filename + " -g -std=c99 ../c/runtime.c")
    if res.stderr.Trim().Length > 0 then
        failwithf "gcc error:\n%s\n\n" res.stderr
    if res.stdout.Trim().Length > 0 then
        printfn "gcc output:\n%s\n\n" res.stdout

let runCompiled () =
    let res = Util.executeProcess("./a.exe", "")
    res.stdout

let mutable testCases : List<string * (List<string * string>)> = []

let runTest compile input =
    let prevCD = Environment.CurrentDirectory
    Environment.CurrentDirectory <- miscPath
    let prevPath = System.Environment.GetEnvironmentVariable("PATH")
    System.Environment.SetEnvironmentVariable("PATH", prevPath + ";" + cygwin)

    let compiled = compile input
    let filename = getTempFile()

    File.WriteAllText(filename, compiled)
    File.WriteAllText("test.s", compiled)
    try
        gccCompile filename
        let output = runCompiled()
        let lminus1 = output.Length - 1
        if output.Length > 0 && output.[lminus1] = '\n' then
            output.Substring(0, lminus1)
        else output
    finally
        File.Delete filename

        Environment.CurrentDirectory <- prevCD
        System.Environment.SetEnvironmentVariable("PATH", prevPath)

let addTests str tests =
    testCases <- (str, tests) :: testCases

let runSingleTest compile input expected =
    try
        let output = runTest compile input
        if output <> expected then
            printfn "-----------------------------------"
            printfn "FAIL!"
            printfn "expected:\n%s" expected
            printfn "got:\n%s" output
            printfn "input:\n%s" input
            printfn "-----------------------------------"
            printfn ""
        with
        | e ->
            printfn "-----------------------------------"
            printfn "FAIL!"
            printfn "exception:\n%s" e.Message
            printfn "stack trace:\n%s" e.StackTrace
            printfn "input:\n%s" input
            printfn "-----------------------------------"
            printfn ""

let runTestsWithName compile testName tests =
    let fold (passed, i) (input, expected) =
        try
            let output = runTest compile input
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
    printfn "Test<%s> run: completed %d / %d" testName n tests.Length
