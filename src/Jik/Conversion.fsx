#load "Util.fs"
#load "Base.fs"
#load "Conversion.fs"

open Conversion
open System
open System.IO

let cygwin = @"C:\cygwin64\bin\"

let sd = __SOURCE_DIRECTORY__
let root = sd + "/../../"
let miscPath = root + "misc/"

let getTempFile () =
    System.IO.Path.Combine(miscPath, Guid.NewGuid().ToString() + ".s")

let gccCompile filename =
    let res = Util.executeProcess("gcc", filename + " -g")
    if res.stderr.Trim().Length > 0 then 
        failwithf "gcc error:\n%s\n\n" res.stderr
    if res.stdout.Trim().Length > 0 then 
        printfn "gcc output:\n%s\n\n" res.stdout

let runCompiled () =
    let res = Util.executeProcess("./a.exe", "")
    res.stdout

let runInput input =
    let prevCD = Environment.CurrentDirectory
    Environment.CurrentDirectory <- miscPath
    let prevPath = System.Environment.GetEnvironmentVariable("PATH")
    System.Environment.SetEnvironmentVariable("PATH", prevPath + ";" + cygwin)
    let filename = "out.c"
    compile filename ("(display " + input + ")")
    try
        gccCompile filename
        let output = runCompiled()
        let lminus1 = output.Length - 1
        if output.Length > 0 && output.[lminus1] = '\n' then
            output.Substring(0, lminus1)
        else output
    finally
        Environment.CurrentDirectory <- prevCD
        System.Environment.SetEnvironmentVariable("PATH", prevPath)

let runTest input expected =
    try
        let output = runInput input
        if output <> expected then
            printfn "FAIL! expected:\n<%s>\ngot:\n<%s>\ninput:\n%s\n" expected output input
    with
    | e -> 
        printfn "FAIL! exception:\n<%s>\nstacktrace:\n<%s>\ninput:\n%s\n" (e.Message) (e.StackTrace) input

runTest "1" "1\n"
runTest "-61" "-61\n"
runTest "#t" "#t\n"
runTest "#f" "#f\n"
runTest "()" "()\n"
runTest "(+ 1 2)" "3\n"
runTest "(+ 1 -2)" "-1\n"
runTest "(if #f 1 2)" "2\n"
runTest "(if #t 1 2)" "1\n"