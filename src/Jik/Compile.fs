module Compile

open System
open SExpr
open Util
open Core
open Graph
open RuntimeConstants
open Display
open Intermediate
open Codegen
open RegisterAllocation
open SelectInstructions
open System.IO
open System.Text

let miscPath = Util.getPathRelativeToRoot "misc/"

let saveToFile filename str = System.IO.File.WriteAllText(filename, str)

let printIr s =
    (fun prog ->
    prog
    |> Intermediate.programToString
    |> saveToFile (miscPath + s)
    prog)

let sourceFile = ""

let allCodegenTransformations =
    selectInstructions
    >> revealGlobals
    >> convertVarsToSlots
    >> convertSlots
    >> addFuncPrologAndEpilog
    >> patchInstr

let stringToAsmForm s =
    s
    |> stringToProgram
    |> allCoreTransformations
    |> allIntermediateTransformations
    |> printIr "test.ir"
    |> allCodegenTransformations
    |> Codegen.programToString

let getTempFile () =
    let filename = Guid.NewGuid().ToString() + ".s"
    Util.getPathRelativeToRoot ("misc/" + filename)

let gccCompile filename outFile =
    let runtime = Util.getPathRelativeToRoot "c/runtime.c"
    let res = Util.executeProcess("gcc", filename + " -g -std=c99 " + runtime + " -o " + outFile)
    if res.stderr.Trim().Length > 0 then
        failwithf "gcc error:\n%s\n\n" res.stderr
    if res.stdout.Trim().Length > 0 then
        printfn "gcc output:\n%s\n\n" res.stdout

let compileToBinary asmSource outFile =
    let filename = getTempFile()

    File.WriteAllText(filename, asmSource)
    File.WriteAllText(Util.getPathRelativeToRoot "misc/test.s", asmSource)

    try
        gccCompile filename outFile
    finally
        File.Delete filename

let compileSchemeString useLibrary source outFile =
    let source =
        if useLibrary then
            let lib = SourceFileReader.readFilesExpandingModules ["library/library.mscm"]
            lib + "\n" + source
        else
            source

    let compiled = stringToAsmForm source
    compileToBinary compiled outFile