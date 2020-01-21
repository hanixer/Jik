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

let defaultOutFile = Util.getPathRelativeToRoot ("misc/a.exe")

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

let compileAsmToBinary asmSource outFile =
    let filename = getTempFile()

    File.WriteAllText(filename, asmSource)
    File.WriteAllText(Util.getPathRelativeToRoot "misc/test.s", asmSource)

    try
        gccCompile filename outFile
    finally
        File.Delete filename

let compileSchemeStringToString useLibrary source =
    let source =
        if useLibrary then
            let libPath = Util.getPathRelativeToRoot "library/library.mscm"
            let lib = SourceFileReader.readFilesExpandingModules [libPath]
            lib + "\n" + source
        else
            source

    stringToAsmForm source

let schemeMainName = "schemeMain.s"

/// Compile all files (which may contain library files) to a binary.
let compileMany files outFile =
    Common.resetFreshLabels()

    let schemeStringToModule s =
        s
        |> stringToProgram
        |> allCoreTransformations
        |> allIntermediateTransformations
        |> allCodegenTransformations

    let handleFile file =
        File.ReadAllText file
        |> schemeStringToModule

    let entryOfModule (prog : Codegen.Program) = prog.Entry

    let modules = List.map handleFile files
    let entryPoints = List.map entryOfModule modules
    let mainModule = createMainModule entryPoints
    let asmFiles = List.map (fun (file : string) ->
        let file = Path.GetFileName(file)
        miscPath + file + ".s") files

    List.zip asmFiles modules
    |> List.iter (fun (asmFile, m) ->
        let text = Codegen.programToString m
        File.WriteAllText(asmFile, text))

    let mainText = Codegen.programToString mainModule
    let mainFile = miscPath + schemeMainName
    File.WriteAllText(mainFile, mainText)

    let runtime = Util.getPathRelativeToRoot "c/runtime.c"
    let allAsmFiles = asmFiles @ [mainFile] |> String.concat " "
    let res = Util.executeProcess("gcc", " -g -std=c99 " + allAsmFiles + " " + runtime + " -o " + outFile)
    if res.stderr.Trim().Length > 0 then
        failwithf "gcc error:\n%s\n\n" res.stderr
    if res.stdout.Trim().Length > 0 then
        printfn "gcc output:\n%s\n\n" res.stdout

    ()

let libraryFiles = [
    "library/library.scm"
]

let compileSchemeStringToBinary useLibrary source outFile =
    let sourceFileName = miscPath + "sourceFile.scm"
    let additional = if useLibrary then libraryFiles else []
    File.WriteAllText(sourceFileName, source)
    compileMany (additional @ [sourceFileName]) outFile