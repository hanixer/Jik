module Compile

open System
open Util
open Core
open Intermediate
open Codegen
open Library
open SelectInstructions
open System.IO
open CodePrinter

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
    >> (fun x -> CodePrinter.programToString false x |> saveToFile (miscPath + "asm.s"); x)
    // >> revealGlobals // seems that this is useless
    >> convertGlobalRefs
    >> convertVarsToSlots
    >> convertSlots
    >> addFuncPrologAndEpilog
    >> patchInstr

let getTempFile () =
    let filename = Guid.NewGuid().ToString() + ".s"
    Util.getPathRelativeToRoot ("misc/" + filename)

let schemeMainName = "schemeMain.s"

/// Returns list of globals and list of their original scheme names
let collectGlobals modules =
    let collect pairs modul =
        let curr = List.zip modul.Globals modul.GlobalsOriginal
        Set.union (Set.ofList curr) pairs

    let pairs = List.fold collect Set.empty modules |> List.ofSeq

    List.unzip pairs

/// Compile all files (which may contain library files) to a binary.
let compileMany files outFile =
    Common.resetFreshLabels()

    let schemeStringToModule s =
        s
        |> stringToProgram
        |> allCoreTransformations
        |> allIntermediateTransformations
        |> printIr ("interm.ir")
        |> allCodegenTransformations

    let handleFile file =
        // try
            File.ReadAllText file
            |> schemeStringToModule
        // with
        // | e ->
        //     printfn "Error during compilation of file: %s" file
        //     printfn "Error: %A" e
        //     raise e

    let entryOfModule (prog : Codegen.Program) = prog.Entry

    let modules = List.map handleFile files
    let constants = List.collect (fun p -> p.ConstantsNames) modules
    let globals, globOriginal = collectGlobals modules
    let entryPoints = List.map entryOfModule modules
    let mainModule = createMainModule constants globals globOriginal entryPoints
    let asmFiles = List.map (fun (file : string) ->
        let file = Path.GetFileName(file)
        miscPath + file + ".s") files

    List.zip asmFiles modules
    |> List.iter (fun (asmFile, m) ->
        let text = programToString false m
        File.WriteAllText(asmFile, text))

    let mainText = programToString true mainModule
    let mainFile = miscPath + schemeMainName
    File.WriteAllText(mainFile, mainText)

    let runtime = Util.getPathRelativeToRoot "c/runtime.c"
    let gc = Util.getPathRelativeToRoot "c/memory.c"
    let objects = Util.getPathRelativeToRoot "c/objects.c"
    let filesForGcc = runtime :: objects :: gc :: asmFiles @ [mainFile] |> String.concat " "
    let res = Util.executeProcess("gcc", " -g -std=c99 " + filesForGcc + " -o " + outFile)
    if res.stderr.Trim().Length > 0 then
        failwithf "gcc error:\n%s\n\n" res.stderr
    if res.stdout.Trim().Length > 0 then
        printfn "gcc output:\n%s\n\n" res.stdout

    ()

let compileSchemeStringToBinary useLibrary source outFile =
    let sourceFileName = miscPath + "sourceFile.scm"
    let libFiles = List.map (fun f -> getPathRelativeToRoot ("library/" + f)) libraryFiles
    let additional = if useLibrary then libFiles else []
    File.WriteAllText(sourceFileName, source)
    compileMany (additional @ [sourceFileName]) outFile

let compileFilesToBinary useLibrary files outFile =
    let libFiles = List.map (fun f -> getPathRelativeToRoot ("library/" + f)) libraryFiles
    let additional = if useLibrary then libFiles else []
    compileMany (additional @ files) outFile

do
    if not(Directory.Exists(miscPath)) then
        Directory.CreateDirectory(miscPath)
        |> ignore
