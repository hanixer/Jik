// Learn more about F# at http://fsharp.org

open System
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

[<EntryPoint>]
let main argv =
    let paths =
        if argv.Length > 0 then List.ofArray argv
        else [getPathRelativeToRoot "library/library.mscm"; getPathRelativeToRoot "examples/one.scm"]
    let source = SourceFileReader.readFilesExpandingModules paths
    let compiled = stringToAsmForm source
    let outFile = Util.getPathRelativeToRoot ("misc/" + "a.exe")
    compileToBinary compiled outFile
    0 // return an integer exit code
