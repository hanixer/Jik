module Compile

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

let compileSchemeString useLibrary source outFile =
    let source =
        if useLibrary then
            let lib = SourceFileReader.readFilesExpandingModules ["library/library.mscm"]
            lib + "\n" + source
        else
            source

    let compiled = stringToAsmForm source
    compileToBinary compiled outFile