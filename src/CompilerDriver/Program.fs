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

let compileLibrary =
    let filename = __SOURCE_DIRECTORY__ + "\\..\\..\\library\\library.scm"
    let source = File.ReadAllText(filename)
    let codegen = stringToAsmForm source
    ()

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
