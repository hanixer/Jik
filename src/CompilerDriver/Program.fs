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
open Compile



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
