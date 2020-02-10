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
    let toRoot = Util.getPathRelativeToRoot
    let defaultS = [toRoot "examples/queens.scm"]
    let sources =
        if argv.Length > 0 then
            argv |> List.ofArray |> List.filter (fun (a:string) -> a.StartsWith("-") |> not)
        else
            defaultS
    let useLib = Seq.contains "-nolib" argv |> not

    compileFilesToBinary useLib sources defaultOutFile
    0 // return an integer exit code
