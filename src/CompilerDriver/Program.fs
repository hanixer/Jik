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
    compileFilesToBinary true [toRoot "examples/maze.scm"] defaultOutFile
    0 // return an integer exit code
