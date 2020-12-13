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

let parseCommandLine args =
    let rec loop ifiles useLib ofile args = 
        match args with
        | "-o" :: file :: rest ->
            loop ifiles useLib file rest
        | "-nolib" :: rest ->
            loop ifiles false ofile rest
        | ifile :: rest -> 
            loop (ifile :: ifiles) useLib ofile rest
        | _ ->
            (List.rev ifiles, useLib, ofile)
    
    loop [] true "misc/a.exe" args

[<EntryPoint>]
let main argv =
    let toRoot = Util.getPathRelativeToRoot
    
    let sources, useLib, ofile = parseCommandLine (List.ofArray argv)

    compileFilesToBinary useLib sources ofile
    0 // return an integer exit code
