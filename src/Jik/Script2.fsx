#load "Base.fs"
#load "Graph.fs"
#load "Util.fs"
#load "Display.fs"
#load "TestDriver.fs"
#load "RuntimeConstants.fs"
#load "Base.fs"
#load "Core.fs"
#load "Intermediate.fs"
#load "Codegen.fs"

open Core
open Graph
open TestDriver
open RuntimeConstants
open Display
open Intermediate
open Codegen

let test s =
    stringToExpr s
    |> tope
    |> labelsToString
    |> printfn "%s"

let testInterf s =
    let labels = stringToExpr s|> tope
    let live = computeLiveAfter labels
    let graph = buildInterference labels live
    let allocated = allocateRegisters (selectInstructions labels, graph)
    printfn "labels:\n%s" (labelsToString labels)
    printfn "live: %A" live
    printfn "allocated:\n%s" (instrsToString allocated)
    printDot graph (__SOURCE_DIRECTORY__ + "../../../misc/out3.dot")

let e ="
(let ([a 1]
      [b 2]
      [c 3])
  (if (< a b)
    (let ([d 4]
          [e 5])
      (+ c (+ d e)))
    (let ([f 6])
      (+ a (+ c f)))))"

let tests = [
    "#t", "#t\n"
    "#f", "#f\n"
    "1", "1\n"
    "-1", "-1\n"
    "(+ 1 2)", "3\n"
    "(+ 1 (+ 2 3))", "6\n"
    "(+ (+ 1 4) (+ 2 3))", "10\n"
    "(< 1 2)", "#t\n"
    "(if 1 2 3)", "2\n"
    "(if #f 2 3)", "3\n"
    "(if (< 3 1) 2 3)", "3\n"
    "(if (if 1 2 3) 4 5)", "4\n"
    "(if 4 (if #f 2 3) 5)", "3\n"
    "(if 4 (if #t 8 9) (if #f 2 3))", "8\n"    
    "
(let ([v 1])
(let ([w 46])
(let ([x (+ v 7)])
(let ([y (+ 4 x)])
(let ([z (+ x w)])
(+ z (- y)))))))", "42\n"
    "
(let ([a 1]
      [b 2]
      [c 3])
  (if (< a b)
    (let ([d 4]
          [e 5])
      (+ c (+ d e)))
    (let ([f 6])
      (+ a (+ c f)))))", "12\n"
]

runTestsWithName compile2 "basic" tests

e |> testInterf