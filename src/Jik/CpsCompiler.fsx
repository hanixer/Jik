#load "Base.fs"
#load "Graph.fs"
#load "Util.fs"
#load "Display.fs"
#load "TestDriver.fs"
#load "RuntimeConstants.fs"
#load "Base.fs"
#load "Core.fs"
#load "Cps.fs"
#load "Codegen.fs"

open Base
open Core
open Cps
open Codegen
open Graph
open TestDriver

/////////////////////////////////////////////////////////////////
/// Testing

let tryit s = 
    stringToSExpr s
    |> sexprToExpr
    |> exprToCps
    |> cpsToString
    |> printfn "%s"

let tryRename s = 
    stringToSExpr s
    |> sexprToExpr
    |> alphaRename Map.empty
    |> printfn "%A"

let test s =
    let cps =
        s
        |> stringToSExpr
        |> sexprToExpr
        |> assignmentConvert
        |> exprToCps
    analyzeFreeVars cps |> ignore
    cps
    |> cpsToCodes
    |> codesToString
    |> printfn "%s"

let testModified s =
    s
    |> stringToExpr
    |> findModifiedVars
    |> printfn "%A"
let testSelIn s =
    stringToCps2 s |> selectInstr |> printfn "%A"
let testCps s =
    stringToCps2 s
    |> cpsToString
    |> printfn "%A"
let printLive live =
    live 
    |> Map.iter (fun k v ->
        printfn "%s: %A" k v)
let testLive s = 
    let cps = stringToCps2 s
    let instrs, liveAfter =
        cps
        |> selectInstr
        |> computeLiveAfter
    let s2 = instrsToString instrs
    let liv2 = cps |> liveness
    printfn "%s" s2
    printfn "%s" (cpsToString cps)
    List.iteri (fun i live ->
        printfn "%d)" i
        Set.iter (fun var ->
            printf  "%s " var) live
        printfn "") liveAfter
    printfn "-----------------"
    liv2 |> printLive

let testInterf s =
    let cps = stringToCps2 s
    printfn "%s" (cpsToString cps)
    let instrs, graph =
        cps
        |> selectInstr
        |> computeLiveAfter
        |> buildInterference
    instrsToString instrs |> printfn "%s"
    printDot graph "../../misc/dot.gv"
let testInterf2 s =
    let cps = stringToCps2 s
    printfn "%s" (cpsToString cps)
    let liv = liveness cps
    let graph = interference cps liv
    printfn "----------------------"
    printLive liv
    printDot graph "../../misc/dot2.gv"

"(letrec (
(sort (lambda (lst)
    (if (pair? lst)
        (insert (car lst) (sort (cdr lst)))
        1)))
(insert (lambda (elem lst)
    (if (pair? lst)
        (let ((x (car lst))
            (l (cdr lst)))
            (if (< elem x)
                (cons elem (cons x l))
                (cons x (insert elem l))))
        (cons elem 1)))))
    (sort (cons 333 (cons 222 (cons 111 1)))))"
"1"
"(let ((f (lambda (a) (lambda (b) (+ a b)))))
    ((f 1) 2))"
"(lambda (a) (lambda (b) (lambda (c) (+ a b c))))"
"(lambda (a) (lambda (b) (opera a b)))"
"(letrec ((f (lambda (x) (opera f g x)))
          (g (lambda (y) (opera f g y))))
    (let ((x 0))
        (set! x (+ x 1))
        (if x 1234 4321)))"
"(let ((x 0))
        (set! x (+ x 1))
        (if x 1234 4321))"
"(letrec ((fact (lambda (x) 
    (if (< x 2)
        x
        (* x (fact (- x 1)))))))
  (fact 5))"
"(if (if 1 2 3) (if 111 222 333) 33)" 
"(+ 1 (- 22 33)))"
"(let ((a 1))
    (let ((b 2))
        (+ a b)))"

let tests = [
    "1", "1\n"
    "-1", "-1\n"
    "(+ 1 (- 22 33)))", "-10\n"
    "(let ((a 1))
    (let ((b 2))
        (+ a b)))", "3\n"
    "(let ((b (- 1))) (+ 43 b))", "42\n"
    "(let ([v 1])
(let ([w 46])
(let ([x (+ v 7)])
(let ([y (+ 4 x)])
(let ([z (+ x w)])
(+ z (- y)))))))", "42\n"
]

// runTestsWithName compile "basic" tests

let e = 
    "(let ([v 1])
        (let ([w 46])
        (let ([x (+ v 7)])
        (let ([y (+ 4 x)])
        (let ([z (+ x w)])
        (+ z (- y)))))))"
e |> testInterf2