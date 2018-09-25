#load "Base.fs"
open System
#load "Optimized.fs"

open Optimized

let runTest input expected : unit=
    try
        let output = evaluateToString input
        if output <> expected then
            printfn "FAIL. input <%s>\nexpected<%s>\ngot<%s>" input expected output
        else ()
        ()
    with
    | e -> 
        if expected <> "***" then
            printfn "FAIL. Exception <%s>\ninput <%s>" e.Message input
        ()

runTest "1" "1"
runTest "(begin
    (set! x 2)
    (set! x 3)
    x)" "3"
runTest "(begin
    (set! x 1)
    (if x 1 2))" "1"
runTest "(begin
    (set! x 1)
    ((lambda (yyy zzz)
        (set! x yyy)) 3 2)
    x)" "3"