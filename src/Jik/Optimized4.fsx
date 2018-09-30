#load "Base.fs"
#load "Optimized4.fs"

open Optimized4

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
runTest "(if 1 2 3)" "2"
runTest "(if (if 1 2 3) 3 4)" "3"
runTest "(begin 1)" "1"
runTest "(begin 1 2 3)" "3"
runTest "(begin 2 (begin 3 4))" "4"