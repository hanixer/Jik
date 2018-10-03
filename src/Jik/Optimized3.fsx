#load "Base.fs"
#load "Optimized3.fs"

open Optimized3

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
runTest "(begin
    (set! x 1)
    ((lambda (yyy zzz a)
        (set! x yyy)) 3 2 5)
    x)" "3"
runTest "(begin
    (set! foo (lambda (x)
        (+ x x)))
    (foo (foo 2)))" "8"

runTest "(begin
    (set! max 10000)
    (set! foo (lambda (x acc)
        (if (> x max)
            acc
            (foo (+ x 1) (+ x acc)))))
    (foo 0 0))" "50005000"

runTest "(begin
    (set! x +)
    (x 1 2))" "3"