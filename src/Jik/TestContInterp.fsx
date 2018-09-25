#load "Base.fs"
#load "ContinuationInterpreter.fs"

open ContinuationInterpreter

let runTest input expected : unit=
    try
        let output = evaluateString input
        if output <> expected then
            printfn "FAIL. input <%s>\nexpected<%s>\ngot<%s>" input expected output
        else ()
        ()
    with
    | e -> 
        if expected <> "***" then
            printfn "FAIL. Exception <%s>\ninput <%s>" e.Message input
        ()

runTest "(car (cons 1 2))" "1"
runTest "((lambda (x) x) 1)" "1"
runTest "(call/cc (lambda (return) (return 2) 3))" "2"
runTest "(catch 'bar 1)" "1"
runTest "(catch 'bar 1 2 3)" "3"
runTest "(catch 'bar (throw 'bar 11))" "11"
runTest "(catch 'bar (* 2 (throw 'bar 5)))" "5"
runTest "((lambda (f) 
   (catch 'bar (* 2 (f 5))) )
 (lambda (x) (throw 'bar x)) )"
    "5"
runTest "((lambda (f) 
   (catch 'bar (* 2 (catch 'bar (* 3 (f 5))))) )
 (lambda (x) (throw 'bar x)) )"
    "10"
runTest "(catch 2
  (* 7 (catch 1
         (* 3 (catch 2
                (throw 1 (throw 2 5)) )) )) )"
    "105"
runTest "(catch 2 (* 7 (throw 1 (throw 2 3))))" "***"

runTest "(block foo 33)" "33"
runTest "(block foo 1 2 33)" "33"
runTest "(block foo (* 1 (return-from foo 2)))" "2"
runTest "(return-from foo 3)" "***"
runTest "((block foo (lambda (x) (return-from foo x))) 3)" "***"
runTest "((block a 
   (* 2 (block b (return-from a (lambda (x) (return-from b x))))) )
 3 )" "***"
runTest "((block a 
   (* 2 (block b (return-from a (lambda (x) (return-from a x))))) )
 3 )" "***"
runTest "(block foo 
  ((lambda (exit)
    (* 2 (block foo
            (* 3 (exit 5)) )) )
    (lambda (x) (return-from foo x)) ) )" "5"