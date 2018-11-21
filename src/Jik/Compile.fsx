#load "RuntimeConstants.fs"
#load "SExpr.fs"
#load "Util.fs"
#load "Compile.fs"
#load "TestDriver.fs"
// #load "scripts/test1.3.fsx"
// #load "scripts/test1.4.fsx"
// #load "scripts/test1.5.fsx"
// #load "scripts/test1.6.fsx"
#load "scripts/test1.7.fsx"
// #load "scripts/test1.8.fsx"
// #load "scripts/test1.9.fsx"

open System
open SExpr
open System.IO
open TestDriver

let transformFile infile outfile =
    let text = "(" + File.ReadAllText infile + ")"
    use tw  = System.IO.File.CreateText(outfile)

    fprintf tw """#load "../TestDriver.fs"
open TestDriver

"""

    let processTest = function
        | List [input; Symbol "=>"; String expected] ->
            let input = sexprToString input
            fprintf tw "@%A, %A" input expected
        | e -> failwithf "processTest: %A" e

    let processTestsGroup = function
        | List (Symbol _ :: String name :: tests) ->
            fprintf tw "\naddTests %A [\n" name
            List.iter (fun t -> 
                fprintf tw "    "
                processTest t
                fprintfn tw "") (tests)
            fprintfn tw "]"

        | e -> failwithf "processTestsGroup: %A" e

    let e = stringToSExpr text |> consToList
    List.iter processTestsGroup e
    e

let ts = [
    // "(let ((x 1))
    //     (set! x fx+)
    //     (x 1 2))", "3\n"
//     "(letrec ((fact (lambda (x) 
//     (if (fx< x 2)
//         x
//         (fx* x (fact (fx- x 1)))))))
//   (fact 3))", "6\n"
    // "(fx+ 1 (fx* 22 33))", "727\n"
    // @"(fx+ (fx+ 1 -2) -3)", "-4\n"
    // "(if (fx= 1 2) (fx+ 1 2) (fx+ 3 4))", "7\n"
    // "(if 1 2 3)", "2\n"
    // "(let ((b (fx< 1 2)))
    //     (if b 2 3))", "2\n"
    // "(+ (+ 1 4) (+ 2 3))", "10\n"
    // "(if 4 5 (if #f 2 3))", "3\n"
    // "(vector-ref (make-vector 4) 1)", "0\n"
    "(let ([v0 (make-vector 2)])
     (let ([v1 (make-vector 2)])
       (vector-set! v0 0 100)
       (vector-set! v0 1 200)
       (vector-set! v1 0 300)
       (vector-set! v1 1 400))
     (vector-ref v0 1))", "200\n"
]


// runTestsWithName Compile.compile "my" ts
// runAllTests() 
