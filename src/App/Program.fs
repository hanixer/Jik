
open SExpr
open Util
open Core
open Graph
open TestDriver
open RuntimeConstants
open Display
open Intermediate
open Codegen
open System.IO

let saveToFile filename str =
    System.IO.File.WriteAllText(filename, str)

let path = __SOURCE_DIRECTORY__ + "\\..\\..\\misc\\test.ir"

let printIr = (fun prog -> prog |> Intermediate.programToString |> saveToFile (path); prog)

let testInterf s =
  let ds, m =
    s 
    |> stringToProgram
    |> fixArithmeticPrims
    |> alphaRename
    |> revealFunctions 
    |> convertProgram
    |> printIr
    |> selectInstructions
    |> computeLiveness
    // |> buildInterference
    |> failwithf "nothing more %A"
    
  use fileOut = new StreamWriter(path)
  let handleLiv out live =
    Map.iter (fun k v -> 
      fprintf out "%d) " k
      Seq.iter (function
        | Var var -> fprintf out "%s, " var
        | Reg reg -> fprintf out "%%%s, " (reg.ToString().ToLower())
        | _ -> ()) v
      fprintfn out "") live
    fprintfn out "\n"
  let handleDef def =
    let out = new StringWriter()
    Seq.iteri (fun i instr ->
      fprintf out "%d) " i
      showInstr out instr
      fprintfn out "") def.Instrs
    let out1 = new StringWriter()
    handleLiv out1 def.LiveBefore
    let out2 = new StringWriter()
    handleLiv out2 def.LiveAfter
    fprintfn fileOut "%s" ((out.ToString()) + (out1.ToString())  + (out2.ToString()))
  List.iter handleDef ds
  handleDef m

let testMainTest s =
  s 
  |> stringToProgram
  |> fixArithmeticPrims
  |> alphaRename
  |> assignmentConvert
  |> revealFunctions 
  |> convertProgram
  |> Intermediate.analyzeFreeVars
  |> Intermediate.closureConversion
  |> printIr
  |> selectInstructions
  |> revealGlobals
  |> computeLiveness
  |> buildInterference
  |> allocateRegisters
  |> convertSlots
  |> stackCorrections
  |> patchInstr
  |> Codegen.programToString

let testLambda str =
    let r = stringToProgram str
    let r = fixArithmeticPrims r
    let r = convertGlobalRefs r
    let r = alphaRename r
    let r = assignmentConvert r
    let r = revealFunctions r
    let r = Intermediate.convertProgram r
    let r = Intermediate.analyzeFreeVars r
    let r = Intermediate.closureConversion r
    printIr r |> ignore

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
let e2 = "(define (one) 1)
(+ (one) 1)"
let e3 = "
(define (one) 
  (let ([a 1]
        [b 2]
        [c 3])
    (if (< a b)
      (let ([d 4]
            [e 5])
        (+ c (+ d e)))
      (let ([f 6])
        (+ a (+ c f))))))
(+ (one) 1)"
let e4 = "(define (f) (if 1 2 3)) (f)"
let e6 = "
(define (double x) (+ x x))
(double 2)"
let e7 = "
(define (double x) (+ x x))
(define (manyArgs x1 x2 x3 x4 x5 x6)
  (+ (double x1) x2 x3 x4 (double x5) x6))
(manyArgs 1 2 3 4 5 6)"
let e8 = "
(define (double x) (+ x x))
(define (twice f x) (f (f x)))
(twice double -1)"
let e9 = "
(define (helper acc n m)
  (if (< n m)
    (helper (+ acc (+ n 1)) (+ n 1) m)
    acc))
(define (sum n m)
  (helper 0 n m))
(sum 0 10000)"
let e10 = "
(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))
(tak 1 2 3)"
let e11 = "
(define (polyn x)
 (+ (- 2 (- x x))
    (+ (- 3 x)
       4)))
(polyn 11)"
let e12 = "
(define (foo vec1 vec2)
  (+ (vector-length vec1) (vector-length vec2)))
(foo (make-vector 3) (make-vector 100))"
let e13 = "
(define (foo vec)
  (let ([v (make-vector 2)])
    (vector-set! vec 0 v)
    (vector-set! v 0 11)
    (vector-set! v 1 22)))
(let ([v (make-vector 1)])
  (foo v)
  (+ (vector-ref (vector-ref v 0) 0)
     (vector-ref (vector-ref v 0) 1)))"
let e14 = "(let ([v (make-vector 3)])
(vector-set! v 0 1)
(vector-set! v 1 2)
(vector-set! v 2 3)
v)"
let e15 = "
(define (helper vec i n m)
(if (< i n)
  (begin (vector-set! vec 0 (make-vector m))
    (helper vec (+ i 1) n m))
  vec))

(define (lotsvectors n m)
(let ([v (make-vector n)])
(helper v 0 n m)))

(vector-length (lotsvectors 1000 1000))"
let e16 = "
(define (foo x)
  (lambda (y) (+ x y)))
((foo 1) 2)"

let tests = [
//     "(* 1 0)", "0\n"
//     "(* 1 5)", "5\n"
//     "(* -1 5)", "-5\n"
//     "(* 2 2)", "4\n"
    "#t", "#t\n"
//     "#f", "#f\n"
//     "1", "1\n"
//     "-1", "-1\n"
//     "(+ 1 2)", "3\n"
//     "(+ 1 (+ 2 3))", "6\n"
//     "(+ (+ 1 4) (+ 2 3))", "10\n"
//     "(< 1 2)", "#t\n"
//     "(if 1 2 3)", "2\n"
//     "(if #f 2 3)", "3\n"
//     "(if (< 3 1) 2 3)", "3\n"
//     "(if (if 1 2 3) 4 5)", "4\n"
//     "(if 4 (if #f 2 3) 5)", "3\n"
//     "(if 4 (if #t 8 9) (if #f 2 3))", "8\n"    
//     "
// (let ([v 1])
// (let ([w 46])
// (let ([x (+ v 7)])
// (let ([y (+ 4 x)])
// (let ([z (+ x w)])
// (+ z (- y)))))))", "42\n"
//     "
// (let ([a 1]
//       [b 2]
//       [c 3])
//   (if (< a b)
//     (let ([d 4]
//           [e 5])
//       (+ c (+ d e)))
//     (let ([f 6])
//       (+ a (+ c f)))))", "12\n"
    // e2, "2\n"
//     e3, "13\n"
//     e4, "2\n"
//     e6, "4\n"
//     e7, "27\n"
//     e8, "-4\n"
//     e9, "50005000\n"
//     e10, "3\n"
//     e11, "-2\n"
]
let vectorTests = [
    "(vector-length (make-vector 4))", "4\n"
    e12, "103\n"
    "(vector? (make-vector 3))", "#t\n"
    "(vector? #t)", "#f\n"
    e13, "33\n"
    e14, "#(1 2 3)\n"
]
let lambdaTests = [
    "(procedure? (lambda (x) x))", "#t\n"
    "(procedure? ((lambda ()
                  (lambda () 1))))", "#t\n"
    @"(let ((f (lambda () 12))) (f))", "12\n"
    @"(let ((f (lambda () (+ 12 13)))) (f))", "25\n"
    @"(let ((f (lambda () 13))) (+ (f) (f)))", "26\n"
    @"(let ((f (lambda () (let ((g (lambda () (+ 2 3)))) (* (g) (g)))))) (+ (f) (f)))", "50\n"
    @"(let ((f (lambda () (let ((f (lambda () (+ 2 3)))) (* (f) (f)))))) (+ (f) (f)))", "50\n"
    @"(let ((f (if (vector? (lambda () 12)) (lambda () 13) (lambda () 14)))) (f))", "14\n"
]
let assignmentTests = [
    "(let ((x 0)) (set! x 1) x)", "1\n"
]

[<EntryPoint>]
let main argv =
    // runTestsWithName testMainTest "basic" tests
    // runTestsWithName testMainTest "vector" vectorTests
    // runTestsWithName testMainTest "lambda" lambdaTests
    // runTestsWithName testMainTest "assignment" assignmentTests
    testLambda e2
    1