﻿// // Learn more about F# at http://fsharp.org
module Program
// open System

// open System.IO

// open Core
// open Graph
// open TestDriver
// open RuntimeConstants
// open Display
// open Intermediate
// open Codegen

// let test s =
//     stringToExpr s
//     |> convertMainExprs
//     |> labelsToString
//     |> printfn "%s"

// // let testInterf s =
// //     let labels = stringToExpr s|> convertMainExprs
// //     let live = computeLiveAfter labels
// //     let graph = buildInterference labels live
// //     let allocated = allocateRegisters (selectInstructions labels, graph)
// //     printfn "labels:\n%s" (labelsToString labels)
// //     printfn "live: %A" live
// //     printfn "allocated:\n%s" (instrsToString allocated)
// //     printDot graph (__SOURCE_DIRECTORY__ + "../../../misc/out3.dot")

// let saveToFile filename str =
//     System.IO.File.WriteAllText(filename, str)

// let path = __SOURCE_DIRECTORY__ + "\\..\\..\\misc\\test.ir"

// let printIr = (fun prog -> prog |> Intermediate.programToString |> saveToFile (path); prog)

// let testInterf s =
//   let ds, m =
//     s 
//     |> stringToProgram
//     |> fixPrims
//     |> alphaRename
//     |> revealFunctions 
//     |> convertProgram
//     |> printIr
//     |> selectInstructions
//     |> computeLiveness
//     |> buildInterference
//   use out = new StreamWriter(path)
//   let handleDef def =
//     Seq.iteri (fun i instr ->
//       fprintf out "%d) " i
//       showInstr out instr
//       fprintfn out "") def.Instrs
//     Map.iter (fun k v -> 
//       fprintf out "%d) " k
//       Seq.iter (function
//         | Var var -> fprintf out "%s, " var
//         | Reg reg -> fprintf out "%%%s, " (reg.ToString().ToLower())
//         | _ -> ()) v
//       fprintfn out "") def.LiveAfter
//     fprintfn out "\n"
//   List.iter handleDef ds
//   handleDef m

// let testMainTest s =
//   s 
//   |> stringToProgram
//   |> fixPrims
//   |> alphaRename
//   |> revealFunctions 
//   |> convertProgram
//   |> printIr
//   |> selectInstructions
//   |> computeLiveness
//   |> buildInterference
//   |> allocateRegisters
//   |> addFunctionBeginEnd
//   |> patchInstr
//   |> Codegen.programToString

// let testNew s =
//     s 
//     |> stringToProgram
//     |> fixPrims
//     |> alphaRename
//     |> revealFunctions 
//     |> convertProgram
//     |> printIr
//     |> selectInstructions
//     |> Codegen.programToString
//     |> printfn "%s"
  
// let e ="
// (let ([a 1]
//       [b 2]
//       [c 3])
//   (if (< a b)
//     (let ([d 4]
//           [e 5])
//       (+ c (+ d e)))
//     (let ([f 6])
//       (+ a (+ c f)))))"
// let e2 = "(define (one) 1)
// (+ (one) 1)"
// let e3 = "
// (define (one) 
//   (let ([a 1]
//         [b 2]
//         [c 3])
//     (if (< a b)
//       (let ([d 4]
//             [e 5])
//         (+ c (+ d e)))
//       (let ([f 6])
//         (+ a (+ c f))))))
// (+ (one) 1)"
// let e4 = "(define (f) (if 1 2 3)) (f)"
// let e6 = "
// (define (double x) (+ x x))
// (double 2)"
// let e7 = "
// (define (double x) (+ x x))
// (define (manyArgs x1 x2 x3 x4 x5 x6)
//   (+ (double x1) x2 x3 x4 (double x5) x6))
// (manyArgs 1 2 3 4 5 6)"
// let e8 = "
// (define (double x) (+ x x))
// (define (twice f x) (f (f x)))
// (twice double -1)"
// let e9 = "
// (define (sum n m)
//   (if (< n m)
//     (+ n (sum (+ n 1) m))
//     n))
// (sum 0 1000)"
// let e10 = "
// (define (tak x y z)
//   (if (not (< y x))
//       z
//       (tak (tak (- x 1) y z)
//            (tak (- y 1) z x)
//            (tak (- z 1) x y))))
// (tak 1 2 3)"
// let e11 = "
// (define (polyn x)
//  (+ (* 2 (* x x))
//     (+ (* 3 x)
//        4)))
// (polyn 11)"
// let tests = [
// //     "#t", "#t\n"
// //     "#f", "#f\n"
// //     "1", "1\n"
// //     "-1", "-1\n"
// //     "(+ 1 2)", "3\n"
// //     "(+ 1 (+ 2 3))", "6\n"
// //     "(+ (+ 1 4) (+ 2 3))", "10\n"
// //     "(< 1 2)", "#t\n"
// //     "(if 1 2 3)", "2\n"
// //     "(if #f 2 3)", "3\n"
// //     "(if (< 3 1) 2 3)", "3\n"
// //     "(if (if 1 2 3) 4 5)", "4\n"
// //     "(if 4 (if #f 2 3) 5)", "3\n"
// //     "(if 4 (if #t 8 9) (if #f 2 3))", "8\n"    
// //     "
// // (let ([v 1])
// // (let ([w 46])
// // (let ([x (+ v 7)])
// // (let ([y (+ 4 x)])
// // (let ([z (+ x w)])
// // (+ z (- y)))))))", "42\n"
// //     "
// // (let ([a 1]
// //       [b 2]
// //       [c 3])
// //   (if (< a b)
// //     (let ([d 4]
// //           [e 5])
// //       (+ c (+ d e)))
// //     (let ([f 6])
// //       (+ a (+ c f)))))", "12\n"
//     // e2, "2\n"
//     // e3, "3\n"
//     // e4, "2\n"
//     e6, "4\n"
//     // e7, "27\n"
//     // e8, "-4\n"
//     // e9, "55\n"
//     // e10, "11\n"
// ]

// // testInterf e6



// [<EntryPoint>]
// let main argv =
//     runTestsWithName testMainTest "basic" tests |> printfn "Test run: %d"
//     0 // return an integer exit code