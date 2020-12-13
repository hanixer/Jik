// Learn more about F# at http://fsharp.org

open System


let src1 = "
(flonum->fixnum (fl* 255.59 (vec-z col)))
"

let src2 = "
(define (make-samples world out nx ny ns camera i j)
  (let loop ([s 0] [col (vec 0.0 0.0 0.0)])
    (if (< s ns)
      (let* ([u (fl/ (fl+ (fixnum->flonum i) (random-flonum)) (fixnum->flonum nx))]
             [v (fl/ (fl+ (fixnum->flonum j) (random-flonum)) (fixnum->flonum ny))]
             [r (generate-ray camera u v)]
             [col2 (color r world 0)])
        (loop (+ s 1) (vec-add col col2)))
      (let* ([col (vec-s/ col (fixnum->flonum ns))]
             [ir (flonum->fixnum (fl* 255.59 (vec-x col)))]
             [ig (flonum->fixnum (fl* 255.59 (vec-y col)))]
             [ib (flonum->fixnum (fl* 255.59 (vec-z col)))])
        (display ir out)
        (display \" \" out)
        (display ig out)
        (display \" \" out)
        (display ib out)
        (newline out)))))
"

let src3 = "
(define (abs x)
  (if (< x 0) 
    (- 0 x) 
    x))
"

[<EntryPoint>]
let main argv =
    let str =
        match argv with
        | [|file|] ->
            System.IO.File.ReadAllText file
        | _ -> src3
    let res = SExpr.tokenize str
    use f = System.IO.File.CreateText("misc/tokens.txt")
    for t in res do
        fprintf f "%A, " t
    printfn "====================="
    
    let sexpr = SExpr.parse res    
    printfn "%s" (SExpr.sexprToCompactString sexpr)

    0 // return an integer exit code
