module TestCases
open System.Text
open System.IO



// e should be 12
let e = "
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
let e17 = "
(define pair?)"
let e18 = "
(let ((s (make-string 2)))
   (string-set! s 0 #\\A)
   (string-set! s 1 #\\B)
   (foreign-call \"write\" 1 s 2)
   (foreign-call \"write\" 1 s 2)
   1)"
let e19 = "
(foreign-call \"print6args\" 1 2 3 4 5 6)
(foreign-call \"print6args\" 1 2 3 4 5 6)
#t"
let e20 = "
(define (sum x y) (+ x y))
(foreign-call \"apply\" sum (cons 1 (cons 2 '())))"
let apply = "
(let ((sum (lambda (x y) (+ x y))))
    (let ((args (cons 1 (cons 2 '()))))
        (apply sum args)))"

let basicTests =
    [ "(* 1 0)", "0\n"
      "(* 1 5)", "5\n"
      "(* -1 5)", "-5\n"
      "(* 2 2)", "4\n"
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
      e2, "2\n"
      e3, "13\n"
      e4, "2\n"
      e6, "4\n"
      e7, "27\n"
      e8, "-4\n"
      e11, "-2\n" ]

let booleanTests =
    [ @"(boolean? #t)", "#t\n"
      @"(boolean? #f)", "#t\n"
      @"(boolean? 0)", "#f\n"
      @"(boolean? 1)", "#f\n"
      @"(boolean? -1)", "#f\n"
      @"(boolean? '())", "#f\n"
      @"(boolean? #\a)", "#f\n"
      @"(boolean? (boolean? 0))", "#t\n"
      @"(boolean? (fixnum? (boolean? 0)))", "#t\n" ]

let vectorTests =
    [ "(vector-length (make-vector 4))", "4\n"
      e12, "103\n"
      "(vector? (make-vector 3))", "#t\n"
      "(vector? #t)", "#f\n"
      e13, "33\n"
      e14, "#(1 2 3)\n" ]

let assignmentTests =
    [ "(let ((x 0)) (set! x 1) x)", "1\n"
      @"(let ((x 12)) (set! x 13) x)", "13\n"
      @"(let ((x 12)) (set! x (+ 1  x)) x)", "13\n"
      @"(let ((x 12)) (let ((x #f)) (set! x 14)) x)", "12\n"
      @"(let ((x 12)) (let ((y (let ((x #f)) (set! x 14)))) x))", "12\n"
      @"(let ((f #f)) (let ((g (lambda () f))) (set! f 10) (g)))", "10\n"
      @"(let ((f (lambda (x) (set! x (+ 1  x)) x))) (f 12))", "13\n"
      @"(let ((x 10)) (let ((f (lambda (x) (set! x (+ 1  x)) x))) (cons x (f x))))", "(10 . 11)\n"
      @"(let ((t #f))
       (let ((locative (cons (lambda () t) (lambda (n) (set! t n)))))
        ((cdr locative) 17)
        ((car locative))))", "17\n"

      @"(let ((locative (let ((t #f)) (cons (lambda () t) (lambda (n) (set! t n)))))) ((cdr locative) 17) ((car locative)))",
      "17\n"
      @"(let ((make-counter (lambda ()
                           (let ((counter -1))
                            (lambda ()
                             (set! counter (+ 1  counter)) counter)))))
       (let ((c0 (make-counter))
             (c1 (make-counter)))
        (c0)
        (cons (c0) (c1))))", "(1 . 0)\n"
      @"(let ((fact #f)) (set! fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))) (fact 5))", "120\n"

      @"(let ((fact #f)) ((begin (set! fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))) fact) 5))",
      "120\n" ]

let andOrTests =
    [ @"(and)", "#t\n"
      @"(and 5)", "5\n"
      @"(and #f)", "#f\n"
      @"(and 5 6)", "6\n"
      @"(and #f ((lambda (x) (x x)) (lambda (x) (x x))))", "#f\n"
      @"(or)", "#f\n"
      @"(or #t)", "#t\n"
      @"(or 5)", "5\n"
      @"(or 1 2 3)", "1\n"
      @"(or (cons 1 2) ((lambda (x) (x x)) (lambda (x) (x x))))", "(1 . 2)\n"
      @"(let ((if 12)) (or if 17))", "12\n"
      @"(let ((if 12)) (and if 17))", "17\n"
      @"(let ((let 8)) (or let 18))", "8\n"
      @"(let ((let 8)) (and let 18))", "18\n"
      @"(let ((t 1)) (and (begin (set! t (+ 1 t)) t) t))", "2\n"
      @"(let ((t 1)) (or (begin (set! t (+ 1 t)) t) t))", "2\n" ]

let pairTests =
    [ "(pair? (cons 1 2))", "#t\n"
      "(pair? #t)", "#f\n"
      "(cons 1 2)", "(1 . 2)\n"
      "(cons 1 '())", "(1)\n"
      "(cons 1 (cons 2 3))", "(1 2 . 3)\n"
      "(cons 1 (cons (cons 2 '()) (cons 3 '())))", "(1 (2) 3)\n"
      "(car (cons 1 2))", "1\n"
      "(cdr (cons 1 2))", "2\n"
      "(cdr (car (cdr (cons 1 (cons (cons 2 '()) (cons 3 '()))))))", "()\n" ]

let setCarCdrTests =
    [ @"(let ((x (cons 1 2))) (begin (set-cdr! x '()) x))", "(1)\n"
      @"(let ((x (cons 1 2))) (set-cdr! x '()) x)", "(1)\n"
      @"(let ((x (cons 12 13)) (y (cons 14 15))) (set-cdr! x y) x)", "(12 14 . 15)\n"
      @"(let ((x (cons 12 13)) (y (cons 14 15))) (set-cdr! y x) y)", "(14 12 . 13)\n"
      @"(let ((x (cons 12 13)) (y (cons 14 15))) (set-cdr! y x) x)", "(12 . 13)\n"
      @"(let ((x (cons 12 13)) (y (cons 14 15))) (set-cdr! x y) y)", "(14 . 15)\n"
      @"(let ((x (let ((x (cons 1 2))) (set-car! x #t) (set-cdr! x #f) x))) (cons x x) x)", "(#t . #f)\n"
      @"(let ((x (cons 1 2)))
       (set-cdr! x x)
       (set-car! (cdr x) x)
       (cons (eq? x (car x)) (eq? x (cdr x))))", "(#t . #t)\n"
      @"(let ((x #f)) (if (pair? x) (set-car! x 12) #f) x)", "#f\n" ]

let whenUnlessTests =
    [ @"(let ((x (cons 1 2)))
       (when (pair? x)
        (set-car! x (+ (car x) (cdr x))))
        x)", "(3 . 2)\n"

      @"(let ((x (cons 1 2))) (when (pair? x) (set-car! x (+ (car x) (cdr x))) (set-car! x (+ (car x) (cdr x)))) x)",
      "(5 . 2)\n"
      @"(let ((x (cons 1 2)))
       (unless (fixnum? x)
        (set-car! x (+ (car x) (cdr x))))
       x)", "(3 . 2)\n"

      @"(let ((x (cons 1 2))) (unless (fixnum? x) (set-car! x (+ (car x) (cdr x))) (set-car! x (+ (car x) (cdr x)))) x)",
      "(5 . 2)\n"
      @"(let ((let 12)) (when let let let let let))", "12\n"
      @"(let ((let #f)) (unless let let let let let))", "#f\n" ]

let condTests =
    [ @"(cond (1 2) (else 3))", "2\n"
      @"(cond (1) (else 13))", "1\n"
      @"(cond (#f #t) (#t #f))", "#f\n"
      @"(cond (else 17))", "17\n"
      @"(cond (#f) (#f 12) (12 13))", "13\n"
      @"(cond ((cons 1 2) => (lambda (x) (cdr x))))", "2\n"
      @"(let ((else #t)) (cond (else 1287)))", "1287\n"
      @"(let ((else 17)) (cond (else)))", "17\n"
      @"(let ((else 17)) (cond (else => (lambda (x) x))))", "17\n"
      @"(let ((else #f)) (cond (else ((lambda (x) (x x)) (lambda (x) (x x))))) else)", "#f\n"
      @"(let ((=> 12)) (cond (12 => 14) (else 17)))", "14\n"
      @"(let ((=> 12)) (cond (=>)))", "12\n"
      @"(let ((=> 12)) (cond (=> =>)))", "12\n"
      @"(let ((=> 12)) (cond (=> => =>)))", "12\n"
      @"(let ((let 12)) (cond (let => (lambda (x) (+ let x))) (else 14)))", "24\n" ]

let lambdaTests =
    [ "(procedure? (lambda (x) x))", "#t\n"
      "(procedure? ((lambda ()
                  (lambda () 1))))", "#t\n"
      @"(let ((f (lambda () 12))) (f))", "12\n"
      @"(let ((f (lambda () (+ 12 13)))) (f))", "25\n"
      @"(let ((f (lambda () 13))) (+ (f) (f)))", "26\n"
      @"(let ((f (lambda () (let ((g (lambda () (+ 2 3)))) (* (g) (g)))))) (+ (f) (f)))", "50\n"
      @"(let ((f (lambda () (let ((f (lambda () (+ 2 3)))) (* (f) (f)))))) (+ (f) (f)))", "50\n"
      @"(let ((f (if (vector? (lambda () 12)) (lambda () 13) (lambda () 14)))) (f))", "14\n"
      @"(let ((f (lambda (x) x))) (f 12))", "12\n"
      @"(let ((f (lambda (x y) (+ x y)))) (f 12 13))", "25\n"
      @"(let ((f (lambda (x) (let ((g (lambda (x y) (+ x y)))) (g x 100))))) (f 1000))", "1100\n"
      @"(let ((f (lambda (g) (g 2 13)))) (f (lambda (n m) (* n m))))", "26\n"
      @"(let ((f (lambda (g) (+ (g 10) (g 100))))) (f (lambda (x) (* x x))))", "10100\n"
      @"(let ((f (lambda (f n m) (if (zero? n) m (f f (- n 1) (* n m)))))) (f f 5 1))", "120\n"
      @"(let ((f (lambda (f n) (if (zero? n) 1 (* n (f f (- n 1))))))) (f f 5))", "120\n"
      @"(let ((n 12)) (let ((f (lambda () n))) (f)))", "12\n"
      @"(let ((n 12)) (let ((f (lambda (m) (+ n m)))) (f 100)))", "112\n" ]

// " (let ((cond +)) (cond (+ 1) (- 2)))", "1\n"
let letrecTests =
    [ @"(letrec () 12)", "12\n"
      @"(letrec ((f 12)) f)", "12\n"
      @"(letrec ((f 12) (g 13)) (+ f g))", "25\n"
      @"(letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))) (fact 5))", "120\n"
      @"(letrec ((f 12) (g (lambda () f))) (g))", "12\n"
      @"(letrec ((f 12) (g (lambda (n) (set! f n)))) (g 130) f)", "130\n"
      @"(letrec ((f (lambda (g) (set! f g) (f)))) (f (lambda () 12)))", "12\n"

      @"(letrec ((f (cons (lambda () f) (lambda (x) (set! f x))))) (let ((g (car f))) ((cdr f) 100) (g)))",
      "100\n"
      @"(letrec ((f (letrec ((g (lambda (x) (* x 2)))) (lambda (n) (g (* n 2)))))) (f 12))", "48\n"
      @"(letrec ((f (lambda (f n) (if (zero? n) 1 (* n (f f (- n 1))))))) (f f 5))", "120\n"
      @"(let ((f (lambda (f)
                (lambda (n)
                 (if (zero? n)
                  1
                  (* n (f (- n 1))))))))
       (letrec ((fix (lambda (f)
                      (f (lambda (n)
                          ((fix f) n))))))
        ((fix f) 5)))", "120\n" ]

let deeplyProcedureTests =
    [ @"(letrec ((sum (lambda (n ac)
                  (if (fxzero? n)
                      ac
                      (sum (- n 1) (fx+ n ac))))))
          (sum 10000 0))", "50005000\n"
      @"(letrec ((e (lambda (x) (if (fxzero? x) #t (o (- x 1))))) (o (lambda (x) (if (fxzero? x) #f (e (- x 1)))))) (e 5000000))", "#t\n" ]

let listTests =
    [ "(define length
       (lambda (l)
         (if (null? l)
           0
           (+ 1 (length (cdr l))))))
     (length '())", "0\n"
      "(let ((f (lambda () (null? 1)))) (f))", "#f\n" ]

let foreignCallTests =
    [ e18, "ABAB1\n"
      e19, "123456123456#t\n" ]

let numcharTests =
    [ @"(number->char 65)", "#\\A\n"
      @"(number->char 97)", "#\\a\n"
      @"(number->char 122)", "#\\z\n"
      @"(number->char 90)", "#\\Z\n"
      @"(number->char 48)", "#\\0\n"
      @"(number->char 57)", "#\\9\n"
      @"(char->number #\A)", "65\n"
      @"(char->number #\a)", "97\n"
      @"(char->number #\z)", "122\n"
      @"(char->number #\Z)", "90\n"
      @"(char->number #\0)", "48\n"
      @"(char->number #\9)", "57\n"
      @"(char->number (number->char 12))", "12\n"
      @"(number->char (char->number #\x))", "#\\x\n" ]

let isCharTests =
    [ @"(char? #\a)", "#t\n"
      @"(char? #\Z)", "#t\n"
      @"(char? #\newline)", "#t\n"
      @"(char? #t)", "#f\n"
      @"(char? #f)", "#f\n"
      @"(char? '())", "#f\n"
      @"(char? (char? #t))", "#f\n"
      @"(char? 0)", "#f\n"
      @"(char? 23870)", "#f\n"
      @"(char? -23789)", "#f\n" ]

let stringTests =
    [ @"(string? (make-string 0))", "#t\n"
      @"(make-string 0)", "\"\"\n"
      @"(let ((s (make-string 1)))
          (string-set! s 0 #\a)
          (string-ref s 0))", "#\\a\n"
      @"(let ((s (make-string 2)))
          (string-set! s 0 #\a)
          (string-set! s 1 #\b)
          (cons (string-ref s 0)
                (string-ref s 1)))", "(#\\a . #\\b)\n"
      @"(let ((i 0)) (let ((s (make-string 1))) (string-set! s i #\a) (string-ref s i)))", "#\\a\n"
      @"(let ((i 0) (j 1)) (let ((s (make-string 2))) (string-set! s i #\a) (string-set! s j #\b) (cons (string-ref s i) (string-ref s j))))", "(#\\a . #\\b)\n"
      @"(let ((i 0) (c #\a)) (let ((s (make-string 1))) (string-set! s i c) (string-ref s i)))", "#\\a\n"
      @"(string-length (make-string 12))", "12\n"
      @"(string? (make-vector 12))", "#f\n"
      @"(string? (cons 1 2))", "#f\n"
      @"(string? 1287)", "#f\n"
      @"(string? '())", "#f\n"
      @"(string? #t)", "#f\n"
      @"(string? #f)", "#f\n"
      @"(pair? (make-string 12))", "#f\n"
      @"(null? (make-string 12))", "#f\n"
      @"(boolean? (make-string 12))", "#f\n"
      @"(vector? (make-string 12))", "#f\n"
      @"(make-string 0)", "\"\"\n"
      @"(let ((v (make-string 2))) (string-set! v 0 #\t) (string-set! v 1 #\f) v)", "\"tf\"\n"
      @"(let ((v (make-string 2)))
          (string-set! v 0 #\x)
          (string-set! v 1 #\x)
          (eq? (string-ref v 0) (string-ref v 1)))", "#t\n"
      @"(let ((v0 (make-string 3))) (let ((v1 (make-string 3))) (string-set! v0 0 #\a) (string-set! v0 1 #\b) (string-set! v0 2 #\c) (string-set! v1 0 #\d) (string-set! v1 1 #\e) (string-set! v1 2 #\f) (cons v0 v1)))", "(\"abc\" . \"def\")\n"
      @"(let ((n 2)) (let ((v0 (make-string n))) (let ((v1 (make-string n))) (string-set! v0 0 #\a) (string-set! v0 1 #\b) (string-set! v1 0 #\c) (string-set! v1 1 #\d) (cons v0 v1))))", "(\"ab\" . \"cd\")\n"
      @"(let ((n 3)) (let ((v0 (make-string n))) (let ((v1 (make-string (string-length v0)))) (string-set! v0 (fx- (string-length v0) 3) #\a) (string-set! v0 (fx- (string-length v1) 2) #\b) (string-set! v0 (fx- (string-length v0) 1) #\c) (string-set! v1 (fx- (string-length v1) 3) #\Z) (string-set! v1 (fx- (string-length v0) 2) #\Y) (string-set! v1 (fx- (string-length v1) 1) #\X) (cons v0 v1))))", "(\"abc\" . \"ZYX\")\n"
      @"(let ((n 1)) (string-set! (make-string n) (- n 1) (fixnum->char 34)) n)", "1\n"
      @"(let ((n 1)) (let ((v (make-string 1))) (string-set! v (- n 1) (fixnum->char n)) (char->fixnum (string-ref v (- n 1)))))", "1\n"
      @"(let ((v0 (make-string 1)))
          (string-set! v0 0 #\a)
          (let ((v1 (make-string 1)))
            (string-set! v1 0 #\A)
            (string-set!
              (if (string? v0) v0 v1)
              (- (string-length (if (string? v0) v0 v1)) 1)
              (fixnum->char
                (+ (char->fixnum (string-ref (if (string? v0) v0 v1)
                                             (- (string-length (if (string? v0) v0 v1)) 1)))
                   1)))
            (cons v0 v1)))", "(\"b\" . \"A\")\n"
      "(let ((s (make-string 1))) (string-set! s 0 #\\\") s)", "\"\"\"\n"
      "(let ((s (make-string 1))) (string-set! s 0 #\\\\) s)", "\"\\\"\n" ]

let callFailure =
    [ "(1 2)", "error\n"
      "(#t 2)", "error\n"
      "((if #f (lambda (x) x) #t) 2)", "error\n"
      "
(let ((f (lambda (x)
            (if (number? x)
                (lambda (y) y)
                #f))))
  ((f #t) #f))", "error\n"
      "
(let ((f (lambda (y) y)))
  (f))", "error\n"
      "
(let ((f (lambda (y) y)))
  (f 1 2))", "error\n" ]

let variableArity =
  [ "(let ([f (lambda args 12)])
      (f))", "12\n"
    "(let ([f (lambda args 12)])
      (f 10))", "12\n"
    "(let ([f (lambda args 12)])
      (f 10 20))", "12\n"
    "(let ([f (lambda args 12)])
      (f 10 20 30))", "12\n"
    "(let ([f (lambda args 12)])
      (f 10 20 30 40))", "12\n"
    "(let ([f (lambda args 12)])
      (f 10 20 30 40 50))", "12\n"
    "(let ([f (lambda args 12)])
      (f 10 20 30 40 50 60 70 80 90))", "12\n"
    "(let ([f (lambda (a0 . args) 12)])
      (f 10))", "12\n"
    "(let ([f (lambda (a0 . args) a0)])
      (f 10))", "10\n"
    "(let ([f (lambda (a0 . args) 12)])
      (f 10 20))", "12\n"
    "(let ([f (lambda (a0 . args) a0)])
      (f 10 20))", "10\n"
    "(let ([f (lambda (a0 a1 . args) (cons a0 a1))])
      (f 10 20 30 40 50 60 70 80 90 100))", "(10 . 20)\n"
    "(let ([f (lambda (a0 a1 a3 a4 . args) (cons a0 (cons a1 (cons a3 (cons a4 '())))))])
      (f 10 20 30 40 50 60 70 80 90 100))", "(10 20 30 40)\n" ]

let variableArityUsingRest =
  [ "(let ([f (lambda args args)])
      (f))", "()\n"
    "(let ([f (lambda args args)])
      (f 10))", "(10)\n"
    "(let ([f (lambda args args)])
      (f 10 20))", "(10 20)\n"
    "(let ([f (lambda args args)])
      (f 10 20 30 40 50 60 70 80 90))", "(10 20 30 40 50 60 70 80 90)\n"
    "(let ([f (lambda (a0 . args) 12)])
      (f 10))", "12\n"
    "(let ([f (lambda (a0 . args) a0)])
      (f 10))", "10\n"
    "(let ([f (lambda (a0 . args) (cons a0 args))])
      (f 10 20))", "(10 20)\n"
    "(let ([f (lambda (a0 a1 . args) (cons a0 (cons a1 args)))])
      (f 10 20 30 40 50 60 70 80 90 100))", "(10 20 30 40 50 60 70 80 90 100)\n"  ]

let tak =
    [ "(define (tak x y z)
       (if (not (< y x))
         z
         (tak (tak (- x 1) y z)
              (tak (- y 1) z x)
              (tak (- z 1) x y))))
     (tak 32 16 8)", "9\n";
      "(cons 1 2)", "3\n" ]

let generateBigLet n =
    use f = new StringWriter()
    fprintfn f "(let ("
    for i in 0..n do
        fprintfn f "  (x%d %d)" i i
    fprintfn f "  )"
    fprintfn f "(+ x1 x3))"
    f.ToString()