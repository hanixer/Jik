#load "../TestDriver.fs"
open TestDriver


addTests "letrec" [
    @"(letrec () 12)", "12\n"
    @"(letrec ((f 12)) f)", "12\n"
    @"(letrec ((f 12) (g 13)) (+ f g))", "25\n"
    @"(letrec ((fact (lambda (n) (if (zero n) 1 (* n (fact (- n 1))))))) (fact 5))", "120\n"
    @"(letrec ((f 12) (g (lambda () f))) (g))", "12\n"
    @"(letrec ((f 12) (g (lambda (n) (set! f n)))) (g 130) f)", "130\n"
    @"(letrec ((f (lambda (g) (set! f g) (f)))) (f (lambda () 12)))", "12\n"
    @"(letrec ((f (cons (lambda () f) (lambda (x) (set! f x))))) (let ((g (car f))) ((cdr f) 100) (g)))", "100\n"
    @"(letrec ((f (letrec ((g (lambda (x) (* x 2)))) (lambda (n) (g (* n 2)))))) (f 12))", "48\n"
    @"(letrec ((f (lambda (f n) (if (zero n) 1 (* n (f f (- n 1))))))) (f f 5))", "120\n"
    @"(let ((f (lambda (f) (lambda (n) (if (zero n) 1 (* n (f (- n 1)))))))) (letrec ((fix (lambda (f) (f (lambda (n) ((fix f) n)))))) ((fix f) 5)))", "120\n"
]

addTests "letrec*" [
    @"(letrec* () 12)", "12\n"
    @"(letrec* ((f 12)) f)", "12\n"
    @"(letrec* ((f 12) (g 13)) (+ f g))", "25\n"
    @"(letrec* ((fact (lambda (n) (if (zero n) 1 (* n (fact (- n 1))))))) (fact 5))", "120\n"
    @"(letrec* ((f 12) (g (lambda () f))) (g))", "12\n"
    @"(letrec* ((f 12) (g (lambda (n) (set! f n)))) (g 130) f)", "130\n"
    @"(letrec* ((f (lambda (g) (set! f g) (f)))) (f (lambda () 12)))", "12\n"
    @"(letrec* ((f (cons (lambda () f) (lambda (x) (set! f x))))) (let ((g (car f))) ((cdr f) 100) (g)))", "100\n"
    @"(letrec* ((f (letrec* ((g (lambda (x) (* x 2)))) (lambda (n) (g (* n 2)))))) (f 12))", "48\n"
    @"(letrec* ((f (lambda (f n) (if (zero n) 1 (* n (f f (- n 1))))))) (f f 5))", "120\n"
    @"(let ((f (lambda (f) (lambda (n) (if (zero n) 1 (* n (f (- n 1)))))))) (letrec* ((fix (lambda (f) (f (lambda (n) ((fix f) n)))))) ((fix f) 5)))", "120\n"
    @"(letrec* ((a 12) (b (+ a 5)) (c (+ b a))) c)", "29\n"
]

addTests "and/or" [
    @"(and)", "#t\n"
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
    @"(let ((t 1)) (or (begin (set! t (+ 1 t)) t) t))", "2\n"
]

addTests "when/unless" [
    @"(let ((x (cons 1 2))) (when (pair? x) (set-car! x (+ (car x) (cdr x)))) x)", "(3 . 2)\n"
    @"(let ((x (cons 1 2))) (when (pair? x) (set-car! x (+ (car x) (cdr x))) (set-car! x (+ (car x) (cdr x)))) x)", "(5 . 2)\n"
    @"(let ((x (cons 1 2))) (unless (fixnum? x) (set-car! x (+ (car x) (cdr x)))) x)", "(3 . 2)\n"
    @"(let ((x (cons 1 2))) (unless (fixnum? x) (set-car! x (+ (car x) (cdr x))) (set-car! x (+ (car x) (cdr x)))) x)", "(5 . 2)\n"
    @"(let ((let 12)) (when let let let let let))", "12\n"
    @"(let ((let #f)) (unless let let let let let))", "#f\n"
]

addTests "cond" [
    @"(cond (1 2) (else 3))", "2\n"
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
    @"(let ((let 12)) (cond (let => (lambda (x) (+ let x))) (else 14)))", "24\n"
]
