#load "../TestDriver.fs"
open TestDriver


addTests "pairs" [
    "(pair? (cons 1 2))", "#t\n"
    "(pair? #t)", "#f\n"
    "(cons 1 2)", "(1 . 2)\n"
    "(cons 1 ())", "(1)\n"
    "(cons 1 (cons 2 3))", "(1 2 . 3)\n"
    "(cons 1 (cons (cons 2 ()) (cons 3 ())))", "(1 (2) 3)\n"
    "(car (cons 1 2))", "1\n"
    "(cdr (cons 1 2))", "2\n"
    "(cdr (car (cdr (cons 1 (cons (cons 2 ()) (cons 3 ()))))))", "()\n"
]
