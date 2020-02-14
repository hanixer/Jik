(define error
    (lambda args
        (foreign-call "s_error" args)))

(define exit-scheme
    (lambda ()
        (foreign-call "s_exit" 0)))

(define random-flonum
    (lambda ()
        (foreign-call "s_randomFlonum")))