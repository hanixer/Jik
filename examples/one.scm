(do ([i 0 (+ i 1)] [res '() (cons (random-flonum) res)])
    ((= i 10) res)
)