(define list
    (lambda args args))

(define list?
    (letrec ([loop
            (lambda (h t)
            (if (pair? h)
                (let ([h (cdr h)])
                    (if (pair? h)
                        (and (not (eq? h t))
                            (loop (cdr h) (cdr t)))
                        (null? h)))
                (null? h)))])
    (lambda (x) (loop x x))))