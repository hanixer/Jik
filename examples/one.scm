(let ([f (lambda (x y z) (+ x (* y z)))])
        (+ (apply f 12 '(7 2)) 1))