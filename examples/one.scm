(define (f x y) (+ (vector-length x) (vector-length y)))

(let ([v (make-vector 3)])
    (let ([d (f v v)])
        (+ d 1)))