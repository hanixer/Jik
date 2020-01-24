
(define vector
    (lambda args
        (let ([len (length args)])
          (let ([v (make-vector len)])
            (let loop ([i 0] [args args])
              (if (>= i len)
                v
                (begin
                  (vector-set! v i (car args))
                  (loop (+ i 1) (cdr args)))))))))