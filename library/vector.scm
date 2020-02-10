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

(define vector-swap!
     (lambda (v i1 i2)
          (let ([t (vector-ref v i1)])
            (vector-set! v i1 (vector-ref v i2))
            (vector-set! v i2 t))))

(define vector-shuffle!
     (lambda (v rng-seed)
          (let ([l (vector-length v)])
            (let loop ([i 0] [rand (rand-next rng-seed)])
                 (when (< i (- l 1))
                     (let ((j (+ i (abs (remainder rand (- l i))))))
                       (vector-swap! v i j)
                       (loop (+ i 1) (rand-next rand))))))))

(define (vector->list s)
  (let loop ((i 0))
    (if (>= i (vector-length s))
        '()
        (cons (vector-ref s i) (loop (+ i 1))))))

(define (list->vector l)
  (apply vector l))