
(define abs
    (lambda (x)
        (if (> x 0)
            x
            (- 0 x))))

(define (number->string n)
  (define (iter n acc)
    (if (= n 0)
        (if (null? acc)
            (cons #\0 '())
            acc)
        (iter (quotient n 10)
              (cons (fixnum->char
                        (+ (char->fixnum #\0)
                           (remainder n 10)))
                     acc))))

    (if (< n 0)
        (list->string (cons #\- (iter (- 0 n) '())))
        (list->string (iter n '()))))

(define (expt x y)
    (foreign-call "s_expt" x y))

(define (sqrt x)
    (expt x 0.5))