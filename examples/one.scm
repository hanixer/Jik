(define (%string-append s1 s2)
    (let* ([l1 (string-length s1)] [l2 (string-length s2)] [s (make-string (+ l1 l2))])
        (let loop ([i 0])
            (cond
                ((< i l1)
                    (begin
                        (string-set! s i (string-ref s1 i))
                        (loop (+ i 1))))
                ((< i (+ l1 l2))
                    (begin
                        (string-set! s i (string-ref s2 (- i l1)))
                        (loop (+ i 1))))
                (else s)))))

(define (string-append . ss)
    (if (null? ss)
        (make-string 0)
        (let loop ([ss (cdr ss)] [acc (car ss)])
            (if (null? ss)
                acc
                (loop (cdr ss) (%string-append acc (car ss)))))))

(string-append "one" "," "two...")

