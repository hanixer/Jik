(define %all-symbols-list '())

(define string->symbol
    (lambda (str)
        (let loop ([ls %all-symbols-list])
            (if (empty? ls)
                (let ([created (make-symbol str)])
                  (set! %all-symbols-list (cons created %all-symbols-list))
                  created)
                (if (string=? (symbol-string (car ls)) str)
                    (car ls)
                    (loop (cdr ls)))))))