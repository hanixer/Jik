(define %all-symbols-list '())

(define string->symbol
    (lambda (str)
        (unless (string? str)
            (error "string->symbol: not a string given!"))
        (let loop ([ls %all-symbols-list])
            (if (empty? ls)
                (let ([created (make-symbol str)])
                  (set! %all-symbols-list (cons created %all-symbols-list))
                  created)
                (let ([h (car ls)])
                    (unless (symbol? h)
                        (error "head is not a symbol" ls))
                    (if (string=? (symbol-string (car ls)) str)
                        (car ls)
                        (loop (cdr ls))))))))

(define symbol->string
    (lambda (s)
        (unless (symbol? s)
            (error "symbol->string: not a symbol"))
        (symbol-string s)))