(define %strings2=?
    (lambda (s1 s2)
        (let ([len (string-length s1)])
        (if (not (eq? len (string-length s2)))
                        #f
                        (let loop ([i 0])
                            (if (< i len)
                                (if (eq? (string-ref s1 i) (string-ref s2 i))
                                    (loop (+ i 1))
                                    #f)
                                #t))))))

(define %strings=?
    (lambda (s s*)
      (if (string? s)
            (if (empty? s*)
                #t
                (let ([s2 (car s*)] [s* (cdr s*)])
                    (if (%strings2=? s s2)
                        (%strings=? s2 s*)
                        #f)))
            (error "string=? - not a string"))))

(define string=?
    (lambda (s . s*)
      (%strings=? s s*)))