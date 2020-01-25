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


  (define ($string<? s1 s2)
    (let ([n1 (string-length s1)]
          [n2 (string-length s2)])
      (if (< n1 n2)
          (let f ([i 0] [n n1] [s1 s1] [s2 s2])
            (if (= i n)
                #t
                (let ([c1 (string-ref s1 i)]
                      [c2 (string-ref s2 i)])
                  (if (< c1 c2)
                      #t
                      (if (= c1 c2)
                          (f (+ i 1) n s1 s2)
                          #f)))))
          (let f ([i 0] [n n2] [s1 s1] [s2 s2])
            (if (= i n)
                #f
                (let ([c1 (string-ref s1 i)]
                      [c2 (string-ref s2 i)])
                  (if (< c1 c2)
                      #t
                      (if (= c1 c2)
                          (f (+ i 1) n s1 s2)
                          #f))))))))

(define string<? $string<?)

  (define list->string
    (letrec ([fill
               (lambda (s i ls)
                 (cond
                   [(null? ls) s]
                   [else
                    (let ([c (car ls)])
                      (unless (char? c)
                        (error "list->string: not a character" c))
                      (string-set! s i c)
                      (fill s (+ i 1) (cdr ls)))]))])
       (lambda (ls)
         (let ([n (length ls)])
           (let ([s (make-string n)])
             (fill s 0 ls))))))