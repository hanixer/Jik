;;; Error
(define error
    (lambda args
        (foreign-call "error" args)))

;;; Lists

(define list
    (lambda args args))

(define empty?
    (lambda (ls)
        (null? ls)))

(define memq
    (lambda (x ls)
      (let f ([x x] [ls ls])
        (and (pair? ls)
             (if (eq? x (car ls))
                 ls
                 (f x (cdr ls)))))))

(define list?
    (letrec ([loop
            (lambda (h t)
            (if (pair? h)
                (let ([h (cdr h)])
                    (if (pair? h)
                        (and (not (eq? h t))
                            (loop (cdr h) (cdr t)))
                        (null? h)))
                (null? h)))])
    (lambda (x) (loop x x))))

(define length
    (lambda (ls)
        (if (empty? ls) 0 (+ 1 (length (cdr ls))))))

;;; Strings

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


;;; Symbols

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

;;; Vectors

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

;;; Foreign calls
(define %output-port-id 1999817)
(define %output-port-length 6)
(define %output-buf-size 4096)

(define output-port?
    (lambda (port)
        (and (vector? port)
             (eq? (vector-length port) %output-port-length)
             (eq? (vector-ref port 0) %output-port-id))))

(define open-output-file
    (lambda (filename)
    ; call open(filename)
    ; get file descriptor
    ;
    1
        ))