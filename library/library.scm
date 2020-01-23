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

;;; Error
(define error
    (lambda args
        (foreign-call "s_error" args)))

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

(define exit-scheme
    (lambda ()
        (foreign-call "s_exit" 0)))

(define newline
    (lambda ()
        (foreign-call "s_write" 1 "\n" 1)))

;;; Output ports

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
    (let ([fd (foreign-call "s_openFileW" filename)])
        (when (eq? fd -1)
            (error "failed to open file"))
        (let ([buf (make-string %output-buf-size)]
              [v (make-vector %output-port-length)])
            (vector-set! v 0 %output-port-id)
            (vector-set! v 1 filename)
            (vector-set! v 2 fd)
            (vector-set! v 3 buf)
            (vector-set! v 4 0) ; index to next position in the buffer
            (vector-set! v 5 %output-buf-size) ; size of the buffer
            v))))

(define flush-output-port
    (lambda (output-port)
        (let ([fd (vector-ref output-port 2)]
              [index (vector-ref output-port 4)]
              [buf (vector-ref output-port 3)])
         (foreign-call "s_write" fd buf index )
         (vector-set! output-port 4 0))))

(define write-char
    (lambda (char output-port)
        (let ([index (vector-ref output-port 4)])
            (unless (< index (vector-ref output-port 5))
                (flush-output-port output-port))
            (let ([index (vector-ref output-port 4)] ; index can be updated after flush
                  [buf (vector-ref output-port 3)])
                (string-set! buf index char)
                (vector-set! output-port 4 (+ index 1))))))

(define close-output-port
    (lambda (output-port)
        (flush-output-port output-port)
        (foreign-call "s_closeFile" (vector-ref output-port 2))))

(define %curr-out-port
    (let ([buf (make-string %output-buf-size)]
           [v (make-vector %output-port-length)])
            (vector-set! v 0 %output-port-id)
            (vector-set! v 1 "")
            (vector-set! v 2 1)
            (vector-set! v 3 buf)
            (vector-set! v 4 0) ; index to next position in the buffer
            (vector-set! v 5 %output-buf-size) ; size of the buffer
            v))

(define current-output-port
    (lambda ()
        %curr-out-port))

;;; Input ports
(define %input-port-id 11644978)
(define %input-port-length 7)
(define %input-buf-size 4096)

(define input-port?
    (lambda (port)
        (and (vector? port)
             (eq? (vector-length port) %input-port-length)
             (eq? (vector-ref port 0) %input-port-id))))

(define open-input-file
    (lambda (filename)
    (let ([fd (foreign-call "s_openFileR" filename)])
        (when (eq? fd -1)
            (error "failed to open file"))
        (let ([buf (make-string %input-buf-size)]
              [v (make-vector %input-port-length)])
            (vector-set! v 0 %input-port-id)
            (vector-set! v 1 filename)
            (vector-set! v 2 fd)
            (vector-set! v 3 buf)
            (vector-set! v 4 0) ; index to next position in the buffer
            (vector-set! v 5 0) ; number of bytes that was read
            (vector-set! v 6 %input-buf-size) ; size of the buffer
            v))))

(define read-char
    (lambda (input-port)
        (let ([index (vector-ref input-port 4)]
              [max (vector-ref input-port 5)]
              [buf (vector-ref input-port 3)]
              [fd (vector-ref input-port 2)])
            (unless (< index max)
                (let ([count (foreign-call "s_read" fd buf (string-length buf))])
                    (vector-set! input-port 5 count)
                    (vector-set! input-port 4 0)))

        (let ([index (vector-ref input-port 4)]
              [max (vector-ref input-port 5)])
              (unless (< index max)
                (error "nothing to read"))
              (let ([char (string-ref buf index)])
                    (vector-set! input-port 4 (+ index 1))
                    char)))))

(define close-input-port
    (lambda (input-port)
        (foreign-call "s_closeFile" (vector-ref input-port 2))))

(define %curr-in-port
    (let ([buf (make-string %input-buf-size)]
           [v (make-vector %input-port-length)])
            (vector-set! v 0 %input-port-id)
            (vector-set! v 1 "")
            (vector-set! v 2 1)
            (vector-set! v 3 buf)
            (vector-set! v 4 0) ; index to next position in the buffer
            (vector-set! v 5 %input-buf-size) ; size of the buffer
            v))

(define current-input-port
    (lambda ()
        %curr-in-port))