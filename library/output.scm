(define write-char
    (lambda (char output-port)
        (unless (char? char) (error "write-char: char is not a char"))
        (let ([index (vector-ref output-port 4)])
            (unless (< index (vector-ref output-port 5))
                (flush-output-port output-port))
            (let ([index (vector-ref output-port 4)] ; index can be updated after flush
                  [buf (vector-ref output-port 3)])
                (string-set! buf index char)
                (vector-set! output-port 4 (+ index 1))))))

(define %write-positive-int
  (lambda (n)
    (when (> n 9)
        (write-int (quotient n 10)))
    (write-char (number->char (+ (char->number #\0)
                                 (remainder n 10)))
                (current-output-port))))

(define write-int
  (lambda (n)
    (when (< n 0)
        (write-char #\- (current-output-port)))
    (%write-positive-int (abs n))))

(define write-string
    (lambda (s)
        (let ([len (string-length s)])
            (let loop ([i 0])
                (when (< i len)
                    (write-char (string-ref s i) (current-output-port))
                    (loop (+ i 1))))
            (flush-output-port (current-output-port)))))

(define newline
    (lambda ()
        (write-char (number->char 10) (current-output-port))
        (flush-output-port (current-output-port))))