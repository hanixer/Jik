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
  (lambda (n p)
    (when (> n 9)
        (write-int (quotient n 10) p))
    (write-char (number->char (+ (char->number #\0)
                                 (remainder n 10)))
                p)))

(define write-int
  (lambda (n p)
    (when (< n 0)
        (write-char #\- p))
    (%write-positive-int (abs n) p)))

(define newline
    (lambda ()
        (write-char (number->char 10) (current-output-port))
        (flush-output-port (current-output-port))))

(define write #f)
(define display #f)

(let ()
  ;; wr is the driver, dispatching on the type of x
  (define wr
    (lambda (x d? p)
      (cond
        ((eof-object? x) (write-string "#<eof>" p))
        ((port? x) (write-string "#<port>" p))
        ((symbol? x) (write-string (symbol->string x) p))
        ((pair? x) (wrpair x d? p))
        ((number? x) (write-int x p))
        ((null? x) (write-string "()" p))
        ((boolean? x) (write-string (if x "#t" "#f") p))
        ((char? x) (if d? (write-char x p) (wrchar x p)))
        ((string? x) (if d? (write-string x p) (wrstring x p)))
        ((vector? x) (wrvector x d? p))
        ((procedure? x) (write-string "#<procedure>" p))
        (else (write-string "#<unknown>" p)))))

;   ;; write-string writes each character of s to p



(define write-string
    (lambda (s p)
        (let ([len (string-length s)])
            (let loop ([i 0])
                (when (< i len)
                    (write-char (string-ref s i) p)
                    (loop (+ i 1))))
            (flush-output-port p))))

;   ;; wrpair handles pairs and nonempty lists
  (define wrpair
    (lambda (x d? p)
      (write-char #\( p)
      (let loop ((x x))
        (wr (car x) d? p)
        (cond
           ((pair? (cdr x))
            (write-char (number->char 32) p)
            (loop (cdr x)))
           ((null? (cdr x)))
           (else
            (write-string " . " p)
            (wr (cdr x) d? p))))
      (write-char #\) p)))

;   ;; wrchar handles characters, recognizing and printing the
;   ;; special syntaxes for #\space and #\newline.  Used only when
;   ;; d? is #f.
  (define wrchar
    (lambda (x p)
      (cond
        [(eq? x (number->char 10)) (write-string "#\\newline" p)]
        [(eq? x (number->char 32)) (write-string "#\\space" p)]
        [else (write-string "#\\" p)
              (write-char x p)])))

;   ;; wrstring handles strings, inserting slashes where
;   ;; necessary.  Used only when d? is #f.
  (define wrstring
    (lambda (x p)
      (write-char #\" p)
      (let ((n (string-length x)))
        (let loop ([i 0])
          (when (< i n)
            (let ((c (string-ref x i)))
              (when (or (= c #\") (= c #\\))
                (write-char #\\ p))
              (write-char c p))
            (loop (+ i 1)))))
      (write-char #\" p)))

;   ;; wrvector handles vectors
  (define wrvector
    (lambda (x d? p)
      (write-string "#(" p)
      (let ([size (vector-length x)])
        (unless (= size 0)
          (let ([last (- size 1)])
            (let loop ([i 0])
              (wr (vector-ref x i) d? p)
              (unless (= i last)
                (write-char (number->char 32) p)
                (loop (+ i 1)))))))
      (write-char #\) p)))

; ;; write calls wr with d? set to #f
(set! write
  (lambda (x . rest)
    (if (null? rest)
        (wr x #f (current-output-port))
        (wr x #f (car rest)))))

; ;; display calls wr with d? set to #t
(set! display
  (lambda (x . rest)
    (if (null? rest)
        (wr x #t (current-output-port))
        (wr x #t (car rest))))))