;; Predefined functions for minischeme.
;; Functions whose name starts with % are meant to be private.

;; Logical negation
(define not
  (lambda (v)
    (if v 0 1)))

;; Non-primitive comparisons
(define > (lambda (x y) (if (< y x) 1 0)))
(define >= (lambda (x y) (if (<= y x) 1 0)))

;; Pairs
(define cons
  (lambda (fst snd)
    (vector fst snd)))
(define car (lambda (pair) (vector-ref pair 0)))
(define cdr (lambda (pair) (vector-ref pair 1)))

;; Lists
(define nil (vector))
(define null? (lambda (pair) (= pair nil)))

;; Integers
(define zero?
  (lambda (n) (= 0 n)))
(define abs
  (lambda (n)
    (if (< n 0) (- 0 n) n)))

;; Characters and strings
(define digit?
  (lambda (c)
    (and (<= #\0 c) (<= c #\9))))

(define string-length
  (lambda (s)
    (vector-ref s 0)))

(define %print-string
  (lambda (s i)
    (if (<= i (string-length s))
        (let ()
          (print-char (vector-ref s i))
          (%print-string s (+ i 1)))
        0)))
(define print-string
  (lambda (s)
    (%print-string s 1)))

;; Input/output
(define %read-int
  (lambda (acc)
    (let ((c (read-char)))
      (if (digit? c)
          (%read-int (+ (* 10 acc) (- c #\0)))
          acc))))
(define read-int
  (lambda ()
    (let ((c (read-char)))
      (if (= c #\-)
          (- 0 (%read-int 0))
          (if (digit? c)
              (%read-int (- c #\0))
              0)))))

(define %print-positive-int
  (lambda (n)
    (if (> n 9)
        (print-int (/ n 10))
        0)
    (print-char (+ #\0 (% n 10)))))
(define print-int
  (lambda (n)
    (if (< n 0)
        (print-char #\-)
        0)
    (%print-positive-int (abs n))))

(define print-space (lambda () (print-char 32)))
(define newline (lambda () (print-char 10)))
