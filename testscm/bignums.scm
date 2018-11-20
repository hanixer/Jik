;; Basic computations (addition, multiplication and factorial) on big
;; numbers. These are represented as lists of "digits" in the base
;; specified below, in order of increasing weight.

(define base 10000)

(define int->bignum list1)

(define bignum-print
  (lambda (b)
    (let ((rev-b (reverse b)))
      (print-int (car rev-b))
      (for-each (lambda (d)
                  (if (< d 1000) (print-int 0) nil)
                  (if (< d 100) (print-int 0) nil)
                  (if (< d 10) (print-int 0) nil)
                  (print-int d))
                (cdr rev-b)))))

(define %+b
  (lambda (b1 b2 carry)
    (if (null? b1)
        (if (= 0 carry) b2 (%+b (int->bignum carry) b2 0))
        (if (null? b2)
            (if (= 0 carry) b1 (%+b b1 (int->bignum carry) 0))
            (let ((d1 (car b1)) (d2 (car b2)))
              (let ((res (+ (+ d1 d2) carry)))
                (cons (% res base) (%+b (cdr b1) (cdr b2) (/ res base)))))))))

;; Addition of two big numbers
(define +b
  (lambda (b1 b2)
    (%+b b1 b2 0)))

(define %scale-bignum
  (lambda (b n carry)
    (if (null? b)
        (if (= 0 carry) nil (int->bignum carry))
        (let ((sh (+ (* (car b) n) carry)))
          (cons (% sh base) (%scale-bignum (cdr b) n (/ sh base)))))))

(define scale-bignum
  (lambda (b n)
    (%scale-bignum b n 0)))

(define *b
  (lambda (b1 b2)
    (if (null? b1)
        nil
        (+b (scale-bignum b2 (car b1))
            (scale-bignum (*b (cdr b1) b2) base)))))


(define zero-b? null?)

(define bignum-fact
  (lambda (n)
    (if (= 0 n)
        (int->bignum 1)
        (*b (int->bignum n) (bignum-fact (- n 1))))))

(print-string "Compute the factorial of? ")
(let ((n (read-int)))
  (print-int n)
  (print-string "! = ")
  (bignum-print (bignum-fact n))
  (newline))
