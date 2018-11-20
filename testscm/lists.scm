;; Warning: this file makes extensive use of closures, and is
;; therefore unusable before closure conversion is implemented in the
;; compiler.
;;
;; Some useful list functions. Should be compatible with the standard
;; list functions in R5RS and SRFI-1, with the following exceptions:
;; partition and split-at return a pair instead of two values, since
;; minischeme doesn't support multiple return values.
;;
;;   R5RS: http://www.schemers.org/Documents/Standards/R5RS/
;; SRFI-1: http://srfi.schemers.org/srfi-1/

(define for-each
  (lambda (f l)
    (if (null? l)
        0
        (let ()
          (f (car l))
          (for-each f (cdr l))))))

(define map
  (lambda (f l)
    (if (null? l)
        l
        (cons (f (car l)) (map f (cdr l))))))

(define fold
  (lambda (f z l)
    (if (null? l)
        z
        (fold f (f z (car l)) (cdr l)))))

(define fold-right
  (lambda (f z l)
    (if (null? l)
        z
        (f (car l) (fold-right f z (cdr l))))))

(define filter
  (lambda (p l)
    (fold-right (lambda (e r)
                  (if (p e) (cons e r) r))
                nil
                l)))

(define partition
  (lambda (p l)
    (fold-right (lambda (e y/n)
                  (if (p e)
                      (cons (cons e (car y/n)) (cdr y/n))
                      (cons (car y/n) (cons e (cdr y/n)))))
                (cons nil nil)
                l)))

(define take
  (lambda (l n)
    (if (= 0 n)
        nil
        (cons (car l) (take (cdr l) (- n 1))))))

(define drop
  (lambda (l n)
    (if (= 0 n)
        l
        (drop (cdr l) (- n 1)))))

(define split-at
  (lambda (l n)
    (if (= 0 n)
        (cons nil l)
        (let ((h/t (split-at (cdr l) (- n 1))))
          (cons (cons (car l) (car h/t))
                (cdr h/t))))))

(define length
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (length (cdr l))))))

(define %reverse-acc
  (lambda (l acc)
    (if (null? l)
        acc
        (%reverse-acc (cdr l) (cons (car l) acc)))))

(define reverse
  (lambda (l) (%reverse-acc l nil)))

(define append
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (if (null? l2)
            l1
            (cons (car l1) (append (cdr l1) l2))))))

(define member
  (lambda (e l equal?)
    (if (null? l)
        nil
        (if (equal? e (car l))
            l
            (member e (cdr l) equal?)))))

(define print-int-list
  (lambda (l)
    (print-char #\()
    (for-each (lambda (elem)
                (print-int elem)
                (print-space))
              l)
    (print-char #\))))

(define list1
  (lambda (e1) (cons e1 nil)))
(define list2
  (lambda (e1 e2) (cons e1 (list1 e2))))
(define list3
  (lambda (e1 e2 e3) (cons e1 (list2 e2 e3))))
(define list4
  (lambda (e1 e2 e3 e4) (cons e1 (list3 e2 e3 e4))))
(define list5
  (lambda (e1 e2 e3 e4 e5) (cons e1 (list4 e2 e3 e4 e5))))
(define list6
  (lambda (e1 e2 e3 e4 e5 e6) (cons e1 (list5 e2 e3 e4 e5 e6))))
(define list7
  (lambda (e1 e2 e3 e4 e5 e6 e7) (cons e1 (list6 e2 e3 e4 e5 e6 e7))))
(define list8
  (lambda (e1 e2 e3 e4 e5 e6 e7 e8) (cons e1 (list7 e2 e3 e4 e5 e6 e7 e8))))
(define list9
  (lambda (e1 e2 e3 e4 e5 e6 e7 e8 e9) (cons e1 (list8 e2 e3 e4 e5 e6 e7 e8 e9))))

(define merge
  (lambda (l1 l2 smaller?)
    (if (null? l1)
        l2
        (if (null? l2)
            l1
            (let ((h1 (car l1)) (h2 (car l2)))
              (if (smaller? h1 h2)
                  (cons h1 (merge (cdr l1) l2 smaller?))
                  (cons h2 (merge l1 (cdr l2) smaller?))))))))

(define sort
  (lambda (l smaller?)
    (let ((len (length l)))
      (if (< len 2)
          l
          (let ((h/t (split-at l (/ len 2))))
            (merge (sort (car h/t) smaller?)
                   (sort (cdr h/t) smaller?)
                   smaller?))))))
