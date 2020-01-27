;; In Emacs, open this file in -*- Scheme -*- mode.

;; solutions are represented as integer list, where the index denotes the
;; row (from the bottom), the value the column. for example, the solution
;; for n = 4
;;   _ _ _ _
;;  | |o| | |
;;  | | | |o|
;;  |o| | | |
;;  | | |o| |
;;
;; is represented as (3, 1, 4, 2)

;; SOME USEFUL LIST FUNCTIONS

(define list-tabulate
  (lambda (n f)
       (let loop ((i n) (l list-empty))
            (if (= i 0)
                l
                (loop (- i 1) (cons (f (- i 1)) l))))))

(define list-range
     (lambda (f t)
          (list-tabulate (+ 1 (- t f)) (lambda (i) (+ f i)))))

(define list-zip-with-index
     (lambda (l)
          (list-zip l (list-range 1 (list-length l)))))

(define list-int-print
     (lambda (l)
          (write #\()
                      (list-for-each (lambda (elem)
                                          (int-print elem)
                                          (char-print ' '))
                                     l)
                      (char-print ')')))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHECK IF NO TWO QUEENS IN A COLUMN

;; essentially checks for duplicates
(define col-ok
  (fun (rows)
       (or (list-empty? rows)
           (and (list-every? (fun (x) (not (= (list-head rows) x)))
                             (list-tail rows))
                (col-ok (list-tail rows))))))
