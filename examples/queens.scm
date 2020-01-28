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
       (let loop ((i n) (l '()))
            (if (= i 0)
                l
                (loop (- i 1) (cons (f (- i 1)) l))))))

(define list-range
     (lambda (f t)
          (list-tabulate (+ 1 (- t f)) (lambda (i) (+ f i)))))

(define list-zip-with-index
     (lambda (l)
          (list-zip l (list-range 1 (length l)))))

(define list-every?
     (lambda (p l)
          (let loop ((l l))
               (or (empty? l)
                   (and (p (car l))
                        (loop (cdr l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHECK IF NO TWO QUEENS IN A COLUMN

;; essentially checks for duplicates
(define col-ok
  (lambda (rows)
       (or (empty? rows)
           (and (list-every? (lambda (x) (not (= (car rows) x)))
                             (cdr rows))
                (col-ok (cdr rows))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHECK IF NO TWO QUEENS IN A DIAGONAL

;; depth denotes how many rows x and y are separated
(define on-diag
     (lambda (x y depth)
          (or (= (+ x depth) y)
              (= (- x depth) y))))

(define diag-ok
  (lambda (rows)
       (or (empty? rows)
           (and (list-every? (lambda (pair)
                                  (not (on-diag (car rows)
                                                (car pair)
                                                (cdr pair))))
                             (list-zip-with-index (cdr rows))) ;; index is the row distance from (car rows)
                (diag-ok (cdr rows))))))

;;;;;;;;;;;;;;;;;;;;;
;; CHECKING SOLUTIONS

(define partial-ok
     (lambda (rows)
          (and (col-ok rows)
               (diag-ok rows))))

;; not actually used in the algorithm below
(define queens-ok
     (lambda (rows n)
          (and (list-every? (lambda (x) (<= x n)) rows) ; no elt. bigger than n
               (= n (length rows))              ; n queens
               (partial-ok rows))))             ; no conflict

;;;;;;;;;;;;;;;;;;;;;
;; FINDING A SOLUTION

(define queens
     (letrec ((%advance
               (lambda (partial n)
                    (if (< (car partial) n)
                        (%queens (cons (+ 1 (car partial))
                                       (cdr partial))
                                 n) ;; try next value of (car partial)
                        '())))   ;; there's no solution for (cdr partial)
              (%queens
               (lambda (partial n)
                    (if (partial-ok partial)
                        (if (= (length partial) n)
                            partial ;; partial solution with full length: we're done
                            (let ((sol (%queens (cons 1 partial) n)))
                              (if (empty? sol)
                                  (%advance partial n)
                                  sol)))
                        (%advance partial n)))))
       (lambda (n) (%queens (list 1) n))))

;;;;;;;;;;;
;; PRINTING

(define for
  (lambda (from to body)
       (if (< from to)
           (begin
             (body from)
             (for (+ from 1) to body)))))

(define %header
     (lambda (rows)
          (newline)
          (write (length rows))
          (write "-queen(s)")
          (newline)
          (write "list: ")
          (write (reverse rows))
          (newline)
          (for 0 (length rows)
               (lambda (x) (write " _")))
          (newline)))

(define %row
     (lambda (p n)
          (for 0 n
               (lambda (x)
                    (write "|")
                    (write (if (= (+ x 1) p) "o" " "))))
          (write "|")
          (newline)))

(define %print-rows
  (lambda (rows n)
       (if (= (length rows) n)
           (%header rows))
       (if (empty? rows)
           (newline)
           (begin
             (%row (car rows) n)
             (%print-rows (cdr rows) n)))))

(define print-solution
     (lambda (rows)
          (display "start printing solution")
          (newline)
          (if (= (length rows) 0)
              (begin
                (write "no solution found!")
                (newline))
              (%print-rows (reverse rows) (length rows)))))

;;;;;;;;;;;;;;;;;
;; USER INTERFACE

(define tui
  (lambda ()
       (display "enter size (0 to exit)> ")
       (newline)
       (let ((size (read-int)))
         (unless (= size 0)
               (print-solution (queens size))
               (tui)))))


;; "main"
; (tui)

