(define list
    (lambda args args))

(define empty?
    (lambda (ls)
        (null? ls)))

(define %reverse-acc
  (lambda (l acc)
    (if (null? l)
        acc
        (%reverse-acc (cdr l) (cons (car l) acc)))))

(define reverse
  (lambda (l) (%reverse-acc l nil)))

(define length
    (lambda (ls)
        (if (empty? ls) 0 (+ 1 (length (cdr ls))))))

(define for-each
  (lambda (f l)
    (unless (null? l)
        (let ()
          (f (car l))
          (for-each f (cdr l))))))

(define for
  (lambda (from to body)
       (if (< from to)
           (begin
             (body from)
             (for (+ from 1) to body)))))

(define %header
     (lambda (rows)
          (length rows)
          (reverse rows)
          (for 0 (length rows)
               (lambda (x) x))))

(define history '())

(define display (lambda (x) (set! history (cons "display" history))))
(define newline (lambda () (set! history (cons "newline" history))))


(define %header
     (lambda (rows)
          (newline)
          (display (length rows))
          (display "-queen(s)")
          (newline)
          (display "list: ")
          (display (reverse rows))
          (newline)
          (for 0 (length rows)
               (lambda (x) (display " _")))
          (newline)))

(define %row
     (lambda (p n)
          (for 0 n
               (lambda (x)
                    (display "|")
                    (display (if (= (+ x 1) p) "o" " "))))
          (display "|")
          (newline)))

(define %print-rows
  (lambda (rows n)
     (display "entered print-rows")
     (newline)
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
                (display "no solution found!")
                (newline))
              (%print-rows (reverse rows) (length rows)))))

(print-solution (list 1 3 2 4 1 3 2 4 1 3 2 4 1 3 2 4 1 3 2 4 1 3 2 4 1 3 2 4 1 3 2 4 1 3 2 4 1 3 2 4 1 3 2 4 1 3 ))
history