(define empty? (lambda (x) (null? x)))

(define list (lambda args args))

(define one
     (lambda (x)
          (empty? x)))

(define two
     (lambda (x)
          (cons (one x) x)))

(define three
     (lambda (x)
          (list
               ; x
               ; (two x)
               ; (two x)
               (two x))))

(define four
     (lambda (x)
          (list
               (cdr (three x))
               (car (three x)))))

; (pair? (four (list 1 2 3)))
; (three (cons 1 (cons 2 (cons 3 '()))))
(three (list 1 2 3))