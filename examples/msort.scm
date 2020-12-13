(define (split ls)
  (letrec ([split-h (lambda (ls ls1 ls2)
                      (cond
                        [(or (null? ls) (null? (cdr ls)))
                         (cons (reverse ls2) ls1)]
                        [else (split-h (cddr ls)
                                (cdr ls1) (cons (car ls1) ls2))]))])
    (split-h ls ls '())))

(define (merge pred ls1 ls2)
  (cond
    [(null? ls1) ls2]
    [(null? ls2) ls1]
    [(pred (car ls1) (car ls2))
     (cons (car ls1) (merge pred (cdr ls1) ls2))]
    [else (cons (car ls2) (merge pred ls1 (cdr ls2)))]))

(define (merge-sort pred ls)
  (cond
    [(null? ls) ls]
    [(null? (cdr ls)) ls]
    [else (let ([splits (split ls)])
            (merge pred
              (merge-sort pred (car splits))
              (merge-sort pred (cdr splits))))]))

(merge-sort (lambda (x y) (< x y)) '(9 6 7 3))
