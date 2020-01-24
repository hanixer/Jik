(define rand-next
  (lambda (s)
    (remainder (* s 19731979)
       219072107)))