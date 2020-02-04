
(define list-every?
     (lambda (p l)
          (let loop ((l l))
               (or (empty? l)
                   (and (p (car l))
                        (loop (cdr l)))))))

(define thing (lambda (p) p))