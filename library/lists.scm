(define list
    (lambda args args))

(define empty?
    (lambda (ls)
        (null? ls)))

(define memq
    (lambda (x ls)
      (let f ([x x] [ls ls])
        (and (pair? ls)
             (if (eq? x (car ls))
                 ls
                 (f x (cdr ls)))))))

(define list?
    (letrec ([loop
            (lambda (h t)
            (if (pair? h)
                (let ([h (cdr h)])
                    (if (pair? h)
                        (and (not (eq? h t))
                            (loop (cdr h) (cdr t)))
                        (null? h)))
                (null? h)))])
    (lambda (x) (loop x x))))

(define length
    (lambda (ls)
        (if (empty? ls) 0 (+ 1 (length (cdr ls))))))

(define append
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (if (null? l2)
            l1
            (cons (car l1) (append (cdr l1) l2))))))

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

(define for-each
  (lambda (f l)
    (unless (null? l)
        (let ()
          (f (car l))
          (for-each f (cdr l))))))

(define %reverse-acc
  (lambda (l acc)
    (if (null? l)
        acc
        (%reverse-acc (cdr l) (cons (car l) acc)))))

(define reverse
  (lambda (l) (%reverse-acc l nil)))

(define map
  (lambda (f l)
    (if (null? l)
        l
        (cons (f (car l))
              (map f (cdr l))))))
