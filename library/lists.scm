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

(define cadr
  (lambda (x)
    (car (cdr x))))

(define cddr
  (lambda (x)
    (cdr (cdr x))))

(define caar
  (lambda (x)
    (car (car x))))

(define list-zip
     (lambda (l1 l2)
          (let loop ((l1 l1) (l2 l2))
               (if (or (empty? l1) (empty? l2))
                   '()
                   (cons
                    (cons (car l1) (car l2))
                    (loop (cdr l1) (cdr l2)))))))

(define (assq k m)
  (if (null? m)
      #f
      (if (eq? k (car (car m)))
          (car m)
          (assq k (cdr m)))))

(define eqv?
  (lambda (x y)
    (cond
      ((eq? x y))
      ((and (flonum? x) (flonum? y)) (fl= x y))
      ((number? x) (= x y))
      ((char? x) (and (char? y) (= x y)))
      (else #f)))) 

(define equal?
  (lambda (x y)
    (cond
      ((eqv? x y))
      ((pair? x)
       (and (pair? y)
            (equal? (car x) (car y))
            (equal? (cdr x) (cdr y))))
      ((string? x) (and (string? y) (string=? x y)))
      ((vector? x)
       (and (vector? y)
            (let ((n (vector-length x)))
              (and (= (vector-length y) n)
                   (let loop ((i 0))
                     (or (= i n)
                         (and (equal? (vector-ref x i) (vector-ref y i))
                              (loop (+ i 1)))))))))
      (else #f)))) 

(define (assoc k m)
  (if (null? m)
      #f
      (if (equal? k (car (car m)))
          (car m)
          (assq k (cdr m)))))