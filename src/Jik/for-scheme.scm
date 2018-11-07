(use-modules (language cps))
(use-modules (language cps intmap))
(use-modules (language tree-il spec))
(use-modules (language tree-il))

(define (tak x y z)
    (if (not (< y x))
        z
        (tak (tak (- x 1) y z)
             (tak (- y 1) z x)
             (tak (- z 1) x y))))

(define (goog x)
  (list
(lambda () (set! x (+ x 1)) x)))
(define (insp exp)
  (compile exp #:from 'scheme #:to 'tree-il))

(define (inspect exp)
  (let ([cpsed (compile exp #:from 'scheme #:to 'cps)]
	[til (compile exp #:from 'scheme #:to 'tree-il)])
    (format #t "#<tree-il ~S>~%" (unparse-tree-il til))
    (write
     (intmap-fold 
      (lambda (k v a)
	(format #t "~A #<cps ~S>~%" k (unparse-cps v))
	a) cpsed '()))))

'(define (make-counter)
  (let* ((n 0)
	 (l (lambda ()
	      (set! n (+ n 2))
	      n)))
    (l)
    l))

#|

(let ((make-counter 
       (fun () 
	    (let ((x 0))
	      (let ((l 
		     (fun ()
			  (let ((m 2))
			    (letcont ((k (cont r (set! n r))))
				     (k (+ n m)))))))
		(letcont ((k2 (cont () (return l))))
			 (k (l))))))))
  (make-counter))
	    


|#

(define thing
  '(let* ((grab #t)
	  (sayno #t)
	  (saymaybe #t)
	  (foo (lambda (x)
     (if (grab x)
	 (sayno x)
	 (saymaybe x)))))
  (foo 9393)))

#|

(let ((f (fun (x) 
	      (letcont ((k1 (cont (v1)
				  (if v1
				      (sayno x)
				      (saymaybe x)))))))))
  (f 2))
				  

|#

(define factor
  '(define (factorial n)
     (define (loop n a)
       (if (<= n 1) 
	   n
	   (loop (- n 1) (* a n))))
     (loop n 1)))

#|

(fun (n)
  (let ((loop (fun (n a)
		   (let ((k1 (cont (v1)
				   (if v1
				       (return n)
				       (let ((v2 (- n 1))
					     (v3 (* a n)))
					 (loop v2 v3))))))
		     (k1 (<= n 1))))))
    (let ((m 1))
      (loop n m))))

|#

(define letrecs
  '(letrec ((f (lambda (x) (what g x)))
	    (g (lambda (x) (what f x))))
     (f (g (+ f g f g)))
     g))

(define e1
  '(let ((n 0))
     (lambda ()
       (set! n (+ n 1))
       n)))

(define e2
  '(letrec ((fact (lambda (n)
		    (if (<= n 1)
			n
			(* n (fact (- n 1)))))))
     fact))

(define e3
  '(let ((f (lambda (f a b) (f a b))))
     (list
      f
      (f + 1 2)
      (f (lambda (a b) 12345) 3 4))))

(define e4
  '(define (fo lst) (map - lst)))

(define e5
  '(let ((x W))
    (let ((y (+ W x 123)))
(if W
    (let ((z (+ x W x))
        (oijewroij 1234))
        (+ x  W z))
    (let ((w (+ W y y)))
        (+ W w y))))))

(define e6
  '(define (mything)
     (letrec ((f (lambda (x) (f f)))
	    (BIG (+ f 123))
	    (SMA (lambda () (lambda (x) x)))
	    (g (lambda (e) (lambda () (+ 9080 BIG)))))
     (list f g x))))

(define e7
  '(define (funk)
     (let* ((x 0) (y 0) (z GLOBAL)
	    (f (lambda ()
		 (let ((LOCAL 765))
		 (set! x 111)
		 (set! y 222)
		 (set! LOCAL 1010)
		 (list x y z LOCAL)))))
       f)))

(define e8
  '(define (funk)
     (lambda (a) (lambda (b) (lambda (c) (+ a b c))))))

(define e9
  '(lambda (x1 x2 x3 x111 x222 x333 x33) (if (if x1 x2 x3) (if x111 x222 x333) x33)))

(define (fgh f g h)
  (let ([x (if (f g)
	       h
	       (if (f g)
		   g
		   h))])
    (x f g h)))

(define (xyzw x y z w) (+ (if x y z) y z))
