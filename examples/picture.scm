(define for
  (lambda (from to body)
       (if (< from to)
           (begin
             (body from)
             (for (+ from 1) to body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors
(define (vec x y z)
  (let ([v (make-vector 3)])
    (vector-set! v 0 x)
    (vector-set! v 1 y)
    (vector-set! v 2 z)
    v))

(define (vec? v)
  (and (vector? v) (eq? (vector-length v) 3)))

(define (check-vec v who)
  (unless (vec? v)
    (error "not a vector" who v)))

(define (vec-x v) (vector-ref v 0))
(define (vec-y v) (vector-ref v 1))
(define (vec-z v) (vector-ref v 2))

(define (vec-length v)
  (check-vec v 'vec-length)
  (let ([x (vec-x v)]
        [y (vec-y v)]
        [z (vec-z v)])
    (sqrt (fl+ (fl* x x) (fl+ (fl* y y) (fl* z z))))))

(define (vec-dot u v)
  (check-vec u 'vec-dot-1)
  (check-vec v 'vec-dot-2)
  (fl+ (fl* (vec-x u) (vec-x v))
       (fl+ (fl* (vec-y u) (vec-y v))
            (fl* (vec-z u) (vec-z v)))))

;;; Vector-vector arithmetic operations.
(define (vec-v+ u v)
  (check-vec v 'vec-v+)
  (vec (fl+ (vec-x u) (vec-x v))
       (fl+ (vec-y u) (vec-y v))
       (fl+ (vec-z u) (vec-z v))))

(define (vec-v- u v)
  (check-vec v 'vec-v-)
  (vec (fl- (vec-x u) (vec-x v))
       (fl- (vec-y u) (vec-y v))
       (fl- (vec-z u) (vec-z v))))

;;; Vector-scalar arithmetic operations.
(define (vec-s* v s)
  (check-vec v 'vec-s*)
  (vec (fl* (vec-x v) s)
       (fl* (vec-y v) s)
       (fl* (vec-z v) s)))

(define (vec-s/ v s)
  (check-vec v 'vec-s*)
  (vec (fl/ (vec-x v) s)
       (fl/ (vec-y v) s)
       (fl/ (vec-z v) s)))

(define (unit-vector v)
  (check-vec v 'unit-vector)
  (vec-s/ v (vec-length v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rays
(define (make-ray a b)
  (let ([r (make-vector 2)])
    (vector-set! r 0 a)
    (vector-set! r 1 b)
    r))

(define (ray-origin r)
  (vector-ref r 0))

(define (ray-direction r)
  (vector-ref r 1))

(define (point-at r t)
  (vec-v+ (ray-origin r)
	  (vec-s* (ray-direction r) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sphere.
(define (hit-sphere center radius r)
  (let* ([oc (vec-v- (ray-origin r) center)]
         [a (vec-dot (ray-direction r) (ray-direction r))]
         [b (fl* 2.0 (vec-dot oc (ray-direction r)))]
         [c (fl- (vec-dot oc oc) (fl* radius radius))]
         [discriminant (fl- (fl* b b) (fl* 4.0 (fl* a c)))])
    (fl> discriminant 0.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
(define (color r)
  (if (hit-sphere (vec 0.0 0.0 -1.0) 0.5 r)
    (vec 1.0 0.0 0.0)
    (let* ([dir (unit-vector (ray-direction r))]
           [t (fl* 0.5 (fl+ (vec-y dir) 1.0))])
      (vec-v+ (vec-s* (vec 1.0 1.0 1.0)
		                  (fl- 1.0 t))
	            (vec-s* (vec 0.5 0.7 1.0) t)))))

(define lower-left-corner (vec -2.0 -1.0 -1.0))
(define horizontal (vec 4.0 0.0 0.0))
(define vertical (vec 0.0 2.0 0.0))
(define origin (vec 0.0 0.0 0.0))

(define (main)
  (let* ([out (open-output-file "misc/out.ppm")]
         [nx 200]
         [ny 100])
    (display "P3" out)
    (newline out)
    (display nx out)
    (display " " out)
    (display ny out)
    (newline out)
    (display 255 out)
    (newline out)

    (do ([j (- ny 1) (- j 1)])
        ((< j 0))
        (do ([i 0 (+ i 1)])
            ((= i nx))
            (let* ([u (fl/ (fixnum->flonum i) (fixnum->flonum nx))]
		   [v (fl/ (fixnum->flonum j) (fixnum->flonum ny))]
		   [d (vec-v+ lower-left-corner
			      (vec-v+ (vec-s* horizontal u)
				      (vec-s* vertical v)))]
		   [r (make-ray origin d)]
		   [col (color r)]
		   [ir (flonum->fixnum (fl* 255.59 (vec-x col)))]
		   [ig (flonum->fixnum (fl* 255.59 (vec-y col)))]
		   [ib (flonum->fixnum (fl* 255.59 (vec-z col)))])
	      (display ir out)
	      (display " " out)
	      (display ig out)
	      (display " " out)
	      (display ib out)
	      (newline out))))

    (close-port out)))

(main)
