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

(define (vec-squared-length v)
  (check-vec v 'vec-squared-length)
  (let ([x (vec-x v)]
        [y (vec-y v)]
        [z (vec-z v)])
    (fl+ (fl* x x) (fl+ (fl* y y) (fl* z z)))))

(define (vec-dot u v)
  (check-vec u 'vec-dot-1)
  (check-vec v 'vec-dot-2)
  (fl+ (fl* (vec-x u) (vec-x v))
       (fl+ (fl* (vec-y u) (vec-y v))
            (fl* (vec-z u) (vec-z v)))))

;;; Vector-vector arithmetic operations.
(define (vec-add u v)
  (check-vec v 'vec-add)
  (vec (fl+ (vec-x u) (vec-x v))
       (fl+ (vec-y u) (vec-y v))
       (fl+ (vec-z u) (vec-z v))))

(define (vec-sub u v)
  (check-vec v 'vec-sub)
  (vec (fl- (vec-x u) (vec-x v))
       (fl- (vec-y u) (vec-y v))
       (fl- (vec-z u) (vec-z v))))

;;; Vector-scalar arithmetic operations.
(define (vec-scale v s)
  (check-vec v 'vec-scale)
  (vec (fl* (vec-x v) s)
       (fl* (vec-y v) s)
       (fl* (vec-z v) s)))

(define (vec-s/ v s)
  (check-vec v 'vec-scale)
  (vec (fl/ (vec-x v) s)
       (fl/ (vec-y v) s)
       (fl/ (vec-z v) s)))

(define (unit-vector v)
  (check-vec v 'unit-vector)
  (vec-s/ v (vec-length v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rays
(define (ray? r)
  (and (vector? r)
       (eq? (vector-length r) 3)
       (eq? (vector-ref r 0) 'ray)))

(define (make-ray a b)
  (vector 'ray a b))

(define (ray-origin r)
  (vector-ref r 1))

(define (ray-direction r)
  (vector-ref r 2))

(define (point-at r t)
  (unless (ray? r) (error "not a ray" 'point-at r))
  (vec-add (ray-origin r)
    (vec-scale (ray-direction r) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hit.
(define (make-hit-record t p normal)
  (vector t p normal))

(define (make-hit-record-empty)
  (vector
    0.0
    (vec 0.0 0.0 0.0)
    (vec 0.0 0.0 0.0)))

(define (hit-record? rec)
  (and (vector? rec)
       (eq? (vector-length rec) 3)
       (flonum? (vector-ref rec 0))
       (vec? (vector-ref rec 1))
       (vec? (vector-ref rec 2))))

(define (hit-record-t rec)
  (vector-ref rec 0))

(define (hit-record-t-set! rec t)
  (vector-set! rec 0 t))

(define (hit-record-p rec)
  (vector-ref rec 1))

(define (hit-record-p-set! rec p)
  (vector-set! rec 1 p))

(define (hit-record-normal rec)
  (vector-ref rec 2))

(define (hit-record-normal-set! rec n)
  (vector-set! rec 2 n))

(define (copy-hit-record to from)
  (hit-record-t-set! to (hit-record-t from))
  (hit-record-p-set! to (hit-record-p from))
  (hit-record-normal-set! to (hit-record-normal from)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sphere.
(define (object-type o)
  (vector-ref o 0))

(define (make-sphere center r)
  (vector 'sphere center r))

(define (sphere? sphere)
  (and (vector? sphere)
       (eq? (vector-length sphere) 3)
       (eq? (vector-ref sphere 0) 'sphere)))

(define (sphere-center sphere)
  (vector-ref sphere 1))

(define (sphere-radius sphere)
  (vector-ref sphere 2))

(define (sphere-make-hit-rec r t center radius)
  (let* ([p (point-at r t)]
         [normal (vec-s/ (vec-sub p center) radius)])
    (make-hit-record t p normal)))

(define (hit-sphere r t-min t-max center radius)
  (let* ([oc (vec-sub (ray-origin r) center)]
         [a (vec-dot (ray-direction r) (ray-direction r))]
         [b (vec-dot oc (ray-direction r))]
         [c (fl- (vec-dot oc oc) (fl* radius radius))]
         [discriminant (fl- (fl* b b) (fl* a c))])
    (if (fl> discriminant 0.0)
      (let ([temp (fl/ (fl- (fl- 0.0 b) (sqrt discriminant)) a)])
        (if (and (fl< temp t-max) (fl> temp t-min))
          (sphere-make-hit-rec r temp center radius)
          (let ([temp (fl/ (fl+ (fl- 0.0 b) (sqrt discriminant)) a)])
            (if (and (fl< temp t-max) (fl> temp t-min))
              (sphere-make-hit-rec r temp center radius)
              #f))))
      #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object sequence.

(define (make-object-seq . objs)
  (vector 'object-seq objs))

(define (object-seq? o)
  (and (vector? o)
       (eq? (vector-length o) 2)
       (eq? (vector-ref o 0) 'object-seq)))

(define (object-seq-objs o)
  (vector-ref o 1))

(define (hit-object-seq r t-min t-max objs)
    (let loop ([objs objs] [hit-anything #f] [closest-so-far t-max] [rec #f])
      (if (empty? objs)
        rec
        (let* ([o (car objs)]
               [rec2 (hit r t-min closest-so-far o)])
          (if rec2
            (loop (cdr objs) #t (hit-record-t rec2) rec2)
            (loop (cdr objs) hit-anything closest-so-far rec))))))

(define (hit r t-min t-max obj)
  (cond
    ((sphere? obj)
      (hit-sphere r t-min t-max (sphere-center obj) (sphere-radius obj)))
    ((object-seq? obj)
      (hit-object-seq r t-min t-max (object-seq-objs obj)))
    (else (error "hit: wrong object"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Camera.
(define (make-camera)
  (let ([lower-left (vec -2.0 -1.0 -1.0)]
        [horizontal (vec 4.0 0.0 0.0)]
        [vertical (vec 0.0 2.0 0.0)]
        [origin (vec 0.0 0.0 0.0)])
    (vector 'camera lower-left horizontal vertical origin)))

(define (generate-ray camera u v)
  (let* ([lower-left (vector-ref camera 1)]
         [horizontal (vector-ref camera 2)]
         [vertical (vector-ref camera 3)]
         [origin (vector-ref camera 4)]
         [d (vec-add lower-left-corner
                    (vec-add (vec-scale horizontal u)
                            (vec-sub (vec-scale vertical v)
                                    origin)))])
    (make-ray origin d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random.
(define (random-in-unit-sphere)
  (define (pick-one)
      (let* ([x (random-flonum)]
             [y (random-flonum)]
             [z (random-flonum)]
             [v (vec x y z)]
             [v (vec-scale v 2.0)]
             [one (vec 1.0 1.0 1.0)]
             [diff (vec-sub v one)])
        diff))

  (let loop ([p (pick-one)])
    (if (fl>= (vec-squared-length p) 1.0)
        (loop (pick-one))
        p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
(define deee 0)
(define (color r world)
  (set! deee (+ deee 1))
  ; (display "color : : : : ")
  ; (display deee)
  ; (newline)
  (let ((result
  (let ([rec (hit r 0.001 1e20 world)])
    (if rec
      (let* ([N (hit-record-normal rec)]
             [p (hit-record-p rec)]
             [target (vec-add p (vec-add N (random-in-unit-sphere)))]
            ;  [target (vec-add p N)]
             [dir (vec-sub target p)]
             [r (make-ray p dir)]
             [col (color r world)])
        (vec-scale col 0.5))
        ;(vec-scale (vec-add (random-in-unit-sphere) (vec 1.0 1.0 1.0)) 0.5)) ; debug
      (let* ([dir (unit-vector (ray-direction r))]
             [t (fl* 0.5 (fl+ (vec-y dir) 1.0))])
        (vec-add (vec-scale (vec 1.0 1.0 1.0) (fl- 1.0 t))
                (vec-scale (vec 0.5 0.7 1.0) t)))))))
        ;(vec 1.0 1.0 1.0)))))) ; debug
  (set! deee (- deee 1))
  result))

(define world
  (make-object-seq
    (make-sphere (vec 0.0 0.0 -1.0) 0.5)
    (make-sphere (vec 0.0 -100.5 -1.0) 100.0)))

(define lower-left-corner (vec -2.0 -1.0 -1.0))
(define horizontal (vec 4.0 0.0 0.0))
(define vertical (vec 0.0 2.0 0.0))
(define origin (vec 0.0 0.0 0.0))

(define (make-samples world out nx ny ns camera i j)
  (let loop ([s 0] [col (vec 0.0 0.0 0.0)])
    (if (< s ns)
      (let* ([u (fl/ (fl+ (fixnum->flonum i) (random-flonum)) (fixnum->flonum nx))]
             [v (fl/ (fl+ (fixnum->flonum j) (random-flonum)) (fixnum->flonum ny))]
             [r (generate-ray camera u v)]
             [col2 (color r world)])
        (loop (+ s 1) (vec-add col col2)))
      (let* ([col (vec-s/ col (fixnum->flonum ns))]
             [ir (flonum->fixnum (fl* 255.59 (vec-x col)))]
             [ig (flonum->fixnum (fl* 255.59 (vec-y col)))]
             [ib (flonum->fixnum (fl* 255.59 (vec-z col)))])
        (display ir out)
        (display " " out)
        (display ig out)
        (display " " out)
        (display ib out)
        (newline out)))))

(define (main)
  (let* (;[out (open-output-file "misc/out.ppm")]
         [out (current-output-port)]
         [nx 200]
         [ny 100]
         [ns 24]
         [camera (make-camera)])
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
            (make-samples world out nx ny ns camera i j)))

    (close-port out)))

(main)

; (fl>= (vec-squared-length (vec 0.0 0.0 0.0)) 1.0)
; (list (random-flonum) (random-flonum) (random-flonum) (random-flonum))
; (vec-squared-length (random-in-unit-sphere))