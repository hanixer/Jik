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

(define (vec-neg v)
  (check-vec v 'vec-neg)
  (vec (fl- 0.0 (vec-x v))
       (fl- 0.0 (vec-y v))
       (fl- 0.0 (vec-z v))))

(define (vec-sub u v)
  (check-vec v 'vec-sub)
  (vec (fl- (vec-x u) (vec-x v))
       (fl- (vec-y u) (vec-y v))
       (fl- (vec-z u) (vec-z v))))

(define (vec-mul u v)
  (check-vec v 'vec-mul)
  (vec (fl* (vec-x u) (vec-x v))
       (fl* (vec-y u) (vec-y v))
       (fl* (vec-z u) (vec-z v))))

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

(define (reflect v n)
  (vec-sub v (vec-scale n (fl* 2.0 (vec-dot v n)))))

(define (refract v n ni/nt)
  (let* ([uv (unit-vector v)]
         [dt (vec-dot uv n)]
         [discriminant (fl- 1.0 (fl* ni/nt (fl* ni/nt (fl- 1.0 (fl* dt dt)))))])
    (if (fl> discriminant 0.0)
      (vec-sub (vec-scale (vec-sub uv (vec-scale n dt)) ni/nt)
               (vec-scale n (sqrt discriminant)))
      #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rays
(define (ray? r)
  (and (vector? r)
       (eq? (vector-length r) 3)
       (eq? (vector-ref r 0) 'ray)))

(define (make-ray o dir)
  (unless (vec? o) (error "o not vec"))
  (unless (vec? dir) (error "dir not vec"))
  (vector 'ray o dir))

(define (ray-origin r)
  (unless (ray? r) (error "r not ray"))
  (vector-ref r 1))

(define (ray-direction r)
  (vector-ref r 2))

(define (point-at r t)
  (unless (ray? r) (error "point-at: not a ray"))
  (vec-add (ray-origin r)
    (vec-scale (ray-direction r) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hit.
(define (make-hit-record t p normal material)
  (unless (flonum? t) (error "t is not flonum"))
  (vector 'hit-record t p normal material))

(define (make-hit-record-empty)
  (vector
    'hit-record
    0.0
    (vec 0.0 0.0 0.0)
    (vec 0.0 0.0 0.0)
    #f))

(define (hit-record? rec)
  (and (vector? rec)
       (eq? (vector-ref rec 0) 'hit-record)))

(define (hit-record-t rec)
  (vector-ref rec 1))

(define (hit-record-p rec)
  (vector-ref rec 2))

(define (hit-record-normal rec)
  (vector-ref rec 3))

(define (hit-record-material rec)
  (vector-ref rec 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sphere.
(define (object-type o)
  (vector-ref o 0))

(define (make-sphere center r material)
  (vector 'sphere center r material))

(define (sphere? sphere)
  (and (vector? sphere)
       (eq? (vector-ref sphere 0) 'sphere)))

(define (sphere-center sphere)
  (vector-ref sphere 1))

(define (sphere-radius sphere)
  (vector-ref sphere 2))

(define (sphere-material sphere)
  (vector-ref sphere 3))

(define (hit-sphere r t-min t-max center radius material)

  (define (make-rec t)
    (unless (flonum? t) (error "make-rec - t is not a flonum"))
    (let* ([p (point-at r t)]
          [normal (vec-s/ (vec-sub p center) radius)])
      (make-hit-record t p normal material)))
  (unless (flonum? t-max) (error "t-max is not a flonum"))
  (unless (vec? (ray-origin r)) (error "ray-origin not vector"))
  (unless (vec? center) (error "center not vector"))

  (let* ([oc (vec-sub (ray-origin r) center)]
         [a (vec-dot (ray-direction r) (ray-direction r))]
         [b (vec-dot oc (ray-direction r))]
         [c (fl- (vec-dot oc oc) (fl* radius radius))]
         [discriminant (fl- (fl* b b) (fl* a c))])
    (if (fl> discriminant 0.0)
      (let ([temp (fl/ (fl- (fl- 0.0 b) (sqrt discriminant)) a)])
        (if (and (fl< temp t-max) (fl> temp t-min))
          (make-rec temp)
          (let ([temp (fl/ (fl+ (fl- 0.0 b) (sqrt discriminant)) a)])
            (if (and (fl< temp t-max) (fl> temp t-min))
              (make-rec temp)
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
      (unless (flonum? closest-so-far)
        (display closest-so-far)
        (display (symbol? closest-so-far))
        (error "cl-so-far not fl"))
      (if (empty? objs)
        rec
        (let* ([o (car objs)]
               [rec2 (hit r t-min closest-so-far o)])
          (unless (or (hit-record? rec2) (boolean? rec2))
            (display rec2)
            (error "rec2 not record"))
          (if rec2
            (loop (cdr objs) #t (hit-record-t rec2) rec2)
            (loop (cdr objs) hit-anything closest-so-far rec))))))

(define (hit r t-min t-max obj)
  (unless (ray? r) (error "hit: ray not ray"))
  (cond
    ((sphere? obj)
      (hit-sphere r t-min t-max (sphere-center obj) (sphere-radius obj) (sphere-material obj)))
    ((object-seq? obj)
      (hit-object-seq r t-min t-max (object-seq-objs obj)))
    (else (error "hit: wrong object"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Materials.
(define (make-lambertian albedo)
  (unless (vec? albedo) (error "make-lamb error"))
  (vector 'lambertian albedo))

(define (lambertian? m)
  (and (vector? m)
       (eq? (vector-ref m 0) 'lambertian)))

(define (lambertian-albedo m)
  (unless (vec? (vector-ref m 1)) (error 'lamb-albedo "error"))
  (vector-ref m 1))

(define (make-metal albedo fuzz)
  (vector 'metal albedo fuzz))

(define (metal? m)
  (and (vector? m)
       (eq? (vector-ref m 0) 'metal)))

(define (metal-albedo m)
  (vector-ref m 1))

(define (metal-fuzz m)
  (vector-ref m 2))

(define (make-dielectric ri)
  (vector 'dielectric ri))

(define (dielectric? m)
  (and (vector? m)
       (eq? (vector-ref m 0) 'dielectric)))

(define (dielectric-ri m)
  (vector-ref m 1))

(define (scatter-lambertian r-in rec albedo)
  (let* ([N (hit-record-normal rec)]
         [p (hit-record-p rec)]
         [target (vec-add p (vec-add N (random-in-unit-sphere)))]
         [dir (vec-sub target p)]
         [scattered (make-ray p dir)]
         [attenuation albedo])
         (unless (vec? attenuation) (error "scatte lamberti"))
    (cons attenuation scattered)))

(define (scatter-metal r-in rec albedo fuzz)
  (let* ([N (hit-record-normal rec)]
         [p (hit-record-p rec)]
         [udir (unit-vector (ray-direction r-in))]
         [reflected (reflect udir N)]
         [rand (random-in-unit-sphere)]
         [scat-dir (vec-add reflected (vec-scale rand fuzz))]
         [scattered (make-ray p scat-dir)]
         [attenuation albedo])
         (unless (vec? attenuation) (error "sc-met"))
    (if (fl> (vec-dot reflected N) 0.0)
      (cons attenuation scattered)
      #f)))

(define (scatter-dielectric r-in rec ref-idx)
  (let* ([N (hit-record-normal rec)]
         [p (hit-record-p rec)]
         [rdir (ray-direction r-in)]
         [reflected (reflect rdir N)]
         [outward? (fl> (vec-dot rdir N) 0.0)]
         [outward-normal (if outward? (vec-neg N) N)]
         [ni/nt (if outward? ref-idx (fl/ 1.0 ref-idx))]
         [refracted (refract rdir outward-normal ni/nt)]
         [attenuation (vec 1.0 1.0 0.0)]
         [scattered (make-ray p refracted)])
    (if refracted
      (cons attenuation scattered)
      #f)))

(define (scatter r-in rec material)
  (cond
    ((lambertian? material)
      (unless (vec? (lambertian-albedo material)) (error 'scatter "scatter "))
      (scatter-lambertian r-in rec (lambertian-albedo material)))
    ((metal? material) (scatter-metal r-in rec (metal-albedo material) (metal-fuzz material)))
    ((dielectric? material) (scatter-dielectric r-in rec (dielectric-ri material)))))

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
(define (color r world depth)
  (let ([rec (hit r 0.001 1e20 world)])
    (if (and rec (< depth 50))
      (let ([scat (scatter r rec (hit-record-material rec))])
        (if scat
          (let* ([attenuation (car scat)]
                 [scattered (cdr scat)]
                 [col (color scattered world (+ depth 1))])
            (unless (vec? attenuation) (error "atten is not vec" attenuation scat))
            (unless (ray? scattered) (error "scattered is not ray"))
            (vec-mul attenuation col))
          (vec 0.0 0.0 0.0)))
      (let* ([dir (unit-vector (ray-direction r))]
             [t (fl* 0.5 (fl+ (vec-y dir) 1.0))])
        (vec-add (vec-scale (vec 1.0 1.0 1.0) (fl- 1.0 t))
                 (vec-scale (vec 0.5 0.7 1.0) t))))))

(define world
  (make-object-seq
    (make-sphere (vec 0.0 0.0 -1.0) 0.5 (make-lambertian (vec 0.8 0.3 0.3)))
    (make-sphere (vec 0.0 -100.5 -1.0) 100.0 (make-lambertian (vec 0.8 0.8 0.0)))
    (make-sphere (vec 1.0 0.0 -1.0) 0.5 (make-metal (vec 0.8 0.6 0.2) 0.0))
    (make-sphere (vec -1.0 0.0 -1.0) 0.5 (make-dielectric 1.5))))

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
             [col2 (color r world 0)])
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
         [ns 100]
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

; (refract (vec 1.0 -1.0 0.0) (vec 0.0 1.0 0.0) 1.3)