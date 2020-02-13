(define for
  (lambda (from to body)
       (if (< from to)
           (begin
             (body from)
             (for (+ from 1) to body)))))

(define out (open-output-file "misc/out.ppm"))
(define nx 100)
(define ny 100)

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
        (let* ([r (fl/ (fixnum->flonum i) (fixnum->flonum nx))]
                       [g (fl/ (fixnum->flonum j) (fixnum->flonum ny))]
                       [b 0.2]
                       [ir (flonum->fixnum (fl* 255.59 r))]
                       [ig (flonum->fixnum (fl* 255.59 g))]
                       [ib (flonum->fixnum (fl* 255.59 b))])
                    (display ir out)
                    (display " " out)
                    (display ig out)
                    (display " " out)
                    (display ib out)
                    (newline out))))

(close-port out)
