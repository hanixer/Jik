(make-vector 2)
(make-vector 2)

; (let ([v (make-vector 3)]
;       [u (make-vector 1)])
; 	(vector-set! u 0 2)
; 	(vector-set! v 0 40)
; 	(vector-set! v 1 #t)
; 	(vector-set! v 2 u)
; 	(if (vector-ref v 1)
; 		(+ (vector-ref v 0)
; 		   (vector-ref (vector-ref v 2) 0))
; 		44))