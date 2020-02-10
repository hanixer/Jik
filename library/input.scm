(define digit?
  (lambda (c)
    (and (<= (char->number #\0) (char->number c)) (<= (char->number c) (char->number #\9)))))

(define %read-int
  (lambda (acc)
    (let ((c (read-char)))
      (if (digit? c)
          (%read-int (+ (* 10 acc) (- (char->number c) (char->number #\0))))
          acc))))

(define read-int
  (lambda ()
    (let ((c (read-char)))
      (if (= c #\-)
          (- 0 (%read-int 0))
          (if (digit? c)
              (%read-int (- (char->number c) (char->number #\0)))
              0)))))