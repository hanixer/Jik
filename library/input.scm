(define read-char*
    (lambda (input-port)
        (let ([index (vector-ref input-port 4)]
              [max (vector-ref input-port 5)]
              [buf (vector-ref input-port 3)]
              [fd (vector-ref input-port 2)])
            (unless (< index max)
                (let ([count (foreign-call "s_read" fd buf (string-length buf))])
                    (vector-set! input-port 5 count)
                    (vector-set! input-port 4 0)))

        (let ([index (vector-ref input-port 4)]
              [max (vector-ref input-port 5)])
              (if (< index max)
                (let ([char (string-ref buf index)])
                    (vector-set! input-port 4 (+ index 1))
                    char)
                (eof-object))))))

(define read-char
    (lambda () (read-char* (current-input-port))))

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