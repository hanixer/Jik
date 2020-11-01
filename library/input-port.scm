(define %input-port-id 11644978)
(define %input-port-length 8)
(define %input-buf-size 16)

(define input-port?
    (lambda (port)
        (and (vector? port)
             (eq? (vector-length port) %input-port-length)
             (eq? (vector-ref port 0) %input-port-id))))

(define open-input-file
    (lambda (filename)
    (let ([fd (foreign-call "s_openFileR" filename)])
        (when (eq? fd -1)
            (error "failed to open file"))
        (let ([buf (make-string %input-buf-size)]
              [v (make-vector %input-port-length)])
            (vector-set! v 0 %input-port-id)
            (vector-set! v 1 filename)
            (vector-set! v 2 fd)
            (vector-set! v 3 buf)
            (vector-set! v 4 0) ; index to next position in the buffer
            (vector-set! v 5 0) ; number of bytes that was read
            (vector-set! v 6 %input-buf-size) ; size of the buffer
            (vector-set! v 7 #f) ; lookahead
            v))))

(define close-input-port
    (lambda (input-port)
        (foreign-call "s_closeFile" (vector-ref input-port 2))))

(define close-port
    (lambda (p)
        (cond
          [(input-port? p) (close-input-port p)]
          [(output-port? p) (close-output-port p)]
          [else
            (error "close-port: not a port given")])))

(define %curr-in-port
    (let ([buf (make-string %input-buf-size)]
          [v (make-vector %input-port-length)])
            (vector-set! v 0 %input-port-id)
            (vector-set! v 1 "")
            (vector-set! v 2 0)
            (vector-set! v 3 buf)
            (vector-set! v 4 0) ; index to next position in the buffer
            (vector-set! v 5 0) ; number of bytes that was read
            (vector-set! v 6 %input-buf-size) ; size of the buffer
            (vector-set! v 7 #f) ; lookahead
            v))

(define current-input-port
    (lambda ()
        %curr-in-port))

(define port?
    (lambda (p)
        (or (output-port? p)
            (input-port? p))))

(define read-char #f)
(define peek-char #f)

(let ()
  (define port-data-available?
    (lambda (ip)
      (< (vector-ref ip 4) (vector-ref ip 5))))

  (define read-data-if-needed
    (lambda (ip)
      (unless (port-data-available? ip)
              (let ([count
                      (foreign-call "s_read"
                                    (vector-ref ip 2)
                                    (vector-ref ip 3)
                                    (vector-ref ip 6))])
                (vector-set! ip 5 count)
                (vector-set! ip 4 0)))))

  (define rd-single
    (lambda (ip)
      (if (port-data-available? ip)
        (let ([index (vector-ref ip 4)]
              [buf (vector-ref ip 3)])
          (vector-set! ip 4 (+ index 1))
          (string-ref buf index))
        (eof-object))))

  (define rd
    (lambda (ip)
      (unless (input-port? ip) (error "input-port is expected in rd"))

      (if (vector-ref ip 7)
        (let ([c (vector-ref ip 7)])
          (vector-set! ip 7 #f)
          c)
        (begin
          (read-data-if-needed ip)
          (rd-single ip)))))

  (define pk
    (lambda (ip)
      (unless (input-port? ip) (error "input-port is expected in pk"))

        (unless (vector-ref ip 7)
            (read-data-if-needed ip)
            (vector-set! ip 7 (rd-single ip))) ; Set lookahead.
        (vector-ref ip 7)))

  (set! peek-char
    (lambda args
      (if (null? args)
          (pk (current-input-port))
          (pk (car args)))))

  (set! read-char
      (lambda args
        (if (null? args)
          (rd (current-input-port))
          (rd (car args))))))