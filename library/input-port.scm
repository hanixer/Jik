(define %input-port-id 11644978)
(define %input-port-length 7)
(define %input-buf-size 4096)

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
            v))))

(define close-input-port
    (lambda (input-port)
        (foreign-call "s_closeFile" (vector-ref input-port 2))))

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
            v))

(define current-input-port
    (lambda ()
        %curr-in-port))

(define port?
    (lambda (p)
        (or (output-port? p)
            (input-port? p))))