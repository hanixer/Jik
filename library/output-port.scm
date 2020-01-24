
(define %output-port-id 1999817)
(define %output-port-length 6)
(define %output-buf-size 4096)

(define output-port?
    (lambda (port)
        (and (vector? port)
             (eq? (vector-length port) %output-port-length)
             (eq? (vector-ref port 0) %output-port-id))))

(define open-output-file
    (lambda (filename)
    (let ([fd (foreign-call "s_openFileW" filename)])
        (when (eq? fd -1)
            (error "failed to open file"))
        (let ([buf (make-string %output-buf-size)]
              [v (make-vector %output-port-length)])
            (vector-set! v 0 %output-port-id)
            (vector-set! v 1 filename)
            (vector-set! v 2 fd)
            (vector-set! v 3 buf)
            (vector-set! v 4 0) ; index to next position in the buffer
            (vector-set! v 5 %output-buf-size) ; size of the buffer
            v))))

(define flush-output-port
    (lambda (output-port)
        (let ([fd (vector-ref output-port 2)]
              [index (vector-ref output-port 4)]
              [buf (vector-ref output-port 3)])
         (foreign-call "s_write" fd buf index )
         (vector-set! output-port 4 0))))

(define close-output-port
    (lambda (output-port)
        (flush-output-port output-port)
        (foreign-call "s_closeFile" (vector-ref output-port 2))))

(define %curr-out-port
    (let ([buf (make-string %output-buf-size)]
           [v (make-vector %output-port-length)])
            (vector-set! v 0 %output-port-id)
            (vector-set! v 1 "")
            (vector-set! v 2 1)
            (vector-set! v 3 buf)
            (vector-set! v 4 0) ; index to next position in the buffer
            (vector-set! v 5 %output-buf-size) ; size of the buffer
            v))

(define current-output-port
    (lambda ()
        %curr-out-port))