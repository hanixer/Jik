(define char-numeric?
    (lambda (c)
        (and (>= c #\0) (<= c #\9))))

(define char-alphabetic?
    (lambda (c)
        (or (and (>= c #\a) (<= c #\z))
            (and (>= c #\A) (<= c #\Z)))))