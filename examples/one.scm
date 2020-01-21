(let ([f (open-output-file "misc/output.txt")])
    (write-char #\a f)
    (flush-output-port f))