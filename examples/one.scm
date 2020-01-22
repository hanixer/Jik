; (let ([f (open-output-file "misc/output.txt")])
;     (let loop ([i 0])
;         (if (< i 5000)
;             (begin
;             (write-char #\a f)
;             (loop (+ i 1)))
;             #f)))

(foreign-call "printPtr" 1)
; (newline)
; (foreign-call "printPtr" 1)
; (newline)