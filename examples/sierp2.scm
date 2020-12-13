(define (sierpinski n)
  (for-each
   (lambda (x) (display (list->string x)) (newline))
   (let loop ((acc (list (list #\*))) (spaces (list #\ )) (n n))
     (if (zero? n)
         acc
         (loop
          (append
           (map (lambda (x) (append (append spaces x) spaces)) acc)
           (map (lambda (x) (append (append x (list #\ )) x)) acc))
          (append spaces spaces)
          (- n 1))))))

(define (sierpinski2 n)
   (let loop ((acc (list (list #\*))) (spaces (list #\ )) (n n))
     (if (zero? n)
         acc
         (loop
          (append
           (map (lambda (x) (append (append spaces x) spaces)) acc)
           (map (lambda (x) (append (append x (list #\ )) x)) acc))
          (append spaces spaces)
          (- n 1)))))

(sierpinski 4)
