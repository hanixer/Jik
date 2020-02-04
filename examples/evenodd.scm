(letrec ([even?
           (lambda (n)
             (if (= n 0)
               even?
               (odd? (- n 1))))]
         [odd?
           (lambda (n)
             (if (= n 0)
               odd?
               (even? (- n 1))))])
    (even? 5))