(define % (lambda (a b) (remainder a b)))
(define / (lambda (a b) (quotient a b)))
(define print-char (lambda (c) (write-char c (current-output-port))))
(define print-string write-string)
(define nil '())

(define abs
  (lambda (x)
    (if (< x 0)
        (- 0 x)
        x)))


(define rand-next
  (lambda (s)
    (% (* s 19731979)
       219072107)))

(define print-n-char
  (lambda (n c)
    (when (> n 0)
        (let ([c (if (number? c) (number->char c) c)])
          (print-char c)
          (print-n-char (- n 1) c)))))

(define contains
  (lambda (l elem)
    (if (null? l)
        0
        (if (= (car l) elem)
            1
            (contains (cdr l) elem)))))

(define nth
  (lambda (l n)
    (if (= n 0)
        (car l)
        (nth (cdr l) (- n 1)))))

(define remove-nth
  (lambda (l n)
    (if (= n 0)
        (cdr l)
        (cons (car l) (remove-nth (cdr l) (- n 1))))))

(define shuffle
  (lambda (l seed)
    (if (null? l)
        l
        (let ((n (% (abs (rand-next seed)) (length l))))
          (cons (nth l n) (shuffle (remove-nth l n) n))))))

; Cells

(define cell
  (lambda (r c s)
    (+ (* r s) c)))

(define atE
  (lambda (c s)
    (+ c 1)))

(define atW
  (lambda (c s)
    (- c 1)))

(define atN
  (lambda (c s)
    (- c s)))

(define atS
  (lambda (c s)
    (+ c s)))

; Walls

(define makeWall
  (lambda (c1 c2)
    (+ (* c1 65536) c2)))

(define fstCell
  (lambda (w)
    (/ w 65536)))

(define sndCell
  (lambda (w)
    (% w 65536)))

(define isWallUp
  (lambda (c1 c2 w)
    (contains w (makeWall c1 c2))))


; Create a maze that has walls everywhere
(define completeMaze-acc
  (lambda (r c s acc)
    (if (< r s)
        (if (< c s)
            (let ((rc (cell r c s)))
              (let ((res1 (if (< c (- s 1))
                              (cons (makeWall (cell r c s) (atE rc s)) acc)
                              acc)))
                (let ((res2 (if (< r (- s 1))
                                (cons (makeWall (cell r c s) (atS rc s)) res1)
                                res1)))
                  (completeMaze-acc r (+ c 1) s res2))))
            (completeMaze-acc (+ r 1) 0 s acc))
            acc)))


(define completeMaze
  (lambda (s)
    (completeMaze-acc 0 0 s nil)))

; Create a list of singleton lists for each cell of the maze
(define fullyDisconnectedSets-acc
  (lambda (r c s acc)
    (if (< r s)
        (if (< c s)
            (let ((res (cons (list (cell r c s)) acc)))
              (fullyDisconnectedSets-acc r (+ c 1) s res))
            (fullyDisconnectedSets-acc (+ r 1) 0 s acc))
        acc)))

(define fullyDisconnectedSets
  (lambda (s)
    (fullyDisconnectedSets-acc 0 0 s nil)))

(define connected
  (lambda (sets c1 c2)
    (and (not (= sets nil))
      (let ((set (car sets)))
        (or (and (contains set c1)
                 (contains set c2))
            (connected (cdr sets) c1 c2))))))

(define not
  (lambda (x)
    (if x 0 1)))

; return the first element that satisfies p
(define find
  (lambda (p l)
    (let ((res (filter p l)))
      (if (null? res)
          res
          (car res)))))

(define connect
  (lambda (sets c1 c2)
    (let ((setOfC1 (find (lambda (e) (contains e c1)) sets))
          (setOfC2 (find (lambda (e) (contains e c2)) sets)))
      (cons (append setOfC1 setOfC2)
            (filter (lambda (e)
                      (and (not (contains e c1))
                           (not (contains e c2))))
                    sets)))))

; execute body for each int between from and to
(define for
  (lambda (from to body)
    (if (< from to)
        (let ()
          (body from)
          (for (+ from 1) to body))
        0)))

(define print-maze
  (lambda (s w)
    (let ((space 32)
          (wall 35))
      (print-n-char (+ (* s 2) 1) wall)
      (newline)
      (for 0 s
           (lambda (r)
               (print-char wall)
               (for 0 s
                    (lambda (c)
                        (print-char space)
                        (if (< c (- s 1))
                            (let ((rc (cell r c s)))
                              (print-char (if (isWallUp rc (atE rc s) w) wall space)))
                            0)))
               (print-char wall)
               (newline)
               (if (< r (- s 1))
                   (let ()
                     (print-char wall)
                     (for 0 s
                          (lambda (c)
                            (let ((rc (cell r c s)))
                              (print-char (if (isWallUp rc (atS rc s) w) wall space))
                              (if (< c (- s 1))
                                  (print-char wall)
                                  0))))
                     (print-char wall)
                     (newline))
                   0)))
      (print-n-char (+ (* s 2) 1) wall)
      (newline))))


(define random-maze-acc
  (lambda (m c acc)
    (if (null? m)
        acc
        (let ((w (car m)))
          (if (connected c (fstCell w) (sndCell w))
              (random-maze-acc (cdr m) c (cons w acc))
              (random-maze-acc (cdr m) (connect c (fstCell w) (sndCell w)) acc))))))


(define random-maze
  (lambda (s seed)
    (let ((m (shuffle (completeMaze s) seed))
          (c (fullyDisconnectedSets s)))
      (random-maze-acc m c nil))))


(print-string "Size: ") ; T
(let ((size 5))
; (let ((size (read-int)))
  (print-string "Seed: ") ; G
  (let ((seed 5))
  ; (let ((seed (read-int)))
    (print-maze size (random-maze size seed))))