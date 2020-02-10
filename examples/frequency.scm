;;; taken from https://www.scheme.com/tspl4/examples.html

(define get-char read-char)
(define put-char (lambda (p c) (write-char c p)))
(define put-datum (lambda (p c) (write c p)))
(define put-string (lambda (p s) (write s p)))

 ;;; If the next character on p is a letter, get-word reads a word
;;; from p and returns it in a string.  If the character is not a
;;; letter, get-word returns the character (on eof, the eof-object).
(define get-word
  (lambda (p)
    (let ([c (get-char p)])
      (if (eq? (char-type c) 'letter)
          (list->string
            (let loop ([c c])
              (cons
                c
                (if (memq (char-type (peek-char p))
                          '(letter digit))
                    (loop (get-char p))
                    '()))))
          c))))

;;; char-type tests for the eof-object first, since the eof-object
;;; may not be a valid argument to char-alphabetic? or char-numeric?
;;; It returns the eof-object, the symbol letter, the symbol digit,
;;; or the argument itself if it is not a letter or digit.
(define char-type
  (lambda (c)
    (cond
      [(eof-object? c) c]
      [(char-alphabetic? c) 'letter]
      [(char-numeric? c) 'digit]
      [else c])))

;;; Tree nodes are represented as a record type with four fields: word,
;;; left, right, and count.  Only one field, word, is initialized by an
;;; argument to the constructor procedure make-tnode.  The remaining
;;; fields are initialized by the constructor and changed by subsequent
;;; operations.
(define make-tnode
    (lambda (word)
        (let ([tnode (make-vector 4)])
            (vector-set! tnode 0 word)
            (vector-set! tnode 1 '())
            (vector-set! tnode 2 '())
            (vector-set! tnode 3 1)
            tnode)))

(define (tnode-word t) (vector-ref t 0))
(define (tnode-left t) (vector-ref t 1))
(define (tnode-right t) (vector-ref t 2))
(define (tnode-count t) (vector-ref t 3))

(define (tnode-left-set! t v) (vector-set! t 1 v))
(define (tnode-right-set! t v) (vector-set! t 2 v))
(define (tnode-count-set! t v) (vector-set! t 3 v))


;;; If the word already exists in the tree, tree increments its
;;; count.  Otherwise, a new tree node is created and put into the
;;; tree.  In any case, the new or modified tree is returned.
(define tree
  (lambda (node word)

    (display "tree")
    (newline)
    (cond
      [(null? node) (make-tnode word)]
      [(string=? word (tnode-word node))
       (tnode-count-set! node (+ (tnode-count node) 1))
       node]
      [(string<? word (tnode-word node))
       (tnode-left-set! node (tree (tnode-left node) word))
       node]
      [else
       (tnode-right-set! node (tree (tnode-right node) word))
       node])))

;;; tree-print prints the tree in "in-order," i.e., left subtree,
;;; then node, then right subtree.  For each word, the count and the
;;; word are printed on a single line.
(define tree-print
  (lambda (node p)
    (display "tree-print")
    (newline)
    (unless (null? node)
      (tree-print (tnode-left node) p)
      (put-datum p (tnode-count node))
      (put-char p (number->char 32))
      (put-string p (tnode-word node))
      (newline p)
      (tree-print (tnode-right node) p))))

;;; frequency is the driver routine.  It opens the files, reads the
;;; words, and enters them into the tree.  When the input port
;;; reaches end-of-file, it prints the tree and closes the ports.
(define frequency
  (lambda (infn outfn)
    (let ([ip (open-input-file infn)]
          [op (open-output-file ofn)])
      (let loop ([root '()])
        (let ([w (get-word ip)])
          (cond
            [(eof-object? w) (tree-print root op)]
            [(string? w) (loop (tree root w))]
            [else (loop root)])))
      (close-port ip)
      (close-port op))))

(define ifn "misc/input.txt")
(define ofn "misc/output.txt")

(frequency ifn ofn)