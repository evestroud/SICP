;; 2.2 Hierarchical Data and the Closure Property

;; Exercise 2.17

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))


;; Exercise 2.18

(define (reverse l)
  (define (helper l r)
    (if (null? l)
        r
        (helper (cdr l)
                (cons (car l) r))))
  (helper l '()))


;; Exercise 2.19

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (null? kinds-of-coins))
         0)
        (else
         (+ (cc amount (cdr kinds-of-coins))
            (cc (- amount (car kinds-of-coins))
                kinds-of-coins)))))

(define us-coins
  (list 50 25 10 5 1))

(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))


;; Exercise 2.20

(define (same-parity . ints)
  (display ints)
  (newline)
  (let ((parity (remainder (car ints) 2)))
    (define (helper ints)
      (cond
       ((null? ints) '())
       ((= parity (remainder (car ints) 2))
        (cons (car ints) (helper (cdr ints))))
       (else (helper (cdr ints)))))
    (cons (car ints) (helper (cdr ints)))))


;; Exercise 2.21

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))


;; Exercise 2.23

(define (for-each f l)
  (if (null? l)
      '()
      (begin (f (car l))
             (for-each f (cdr l)))))


;; 2.2.2 Hierarchical Structure

;; Exercise 2.27

(define (deep-reverse l)
  (define (helper l r)
    (cond
     ((null? l) r)
     ((pair? (car l))
      (helper (cdr l) (cons (deep-reverse (car l)) r)))
     (else (helper (cdr l) (cons (car l) r)))))
  (helper l '()))


;; Exercise 2.28

(define (fringe l)
  (cond
   ((null? l) l)
   ((pair? (car l))
    (append (fringe (car l))
            (fringe (cdr l))))
   (else (cons (car l) (fringe (cdr l))))))


;; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; 1

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define a (make-mobile (make-branch 2 3) (make-branch 2 3)))
(define b (make-mobile (make-branch 2 3) (make-branch 4 5)))
(define c (make-mobile (make-branch 5 a) (make-branch 3 b)))

;; 2

(define (mobile? structure)
  (pair? structure))

(define (total-weight mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (+ (branch-weight left) (branch-weight right))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (mobile? structure)
        (total-weight structure)
        structure)))

;; 3

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced? mobile)
  (let ((left-torque (torque (left-branch mobile)))
        (right-torque (torque (right-branch mobile))))
    (= left-torque right-torque)))

;; 4

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; No change needed to selectors


;; Exercise 2.30

(define (square-tree t)
  (cond
   ((null? t) '())
   ((pair? (car t))
    (cons (square-tree (car t))
          (square-tree (cdr t))))
   (else (cons (square (car t)) (square-tree (cdr t))))))

(define (square-tree tree)
  (map (lambda (t)
         (if (pair? t)
             (square-tree t)
             (square t)))
       tree))


;; Exercise 2.31

(define (tree-map fn tree)
  (map (lambda (t)
         (if (pair? t)
             (tree-map fn t)
             (fn t)))
       tree))


;; Exercise 2.32 - had to look this one up

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x)
                       (cons (car s) x))
                     rest)))))


;; 2.2.3 Sequences as Conventional Interfaces

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else  (filter predicate
                       (cdr sequence)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))


;; Exercise 2.33

;; (define (map p sequence)
;;   (accumulate (lambda (x y) (cons (p x) y))
;;               '() sequence))

;; (define (append seq1 seq2)
;;   (accumulate cons seq2 seq1))

;; (define (length sequence)
;;   (accumulate (lambda (x y) (1+ y))
;;               0 sequence))


;; Exercise 2.34

(define
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))


;; Exercise 2.35

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (cond
                      ((null? x) 0)
                      ((not (pair? x)) 1)
                      (else (count-leaves x))))
                   (list (car t) (cdr t)))))


;; Exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


;; Exercise 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mi)
         (dot-product mi v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi)
           (matrix-*-vector n mi)) m)))


;; Exercise 2.38 (provided code only, answer in notes)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))


;; Exercise 2.39

(define (reverse sequence)
  (fold-right
   (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left
   (lambda (x y) (cons y x)) nil sequence))


;; Nested Mappings

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (flatmap
         (lambda (i)
           (map (lambda (j)
                  (list i j))
                (enumerate-interval
                 1
                 (- i 1))))
         (enumerate-interval 1 n)))))


;; Exercise 2.40

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
        prime-sum?
        (unique-pairs n))))


;; Exercise 2.41

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
            (map (lambda (k)
                   (list k j i))
                 (enumerate-interval 1 (- j 1))))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (sums-to-s? s l)
  (= s (apply + l)))

(define (sum-to-s-triples s n)
  (filter
   (lambda (l) (sums-to-s? s l))
   (unique-triples n)))


;; Exercise 2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (safe? k positions)
  (define (safe-iter i top mid bot)
    (if (= i 0) #t
    (let ((i-position (list-ref positions (- i 1))))
      (cond
       ((or (= top i-position)
            (= mid i-position)
            (= bot i-position))
        #f)
       (else (safe-iter (- i 1)
                        (+ top 1)
                        mid
                        (- bot 1)))))))
  (let ((k-position (list-ref positions (- k 1))))
    (safe-iter (- k 1)
               (+ k-position 1)
               k-position
               (- k-position 1))))

(define (adjoin-position new-row k rest-of-queens)
  (if (= k 1)
      (cons new-row rest-of-queens)
      (cons (car rest-of-queens)
            (adjoin-position
             new-row
             (- k 1)
             (cdr rest-of-queens)))))


;; 2.2.4 Example: A Picture Language

(use-modules (cairo) (srfi srfi-9) (srfi srfi-9 gnu))