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
