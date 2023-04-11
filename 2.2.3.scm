;; 2.2.3 Sequences as Conventional Interfaces

(define-module (chapter-2-2-3)
  #:export (flatmap)
  #:export (accumulate))

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
