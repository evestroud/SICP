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
