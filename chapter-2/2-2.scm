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
                (append (list (car l)) r))))
  (helper l '()))
