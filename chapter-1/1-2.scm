;; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))


;; 1.2.2 Tree Recursion

(define (fib-iter n)
  (define (iter c p i)
    (if (= i n) c
        (iter (+ c p) c (1+ i))))
  (if (= n 0) 1
      (iter 1 0 0)))

(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((< amount 0) 0)
          ((= kinds-of-coins 0) 0)
          (else
           (+ (cc amount (- kinds-of-coins 1))
              (cc (- amount (denomination kinds-of-coins))
                  kinds-of-coins)))))
  (define (denomination kind-of-coin)
    (cond ((= kind-of-coin 1) 1)
          ((= kind-of-coin 2) 5)
          ((= kind-of-coin 3) 10)
          ((= kind-of-coin 4) 25)
          ((= kind-of-coin 5) 50)))
  (cc amount 5))
