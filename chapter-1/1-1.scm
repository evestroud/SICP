;; 1.1.7 Example: Square Roots by Newton’s Method

(define (sqrt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess x)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average . items)
  (/ (apply + items) (length items)))

; Exercise 1.7

;; (define (good-enough? guess prev-guess x)
;;   (< (abs (- (* guess guess) x)) .001))

(define (good-enough? guess prev-guess x)
   (< (/ (abs (- guess prev-guess)) guess) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 (inf) x)) ; 2.0 to force a float


;; Exercise 1.6
(define (new-if predicate
                then-clause
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))


;; Exercise 1.8

(define (cube-rt-iter guess prev-guess x)
  (if (good-enough? guess prev-guess x)
      guess
      (cube-rt-iter (improve-cube-rt guess x) guess x)))

(define (improve-cube-rt guess x)
  (/
   (+ (/ x (* guess guess)) (* 2 guess))
   3))

(define (good-enough? guess prev-guess x)
   (< (/ (abs (- guess prev-guess)) guess) 0.001))

(define (cube-rt x)
  (cube-rt-iter 1.0 (inf) x)) ; 2.0 to force a float
