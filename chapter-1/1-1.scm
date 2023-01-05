;; 1.1.7 Example: Square Roots by Newton’s Method

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average . items)
  (/ (apply + items) (length items)))

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) .000001))

(define (sqrt x)
  (sqrt-iter (average 1 (/ x 2.0)) x)) ; 2.0 to force a float
