;; 1.3.1 Procedures as Arguments

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Exercise 1.29

(define (simpson-integral f a b n)
  (if (> (remainder n 2) 0) (error "n must be even"))
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term k)
    (cond ((= 0 k) (y k))
          ((even? k) (* 4 (y k)))
          ((odd? k) (* 2 (y k)))))
  (* (/ h 3)
     (sum term 0 1+ n)))

;; Exercise 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (product-recursive term a next b)
  (if (> a b) 1
      (* (term a) (product-recursive term (next a) next b))))

(define (factorial n)
  (product-recursive identity 1 1+ n))

(define (wallis-approx precision)
  (define (term n)
    (* (/ (* 2 n) (- (* 2 n) 1))
       (/ (* 2 n) (+ (* 2 n) 1))))
  (* 2 (product term 1 1+ precision)))

;; 1.32

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (accumulate-recursive combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recursive
                 combiner
                 null-value
                 term
                 (next a)
                 next
                 b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate-recursive * 1 term a next b))

;; 1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

(define (prime? n)
  (define (divisible-by-any? n factors)
    (cond ((null? factors) #f)
          ((= 0 (remainder n (car factors))) #t)
          (else (divisible-by-any? n (cdr factors)))))
  (define (iter i factors)
    (cond ((= i n) #t)
          ((divisible-by-any? i factors) (iter (1+ i) factors))
          ((= (remainder n i) 0) #f)
          (else (iter (1+ i) (append factors (list i))))))
  (if (< n 2) #f (iter 2 '())))

(define (square x) (* x x))

(define (sum-squares-primes a b)
  (filtered-accumulate prime? + 0 square a 1+ b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relatively-prime? i n)
  (= 1 (gcd i n)))

(define (product-relatively-prime n)
  (define (filter i) (relatively-prime? i n))
  (filtered-accumulate filter * 1 identity 1 1+ n))


;; 1.3.3 Procedures as General Methods

;; Finding roots of equations by the half-interval method

(define (average . items)
  (/ (apply + items) (length items)))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint
         (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond
           ((positive? test-value)
            (search f neg-point midpoint))
           ((negative? test-value)
            (search f midpoint pos-point))
           (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
           (search f a b))
          ((and (negative? b-value)
                (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of
                   opposite sign" a b)))))

;; Finding fixed points of functions

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  ;; (define count 0) ; Exercise 1.36
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    ;; (set! count (1+ count)) ; Exercise 1.36
    ;; (format #t "~a\n" guess) ; Exercise 1.36
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try (average guess next)))))
  ;; (cons ; Exercise 1.36
   (try first-guess)
        ;; count) ; Exercise 1.36
  )

(define (sqrt x)
  (fixed-point
   (lambda (y) (average y (/ x y)))
   1.0))

;; Exercise 1.35

;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0); -> 1.618...

;; Exercise 1.36

;(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

;; Exercise 1.37

(define (cont-frac f n d k)
  ;; (define (recur n d i)
  ;;   (if (= i k)
  ;;       0
  ;;       (+ (/ (n i) (+ (d i) (recur n d (1+ i)))))))
  ;; (recur n d 0)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (f (d i) acc)))))
  (iter k 0))

(define (phi-approx k)
  (/ 1
     (cont-frac + (lambda (i) 1.0) (lambda (i) 1.0) k)))

;; Exercise 1.38

(define (e-approx k)
  (+ 2
     (cont-frac +
                (lambda (x) 1.0)
                (lambda (x)
                  (if (= 2 (remainder x 3))
                      (* 2 (+ 1 (floor-quotient x 3)))
                      1))
                k)))

;; Exercise 1.39

(define (tan-cf x k)
  (exact->inexact
   (cont-frac -
              (lambda (i) (if (= 1 i) x (square x)))
              (lambda (i) (+ 1 (* 2 (- i 1))))
              k)))

;; 1.3.4

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (sqrt x)
  (fixed-point
   (average-damp
    (lambda (y) (/ x y)))
   1.0))

(define (cube-root x)
  (fixed-point
   (average-damp
    (lambda (y)
      (/ x (square y))))
   1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (sqrt x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (/ x y))
   average-damp
   1.0))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

;; Exercise 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;; Exercise 1.41

(define (double f)
  (lambda (x)
    (f (f x))))

;; Exercise 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

;; Exercise 1.43

(define (repeated f n)
  (lambda (x)
    (define (iter acc i)
      (if (= i n)
          acc
          (iter (f acc) (inc i))))
    (iter x 0)))

(define (repeated f n)
  (define (iter g i)
    (if (= i n)
        g
        (iter (compose g f) (inc i))))
  (iter f 1))

;; Exercise 1.44

(define (smooth f)
  (lambda (x)
    (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;; Exercise 1.45

(define (nroot-rdamp n r)
  (lambda (x)
    (fixed-point
     ((repeated average-damp r)
      (lambda (y) (/ x (expt y (- n 1)))))
     1.0)))

;; Was not able to solve this because the expected behavior of this function
;; appears to be different than the actual behavior. It is expected that it will
;; "fail to converge" (enter an infinite loop) quickly as n is increased, and r
;; will need to be increased to correct this failure. This failure to converge
;; does not happen with inputs small enough to reasonably analyze with manual
;; testing. I believe some difference between 2023 Guile Scheme and 1996 MIT
;; Scheme causes this function to behave very differently.
;;
;; The expected behavior is to need have a logarithmic relationship between n
;; and r (double n, increment r) to guarantee that the function converges. My
;; original approach to this was to use the above function and manually change
;; n and r to find a simple relationship between the two variables. This would
;; have worked fine with an implementation of Scheme that behaved similarly to
;; MIT Scheme.

;; Exercise 1.46

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt x)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- (square guess) x)) 0.001))
    (lambda (guess)
      (average guess (/ x guess))))
   1.0))

(define (fixed-point f)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- guess (f guess))) .00001))
    (lambda (guess)
      (f guess)))
   1.0))
