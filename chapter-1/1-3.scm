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

(define (cont-frac n d k)
  ;; (define (recur n d i)
  ;;   (if (= i k)
  ;;       0
  ;;       (+ (/ (n i) (+ (d i) (recur n d (1+ i)))))))
  ;; (recur n d 0)
  (define (iter i acc)
    (if (= i 0)
        acc
        (iter (- i 1) (/ (n i) (+ (d i) acc)))))
  (iter k 0))

(define (phi-approx k)
  (/ 1
     (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))

;; Exercise 1.38

(define (e-approx k)
  (+ 2
     (cont-frac (lambda (x) 1.0)
                (lambda (x)
                  (if (= 2 (remainder x 3))
                      (* 2 (+ 1 (floor-quotient x 3)))
                      1))
                k)))
