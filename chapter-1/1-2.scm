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


;; Exercise 1.11

(define (f n)
  (if (< n 3) n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f-iter n)
  (define (iter c p pp i)
    (cond
     ((= i n) c)
     ((< i 2) (iter (1+ c) c p (1+ i)))
     (else (iter (+ c (* 2 p) (* 3 pp))
                 c
                 p
                 (1+ i)))))
  (iter 0 0 0 0))


;; Exercise 1.12

(define (pascal row col)
  (cond
   ((> col row) (error "illegal input: col must be <= row"))
   ((or (= col 0) (= col row)) 1)
   (else (+ (pascal (- row 1) (- col 1))
            (pascal (- row 1) col)))))

(define (pascal-row row)
  (format #t "~a\n"
          (map (lambda (col) (pascal row col)) (iota (1+ row)))))

(define (pascal-triangle size)
  (for-each pascal-row (iota size)))

;; Exercise 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

(define (counter fn)
  (define count 0)
  (define (wrapped x)
    (set! count (1+ count))
    (fn x))
  (define (get-count) count)
  (list (cons "fn" wrapped) (cons "get" get-count)))

;; 1.
;; (define p-counter (counter p))
;; (define p (assoc-ref p-counter "fn"))
;; (define p-counter (assoc-ref p-counter "get"))


;; 1.2.4 Exponentiation

(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt-rec b (- n 1)))))

(define (expt-iter b n)
  (define (iter i total)
    (if (= i n)
        total
        (iter (1+ i) (* b total))))
  (iter 0 1))

(define (expt-fast b n)
  (cond
   ((= n 0) 1)
   ((even? n) (square (expt-fast b (/ n 2))))
   (else (* b (expt-fast b (- n 1))))))

(define (square n)
  (* n n))

;; Exercise 1.16

(define (expt-fast-iter b n)
  (define (iter a b n)
    (cond
     ((= n 0) a)
     ((even? n) (iter a (square b) (/ n 2)))
     (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

;; Exercise 1.17

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (mult-fast a b)
  (cond
   ((= 1 a) b)
   ((even? a) (double (mult-fast (halve a) b)))
   (else (+ b (mult-fast (- a 1) b)))))

;; Exercise 1.18

(define (mult-fast-iter a b)
  (define (iter a b c)
    (cond
     ((= 0 a) c)
     ((even? a) (iter (halve a) (double b) c))
     (else (iter (- a 1) b (+ b c)))))
  (iter a b 0))

;; Exercise 1.19

(define (fib n)
  (define (iter a b p q count)
    (cond ((= count 0)
           b)
          ((even? count)
           (iter a
                 b
                 (+ (square p) (square q))
                 (+ (* 2 p q) (square q))
                 (/ count 2)))
          (else
           (iter (+ (* b q)
                    (* a q)
                    (* a p))
                 (+ (* b p)
                    (* a q))
                 p
                 q
                 (- count 1)))))
  (iter 1 0 0 1 n))


;; 1.2.5 Greatest Common Divisors

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;; 1.2.6 Testing for Primality

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n)
           n)
          ((divides? test-divisor n)
           test-divisor)
          (else (find-divisor
                 n
                 ; below modified for 1.23
                 (if (= test-divisor 2) 3 (+ test-divisor 2))))))
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    ;; (= (expmod-2 a n n) a)) ; Exercise 1.25
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else #f)))

;; Exercise 1.22

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n) ; modified for Exercise 1.24
      (format #t "~a - ~a\n" n (- (runtime) start-time))))

(define (runtime)
  (exact->inexact
   (let ((time (gettimeofday)))
     (+ (car time) (/ (cdr time) 1000000)))))

(define (search-for-primes start end)
  (let ((start (if (even? start) (1+ start) start)))
    (for-each timed-prime-test (iota (floor-quotient (- end start) 2) start 2))))

;; Exercise 1.25

(define (expmod-2 base exp m)
  (remainder (expt-fast base exp) m))

;; Exercise 1.27

(define (fermat-comprehensive n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (test-all i)
    (cond
     ((not (try-it i)) #f)
     ((<= n (square i))  #t)
     (else (test-all (1+ i)))))
  (if (= n 2)
      #t
      (test-all 2)))

(define (debug x)
  (display x)
  (newline)
  x)

;; Exercise 1.28


(define (miller-rabin n)
  (define (nontrivial? x m)
    (if (and (not (= 1 x)) (not (= (- m 1) x))
             (= (remainder (square x) m) 1))
        0
        x))
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder
            (square
             (nontrivial? (expmod base (/ exp 2) m) m))
            m))
          (else
           (remainder
            (* base (expmod base (- exp 1) m))
            m))))
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (define (test times)
    (cond ((= times 0) #t)
          ((try-it (+ 1 (random (- n 1))))
           (test (- times 1)))
          (else #f)))
  (test 10))
