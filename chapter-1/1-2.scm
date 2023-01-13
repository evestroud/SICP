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
