;; 2.1.1 Arithmetic Operations for Rational Numbers

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g)
          (/ d g))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))


;; Exercise 2.1

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g)) (d (/ d g)))
      (if (> d 0)
          (cons n d)
          (cons (* -1 n) (* -1 d))))))


;; Exercise 2.2

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (make-point
   (/ (+ (x-point (start-segment s))
         (x-point (end-segment s)))
      2)
   (/ (+ (y-point (start-segment s))
         (y-point (end-segment s)))
      2)))

(define (print-point p)
  (display (format #f "(~a,~a)\n" (x-point p) (y-point p))))


;; Exercise 2.3
;; ... straight up don't know enough geometry to do this without spending
;; a bunch of time and looking everything up. I get the purpose of the
;; exercise and don't feel the need to put myself through that.
;; The reliance on recent and intuitive familiarity with mathematical
;; concepts all the way up to calculus is a major weakness of this text.
;; Can you not really demonstrate these concepts more intuitively?
;; Mathematics is notoriously arcane and reliant on obsession or talent.

(define (make-rectangle h w origin angle)
  (cons (cons h w) (cons origin angle)))

(define (rectangle-height r)
  (car (car r)))

(define (rectangle-width r)
  (cdr (car r)))

(define (rect-perimeter r)
  (+ (* 2 (rect-height r))
     (* 2 (rect-width r))))

(define (rect-area r)
  (* (rect-height r) (rect-width r)))


;; Exercise 2.4

(define (cons-proc x y)
  (lambda (m) (m x y)))

(define (car-proc z)
  (z (lambda (p q) p)))

(define (cdr-proc z)
  (z (lambda (p q) q)))


;; Exercise 2.5

(define (cons-exp x y)
  (* (expt 2 x) (expt 3 y)))

(define (car-exp z)
  (if (= (remainder z 2) 1)
      0
      (1+ (car-exp (/ z 2)))))

(define (cdr-exp z)
  (if (= (remainder z 3) 1)
      0
      (1+ (car-exp (/ z 3)))))


;; Exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

;; http://community.schemewiki.org/?sicp-ex-2.6
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; Exercise 2.7

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (car interval))

(define (lower-bound interval)
  (cdr interval))
