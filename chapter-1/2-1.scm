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

(define (orthagonal? p1 p2 p3))

(define (rect-perimeter r)
  (+ (* 2 (rect-height r))
     (* 2 (rect-width r))))

(define (rect-area r)
  (* (rect-height r) (rect-width r)))
