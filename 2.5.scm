;; Tag operations
                                        ; Exercise 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum:
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum:
              CONTENTS" datum))))


;; Generic operations
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types:
             APPLY-GENERIC"
           (list op type-tags))))))


;; Generic Arithmetic
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
                                        ; Exercise 2.79
(define (equ? x y) (apply-generic 'equ? x y))
                                        ; Exercise 2.80
(define (=zero? n) (apply-generic '=zero? n))


;; Operations and types table
(define operator-and-type-table (make-hash-table))
(define (get op type)
  (hash-ref
   (hash-ref operator-and-type-table op)
   type))
(define (put op type value)
  (let ((type-table (hash-ref operator-and-type-table op (make-hash-table))))
    (hash-set!
     type-table
     type
     value)
    (hash-set! operator-and-type-table op type-table)))

;; Setting up arithmetic packages
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
                                        ; Exercise 2.79
  (put 'equ? '(scheme-number scheme-number) =)
                                        ; Exercise 2.80
  (put '=zero? '(scheme-number) (lambda (n) (= n 0)))
  'done)

(install-scheme-number-package)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (=zero? d)
        (error "Cannot make a rational number with a denominator of 0")
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))))
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
                                        ; Exercise 2.79
  (define (equ?-rat x y)
    (and (= (numer x) (numer y))
         (= (denom y) (denom y))))
                                        ; Exercise 2.80
  (define (=zero?-rat n)
    (= 0 (numer n)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
                                        ; Exercise 2.79
  (put 'equ? '(rational rational) equ?-rat)
                                        ; Exercise 2.80
  (put '=zero? '(rational) =zero?-rat)
  'done)

(install-rational-package)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (install-rectangular-package)
    ;; internal procedures
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y)
      (cons x y))
    (define (magnitude z)
      (sqrt (+ (expt (real-part z) 2)
               (expt (imag-part z) 2))))
    (define (angle z)
      (atan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
      (cons (* r (cos a)) (* r (sin a))))
                                        ; Exercise 2.79
    (define (equ?-rectangular x y)
      (and (= (real-part x) (real-part y))
           (= (imag-part x) (imag-part y))))
                                        ; Exercise 2.80
    (define (=zero?-rectangular n)
      (and (= 0 (real-part n))
           (= 0 (imag-part n))))
    ;; interface to the rest of the system
    (define (tag x)
      (attach-tag 'rectangular x))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angle '(rectangular) angle)
    (put 'make-from-real-imag 'rectangular
         (lambda (x y)
           (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'rectangular
         (lambda (r a)
           (tag (make-from-mag-ang r a))))
                                        ; Exercise 2.79
    (put 'equ? '(rectangular rectangular) equ?-rectangular)
                                        ; Exercise 2.80
    (put '=zero? '(rectangular) =zero?-rectangular)
    'done)

  (define (install-polar-package)
    ;; internal procedures
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z)
      (* (magnitude z) (cos (angle z))))
    (define (imag-part z)
      (* (magnitude z) (sin (angle z))))
    (define (make-from-real-imag x y)
      (cons (sqrt (+ (expt x 2) (expt y 2)))
            (atan y x)))
                                        ; Exercise 2.79
    (define (equ?-polar x y)
      (and (= (magnitude x) (magnitude y))
           (= (angle x) (angle y))))
                                        ; Exercise 2.80
    (define (=zero?-polar n)
      (= 0 (magnitude n)))
    ;; interface to the rest of the system
    (define (tag x) (attach-tag 'polar x))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angle '(polar) angle)
    (put 'make-from-real-imag 'polar
         (lambda (x y)
           (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'polar
         (lambda (r a)
           (tag (make-from-mag-ang r a))))
                                        ; Exercise 2.79
    (put 'equ? '(polar polar) equ?-polar)
                                        ; Exercise 2.80
    (put '=zero? '(polar) =zero?-polar)
    'done)

  (install-rectangular-package)
  (install-polar-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
  (define (real-part n)
    ((get 'real-part (list (type-tag n))) (contents n)))
  (define (imag-part n)
    ((get 'imag-part (list (type-tag n))) (contents n)))
  (define (magnitude n)
    ((get 'magnitude (list (type-tag n))) (contents n)))
  (define (angle n)
    ((get 'angle (list (type-tag n))) (contents n)))
                                        ; Exercise 2.79
  (define (equ?-complex x y)
    (equ? x y))
                                        ; Exercise 2.80
  (define (=zero?-complex n)
    (=zero? n))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
                                        ; Exercise 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
                                        ; Exercise 2.79
  (put 'equ? '(complex complex) equ?-complex)
                                        ; Exercise 2.80
  (put '=zero? '(complex) =zero?-complex)
  'done)

(install-complex-package)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
                                        ; Exercise 2.77
(define (real-part n)
  ((get 'real-part (list (type-tag n))) (contents n)))
(define (imag-part n)
  ((get 'imag-part (list (type-tag n))) (contents n)))
(define (magnitude n)
  ((get 'magnitude (list (type-tag n))) (contents n)))
(define (angle n)
  ((get 'angle (list (type-tag n))) (contents n)))


;; Coercion

; Coercion table
(define coercion-table (make-hash-table))
(define (get-coercion type1 type2)
  (hash-ref
   (hash-ref coercion-table type2)
   type1))
(define (put-coercion type1 type2 value)
  (let ((type-table (hash-ref coercion-table type1 (make-hash-table))))
    (hash-set!
     type-table
     type2
     value)
    (hash-set! coercion-table type1 type-table)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2
                       (get-coercion type1
                                     type2))
                      (t2->t1
                       (get-coercion type2
                                     type1)))
                  (cond (t1->t2
                         (apply-generic
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic
                          op a1 (t2->t1 a2)))
                        (else
                         (error
                          "No method for
                           these types"
                          (list
                           op
                           type-tags))))))
              (error
               "No method for these types"
               (list op type-tags)))))))

; Coercion operations

(define (scheme-number->complex n)
  (make-complex-from-real-imag
   (contents n) 0))

(put-coercion 'scheme-number 'complex
              scheme-number->complex)
