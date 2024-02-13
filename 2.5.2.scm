;; Operator and type table
(define operator-and-type-table (make-hash-table))

(define (get op type)
  (let ((type-table-for-op (hash-ref operator-and-type-table op)))
    (if type-table-for-op
        (hash-ref
         type-table-for-op
         type)
        #f)))

(define (put op type value)
  (let ((type-table (hash-ref operator-and-type-table op (make-hash-table))))
    (hash-set!
     type-table
     type
     value)
    (hash-set! operator-and-type-table op type-table)))

;; Coercion table
(define coercion-table (make-hash-table))

(define (get-coercion type1 type2)
  (if (eq? type1 type2)
      #f
      (let ((coercions-for-type (hash-ref coercion-table type1)))
        (if coercions-for-type
            (hash-ref
             coercions-for-type
             type2)
            #f))))

(define (put-coercion type1 type2 value)
  (let ((type-table (hash-ref coercion-table type1 (make-hash-table))))
    (hash-set!
     type-table
     type2
     value)
    (hash-set! coercion-table type1 type-table)))

;; Generic Functions
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum:
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum:
              CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types:
             APPLY-GENERIC"
            (list op type-tags))))))

(define (add . args) (apply apply-generic (cons 'add args)))
(define (sub . args) (apply apply-generic (cons 'sub args)))
(define (mul . args) (apply apply-generic (cons 'mul args)))
(define (div . args) (apply apply-generic (cons 'div args)))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? n) (apply-generic '=zero? n))

;; Number packages
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(integer integer)
       (lambda (x y) (= x y)))
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  'done)

(install-integer-package)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
  (define (equ?-rat x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero?-rat x)
    (= 0 (numer x)))
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
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero?-rat x)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
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
    (define (equ?-rectangular x y)
      (and (equ? (real-part x) (real-part y))
           (equ? (imag-part x) (imag-part y))))
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
    (put 'equ? '(rectangular rectangular) equ?-rectangular)
    (put '=zero? '(rectangular) =zero?-rectangular)
    'done)
  (install-rectangular-package)

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
    (define (equ?-polar x y)
      (and (equ? (magnitude x) (magnitude y))
           (equ? (angle x) (angle y))))
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
    (put 'equ? '(polar polar) equ?-polar)
    (put '=zero? '(polar) =zero?-polar)
    'done)
  (install-polar-package)

  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
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
  (define (equ?-complex x y)
    (equ? x y))
  (define (=zero?-complex n)
    (=zero? n))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
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
  (put 'equ? '(complex complex) equ?-complex)
  (put '=zero? '(complex) =zero?-complex)
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag
        'rectangular)
   x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang
        'polar)
   r a))
(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))
