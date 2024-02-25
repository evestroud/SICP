(use-modules (srfi srfi-1))

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

(define tower
  '(complex
    real
    rational
    integer))

(define (get-height type)
  (list-index
   (lambda (elem) (eq? elem type))
   tower))

(define (raise n)
  (let ((coerce (get 'raise (type-tag n))))
    (if coerce
        (coerce n)
        #f)))

(define (raise-to-height value target)
  (let* ((type (type-tag value))
         (height (get-height type)))
    (cond
     ((= height target) value)
     ((> target height) (error "Invalid raise" (list height target)))
     (else (raise-to-height (raise value) target)))))

(define (drop x)
  (let ((proc (get 'project (type-tag x))))
    (if proc
        (let ((projected (proc (contents x))))
          (if (equ? x (raise projected))
              (drop projected)
              x))
        x)))

(define (apply-unary op x)
  (let* ((type (type-tag x))
         (proc (get op (list type)))
         (value (contents x)))
    (proc value)))

(define (apply-binary op x y)
  (let* ((x-type (type-tag x))
         (y-type (type-tag y))
         (proc (get op (list x-type y-type))))
    (if proc
        (apply proc (map contents (list x y)))
        (let* ((x-height (get-height x-type))
               (y-height (get-height y-type)))
          (cond
           ((< x-height y-height)
            (let ((raised-y (raise-to-height y x-height)))
              (apply-binary op x raised-y)))
           ((> x-height y-height)
            (let ((raised-x (raise-to-height x y-height)))
              (apply-binary op raised-x y)))
           (else (error "APPLY-BINARY: No method for these types" (list op x y))))))))

(define (apply-generic op . args)
  (let ((result
         (if (= (length args) 1)
             (apply-unary op (car args))
             (fold (lambda (arg acc)
                     (apply-binary op arg acc))
                   (car args)
                   (cdr args)))))
    (if (not (boolean? result))
        (drop result)
        result)))

(define (add . args) (apply apply-generic (cons 'add args)))
(define (sub . args) (apply apply-generic (cons 'sub args)))
(define (mul . args) (apply apply-generic (cons 'mul args)))
(define (div . args) (apply apply-generic (cons 'div args)))
(define (exp x y) (apply apply-generic (list 'exp x y)))
(define (square-root x) (apply-generic 'square-root x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arc-tan x y) (apply-generic 'arc-tan x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? n) (apply-generic '=zero? n))

;; Number packages
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (y x) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (y x) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (y x) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (y x) (tag (/ x y))))
  (put 'exp '(integer integer)
       (lambda (y x) (tag (expt x y))))
  (put 'square-root '(integer)
       (lambda (x) (make-real (sqrt x))))
  (put 'sine '(integer)
       (lambda (x) (make-real (sin x))))
  (put 'cosine '(integer)
       (lambda (x) (make-real (cos x))))
  (put 'arc-tan '(integer integer)
       (lambda (y x) (make-real (atan x y))))
  (put 'equ? '(integer integer)
       (lambda (y x) (= x y)))
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'make 'integer
       (lambda (x) (tag x)))
  (put-coercion 'integer 'rational
                (lambda (n) (make-rational (contents n) 1)))
  (put 'raise 'integer
       (lambda (x)
         ((get-coercion 'integer 'rational) x)))
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
  (define (add-rat y x)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat y x)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat y x)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat y x)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (exp-rat y x)
    (let ((exponent (/ (numer y) (denom y))))
      (make-rat (expt (numer x) exponent)
                (expt (denom x) exponent))))
  (define (square-root-rat x)
    (sqrt (/ (numer x) (denom x))))
  (define (sine-rat x)
    (sin (/ (numer x) (denom x))))
  (define (cosine-rat x)
    (cos (/ (numer x) (denom x))))
  (define (arc-tan-rat y x)
    (atan (/ (numer x) (denom x)) (/ (numer y) (denom y))))
  (define (equ?-rat y x)
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
  (put 'exp '(rational rational)
       (lambda (x y) (tag (exp-rat x y))))
  (put 'square-root '(rational)
       (lambda (x) (make-real (square-root-rat x))))
  (put 'sine '(rational)
       (lambda (x) (make-real (sine-rat x))))
  (put 'cosine '(rational)
       (lambda (x) (make-real (cosine-rat x))))
  (put 'arc-tan '(rational rational)
       (lambda (x y) (make-real (arc-tan-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))
  (put '=zero? '(rational)
       (lambda (x) (=zero?-rat x)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put-coercion 'rational 'real
                (lambda (x)
                  (let ((n (contents x)))
                    (exact->inexact (/ (numer n) (denom n))))))
  (put 'raise 'rational
       (lambda (x)
         (make-real ((get-coercion 'rational 'real) x))))
  (put 'project 'rational
       (lambda (x)
         (make-integer (floor-quotient (numer x) (denom x)))))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (y x) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (y x) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (y x) (tag (* x y))))
  (put 'div '(real real)
       (lambda (y x) (tag (/ x y))))
  (put 'exp '(real real)
       (lambda (y x) (tag (expt x y))))
  (put 'square-root '(real)
       (lambda (x) (tag (sqrt x))))
  (put 'sine '(real)
       (lambda (x) (tag (sin x))))
  (put 'cosine '(real)
       (lambda (x) (tag (cos x))))
  (put 'arc-tan '(real real)
       (lambda (y x) (tag (atan x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'equ? '(real real) =)
  (put '=zero? '(real) (lambda (n) (= n 0)))
  (put-coercion 'real 'complex
                (lambda (x) (make-complex-from-real-imag (contents x) (make-integer 0))))
  (put 'raise 'real
       (lambda (x)
         ((get-coercion 'real 'complex) x)))
  (put 'project 'real
       (lambda (x)
         (let ((n (rationalize (inexact->exact x) 1/100)))
           (make-rational (numerator n) (denominator n)))))
  'done)

(install-real-package)

(define (make-real n)
  ((get 'make 'real) n))

(define (install-complex-package)
  (define (install-rectangular-package)
    ;; internal procedures
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y)
      (cons x y))
    (define (magnitude z)
      (square-root (add (exp (real-part z) (make-integer 2))
                        (exp (imag-part z) (make-integer 2)))))
    (define (angle z)
      (arc-tan (imag-part z) (real-part z)))
    (define (make-from-mag-ang r a)
      (cons (mul r (cosine a)) (mul r (sine a))))
    (define (equ?-rectangular x y)
      (and (equ? (real-part x) (real-part y))
           (equ? (imag-part x) (imag-part y))))
    (define (=zero?-rectangular n)
      (and (=zero? (real-part n))
           (=zero? (imag-part n))))
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
    (put 'equ? '(rectangular polar) equ?-rectangular)
    (put '=zero? '(rectangular) =zero?-rectangular)
    'done)
  (install-rectangular-package)

  (define (install-polar-package)
    ;; internal procedures
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z)
      (mul (magnitude z) (cosine (angle z))))
    (define (imag-part z)
      (mul (magnitude z) (sine (angle z))))
    (define (make-from-real-imag x y)
      (cons (square-root (add (exp x (make-integer 2)) (exp y (make-integer 2))))
            (arc-tan y x)))
    (define (equ?-polar x y)
      (and (= (magnitude x) (magnitude y))
           (= (angle x) (angle y))))
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
    (put 'equ? '(polar rectangular) equ?-polar)
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
  (define (add-complex z2 z1)
    (make-from-real-imag
     (add (real-part z1) (real-part z2))
     (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z2 z1)
    (make-from-real-imag
     (sub (real-part z1) (real-part z2))
     (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z2 z1)
    (make-from-mag-ang
     (mul (magnitude z1) (magnitude z2))
     (add (angle z1) (angle z2))))
  (define (div-complex z2 z1)
    (make-from-mag-ang
     (div (magnitude z1) (magnitude z2))
     (sub (angle z1) (angle z2))))
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
  (put 'project 'complex
       (lambda (x)
         (make-real (real-part x))))
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


;; 2.5.3 Example: Symbolic Algebra

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x)
    (and (symbol? x)
         (not (memq x '(+ - * / **)))))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (install-sparse-term-list-package)
    (define (tag term-list) (attach-tag 'sparse-term-list term-list))
    (define (adjoin-term-sparse term term-list)
      (if (=zero? (coeff term))
          term-list
          (cons term term-list)))
    (define (empty-termlist?-sparse term-list)
      (null? term-list))
    (define (first-term-sparse term-list) (car term-list))
    (define (rest-terms-sparse term-list) (cdr term-list))
    (put 'adjoin-term '(sparse-term-list)
         (lambda (term term-list) (tag (adjoin-term-sparse term term-list))))
    (put 'empty-termlist? '(sparse-term-list) empty-termlist?-sparse)
    (put 'first-term '(sparse-term-list) first-term-sparse)
    (put 'rest-terms '(sparse-term-list)
         (lambda (term-list)
           (tag (rest-terms-sparse term-list))))
    'done)
  (install-sparse-term-list-package)

  (define (adjoin-term term term-list)
    (let* ((type (type-tag term-list))
           (terms (contents term-list)))
      ((get 'adjoin-term (list type)) term terms)))
  (define (empty-termlist? term-list) (apply-generic 'empty-termlist? term-list))
  (define (first-term term-list) (apply-generic 'first-term term-list))
  (define (rest-terms term-list) (apply-generic 'rest-terms term-list))
  (define (map-terms proc term-list)
    (if (empty-termlist? term-list)
        term-list
        (let ((term-order (order (first-term term-list)))
              (term-coeff (coeff (first-term term-list))))
          (adjoin-term (make-term term-order (proc term-coeff))
                       (map-terms proc (rest-terms term-list))))))

  ;; operations
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
              ADD-POLY"
               (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1
                     (add-terms (rest-terms L1)
                                L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2
                     (add-terms
                      L1
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term
                      (order t1)
                      (add (coeff t1)
                           (coeff t2)))
                     (add-terms
                      (rest-terms L1)
                      (rest-terms L2)))))))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (add-terms (term-list p1)
                    (negate-terms (term-list p2))))
        (error "Polys not in same var:
              ADD-POLY"
               (list p1 p2))))
  (define (negate-terms terms)
    (map-terms
     (lambda (t) (mul t (make-integer -1)))
     terms))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
              MUL-POLY"
               (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms
         (mul-term-by-all-terms
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms
            t1
            (rest-terms L))))))

  (define (=zero?-poly p)
    (fold
     (lambda (term result)
       (and result (=zero? (coeff term))))
     #t
     (term-list p)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial)
       (lambda (p)
         (=zero?-poly p)))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
