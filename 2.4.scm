;; 2.4.1 Representations for Complex Numbers

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


;; Rectangular implementation

(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y)
  (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))


;; Polar representation

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))
(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a)
  (cons r a))


;; 2.4.2 Tagged Data

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

; Predicates for identifying representation of a complex number

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

; Selectors and constructors for tagged data

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag
   'rectangular
   (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z)
     (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z)
     (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag
   'polar
   (cons (sqrt (+ (square x) (square y)))
         (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

; Generic selectors and constructors

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type:
               REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type:
               IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type:
               MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type:
               ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))


;; 2.4.3 Data-Directed Programming and Additivity

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
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
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
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
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types:
             APPLY-GENERIC"
            (list op type-tags))))))


;; Exercise 2.73

(define (variable? x)
  (and (symbol? x) (not (or (eq? '+ x)
                            (eq? '* x)
                            (eq? '** x)))))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp)
           (if (eq? exp var)
               1
               0))
         (else ((get 'deriv (operator exp))
                (operands exp)
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; 2.73.2
(define (install-sum-package)
  ;; internal procedures
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  ;; interface to the rest of the system
  (define (deriv-sum operands var)
    (let ((x (addend operands))
          (y (augend operands)))
      (make-sum (deriv x var)
                (deriv y var))))
  (put 'deriv '+ deriv-sum)
  (put 'make '+ make-sum)
  'done)

(define (install-product-package)
  ;; internal procedures
  (define (multiplier s) (car s))
  (define (multiplicand s) (cadr s))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0)
               (=number? m2 0))
           0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2))
           (* m1 m2))
          (else (list '* m1 m2))))

  ;; interface to the rest of the system
  (define (deriv-product operands var)
    (let ((x (multiplier operands))
          (y (multiplicand operands)))
      ((get 'make '+)
       (make-product (deriv x var) y)
       (make-product x (deriv y var)))))
  (put 'deriv '* deriv-product)
  (put 'make '* make-product)
  'done)

; 2.73.3
(define (install-exponent-package)
  ;; internal procedures

  (define (base x)
    (car x))
  (define (exponent x)
    (cadr x))
  (define (make-exponentiation base exponent)
    (cond
     ((=number? exponent 0) 1)
     ((=number? exponent 1) base)
     (else (list '** base exponent))))

  ;; interface to the rest of the system
  (define (deriv-exponent operands var)
    (let ((x (base operands))
          (y (exponent operands)))
      (if (number? y)
             (cond
              ((=number? y 0) 1)
              ((=number? y 1) x)
              (else
               ((get 'make '*)
                ((get 'make '*)
                 y
                 (make-exponentiation x (- y 1)))
                (deriv x var))))
             (error "derivative of variable exponentiation not implemented"))))
  (put 'deriv '** deriv-exponent)
  (put 'make '** make-exponentiation)
  'done)

(use-modules (srfi srfi-1))
(define operator-table (make-hash-table))
(define (get op type)
  (hash-ref
   (hash-ref operator-table op)
   type))
(define (put op type value)
  (let ((type-table (hash-ref operator-table op (make-hash-table))))
    (hash-set!
     type-table
     type
     value)
    (hash-set! operator-table op type-table)))
(define (view-operator-table)
  (display (hash-map->list
            (lambda (x y)
              (format #f
                      "~a:\n~a\n"
                      x
                      (hash-map->list
                       (lambda (xx yy)
                         (format #f "  ~a: ~a\n" xx yy))
                       y)))
            operator-table)))

(install-sum-package)
(install-product-package)
(install-exponent-package)


;; Exercise 2.74
; 2.74.1
(define (get-record employee-id division)
  (get 'employee-record division) employee-id)
; 2.74.2
(define (get-salary employee-id division)
  (get 'salary division) (get-record employee-id division))
; 2.74.3
(define (find-employee-record employee-id all-divisions)
  (fold
   (lambda (found-employee? previous)
     (or found-employee? previous))
   #f
   (map
    (lambda (division)
      ((get 'employee-record division) employee-id))
    all-divisions)))
