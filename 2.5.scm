(use-modules (srfi srfi-1))

;; Tag operations
                                        ; Exercise 2.78
(define (attach-tag type-tag contents)
  ; TODO guile's built-in number predicates will pass reals as ints/rats if it can
  ; convert them easily, leading to loops where raising 1 -> 1.0 gets constantly
  ; re-interpreted as an integer (or 0.5 as a rational)
  ; - remove the implicit stripping of type tags
  ; - add explicit tagging back to integer and real packages
  ; - MAYBE - remove type inference in type-tag
  ;     - this is the majority of the syntactic sugar
  ;     - refactor it to convert and tag Scheme rationals to package rationals?
  ;     - could also add a set of meta functions that strip tags and convert to Scheme numbers
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((integer? datum) 'integer)
        ((real? datum) 'real)
        (else (error "Bad tagged datum:
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum:
              CONTENTS" datum))))


;; Generic operations
;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           (error
;;            "No method for these types:
;;              APPLY-GENERIC"
;;            (list op type-tags))))))


;; Generic Arithmetic
(define (add . args) (apply apply-generic (cons 'add args)))
(define (sub . args) (apply apply-generic (cons 'sub args)))
(define (mul . args) (apply apply-generic (cons 'mul args)))
(define (div . args) (apply apply-generic (cons 'div args)))
                                        ; Exercise 2.79
(define (equ? x y) (apply-generic 'equ? x y))
                                        ; Exercise 2.80
(define (=zero? n) (apply-generic '=zero? n))


;; Operations and types table
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

; Coercion table

(define coercion-table (make-hash-table))
(define (get-coercion type1 type2)
                                        ; Exercise 2.81
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


;; Setting up arithmetic packages
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
  (put 'make 'integer
       (lambda (x) (tag x)))
                                        ; Exercise 2.79
  (put 'equ? '(integer integer) =)
                                        ; Exercise 2.80
  (put '=zero? '(integer) (lambda (n) (= n 0)))
                                        ; Exercise 2.81
  (put 'exp
       '(integer integer)
       (lambda (x y)
         (tag (expt x y))))
                                        ; Exercise 2.83
  (put-coercion 'integer 'rational
                (lambda (n) (make-rational n 1)))
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
    (cond ((=zero? d) (error "Cannot make a rational number with a denominator of 0"))
          ((and (equal? 'integer (type-tag n)) (equal? 'integer (type-tag d)))
           (let ((g (gcd n d)))
             (cons (/ n g) (/ d g))))
          (else (contents (div n d)))))
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
    (make-rat (* (denom x) (numer y))
              (* (numer x) (denom y))))
                                        ; Exercise 2.79
  (define (equ?-rat x y)
    (and (equ? (numer x) (numer y))
         (equ? (denom y) (denom y))))
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
                                        ; Exercise 2.83
  (put-coercion 'rational 'real
                (lambda (x)
                  (let ((n (contents x)))
                    (exact->inexact (/ (numer n) (denom n))))))
  (put 'raise 'rational
       (lambda (x)
         ((get-coercion 'rational 'real) x)))
                                        ; Exercise 2.85
  (put 'project 'rational
       (lambda (x)
         (floor-quotient (numer x) (denom x))))
  'done)

(install-rational-package)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
                                        ; Exercise 2.79
  (put 'equ? '(real real) =)
                                        ; Exercise 2.80
  (put '=zero? '(real) (lambda (n) (= n 0)))
                                        ; Exercise 2.81
  (put 'exp
       '(real real)
       (lambda (x y)
         (tag (expt x y))))
                                        ; Exercise 2.83
  (put-coercion 'real 'complex
                (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'raise 'real
       (lambda (x)
         ((get-coercion 'real 'complex) x)))
  (put 'project 'real
       (lambda (x)
         ; challenge i won't get around to: make my own rationalize fn
         (let ((n (rationalize (inexact->exact x) 1/100)))
           (make-rational (numerator n) (denominator n)))))
  'done)

(install-real-package)
(define (make-real n)
  ((get 'make 'real) n))

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
      (and (equ? (real-part x) (real-part y))
           (equ? (imag-part x) (imag-part y))))
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
      (and (equ? (magnitude x) (magnitude y))
           (equ? (angle x) (angle y))))
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
     (add (real-part z1) (real-part z2))
     (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (sub (real-part z1) (real-part z2))
     (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (mul (magnitude z1) (magnitude z2))
     (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (div (magnitude z1) (magnitude z2))
     (sub (angle z1) (angle z2))))
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
  ; Exercise 2.85
  (put 'project 'complex
       (lambda (x)
         (make-real (real-part x))))
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

;; (define (apply-generic op . args)
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           (cond
;;            ((= (length args) 2)
;;             (let ((type1 (car type-tags))
;;                   (type2 (cadr type-tags))
;;                   (a1 (car args))
;;                   (a2 (cadr args)))
;;               (let ((t1->t2
;;                      (get-coercion type1
;;                                    type2))
;;                     (t2->t1
;;                      (get-coercion type2
;;                                    type1)))
;;                 (cond (t1->t2
;;                        (apply-generic
;;                         op (t1->t2 a1) a2))
;;                       (t2->t1
;;                        (apply-generic
;;                         op a1 (t2->t1 a2)))
;;                       (else
;;                        (error
;;                         "No method for these types"
;;                         (list
;;                          op
;;                          type-tags)))))))
;;                                         ; Exercise 2.82
;;            ((> (length args) 2)
;;             (let* ((types (delete-duplicates type-tags))
;;                    (coercions (fold
;;                                (lambda (type1 return)
;;                                  (display (format #f "~a ~a\n" type1 return))
;;                                  (if (every (lambda (x) x) return)
;;                                      return
;;                                      (map
;;                                       (lambda (type2)
;;                                         (if (eq? type1 type2)
;;                                             (lambda (x) x)
;;                                             (get-coercion type2 type1)))
;;                                       type-tags)))
;;                                (list #f)
;;                                types)))
;;               (display coercions) (newline)
;;               (if (every (lambda (x) x) coercions)
;;                   (let ((coerced (map
;;                                   (lambda (arg coerce)
;;                                     (coerce arg))
;;                                   args
;;                                   coercions)))
;;                     (display coerced) (newline)
;;                     (fold
;;                      (lambda (a2 a1)
;;                        (apply-generic op a1 a2))
;;                      (car coerced)
;;                      (cdr coerced)))
;;                   (error
;;                   "No method for these types"
;;                   (list op type-tags)))))
;;            (else (error
;;                   "No method for these types"
;;                   (list op type-tags))))))))

; Coercion operations

;; (define (integer->complex n)
;;   (make-complex-from-real-imag
;;    (contents n) 0))

;; (put-coercion 'integer 'complex
;;               integer->complex)


;; Exercise 2.81

;; (define (integer->integer n) n)
;; (define (complex->complex z) z)

;; (put-coercion 'integer 'integer
;;               integer->integer)

;; (put-coercion 'complex 'complex
;;               complex->complex)

(define (exp x y)
  (apply-generic 'exp x y))


;; Exercise 2.83

(define (raise n)
  (let ((coerce (get 'raise (type-tag n))))
    (if coerce
        (coerce n)
        #f)))

;; (put-coercion 'rational 'complex
;;               (lambda (n) (make-complex-from-real-imag n 0)))


;; Exercise 2.84

(define tower
  '(complex
    real
    rational
    integer))

(define (get-height type)
  (list-index
   (lambda (elem) (eq? elem type))
   tower))

(define (raise-to-height value target)
  (let* ((type (type-tag value))
         (height (get-height type)))
    (cond
     ((= height target) value)
     ((> target height) (error "Invalid raise" (list height target)))
     (else (raise-to-height (raise value) target)))))

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
  (if (= (length args) 1)
      (apply-unary op (car args))
      (let ((result (fold
                     (lambda (arg acc)
                       (apply-binary op arg acc))
                     (car args)
                     (cdr args))))
        (if (not (boolean? result))
            (drop result)
            result))))


;; Display helpers

(define (auto-format . args)
  (let ((format-string (string-join (make-list (length args) "~a"))))
    (apply format (append (list #f format-string) args))))

(define (print . args)
  (display (apply auto-format args)))

(define (println . args)
  (apply print args)
  (newline))


;; Exercise 2.85

(define (drop x)
  (let ((proc (get 'project (type-tag x))))
    (if proc
        (let ((projected (proc (contents x))))
          (if (equ? x (raise projected))
              (drop projected)
              x))
        x)))
