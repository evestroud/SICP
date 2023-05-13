;; 2.3.2 Example: Symbolic Differentiation

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)  ; Exercise 2.56
         (if (number? (exponent exp))
             (cond
              ((eq? (exponent exp) 0) 1)
              ((eq? (exponent exp) 1) (base exp))
              (else
               (make-product
                (make-product
                 (exponent exp)
                 (make-exponentiation (base exp) (- (exponent exp) 1)))
                (deriv (base exp) var))))
             (error "derivative of variable exponentiation not implemented")))
        (else (error "unknown expression type: DERIV" exp))))

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

(define (expression? x)
  (or (variable? x)
      (number? x)
      (sum? x)
      (product? x)
      (exponentiation? x)))

(define (expression-list? x)
  (and (list? x) (expression? (car x))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
                                        ; Exercise 2.57
        ((sum? a2) (append (list '+ a1)
                           (augend a2)))
        ((expression-list? a2) (append (list '+ a1) a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s)
                                        ; Exercise 2.57
  (if (and (expression-list? (cddr s))
           (> (length (cddr s)) 1))
      (make-sum (caddr s) (cdddr s))
      (caddr s)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
                                        ; Exercise 2.57
        ((product? m2) (append (list '* m1)
                               (multiplicand m2)))
        ((expression-list? m2) (append (list '* m1) m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p)
                                        ; Exercise 2.57
  (if (and (expression-list? (cddr p))
           (> (length (cddr p)) 1))
      (make-product (caddr p) (cdddr p))
      (caddr p)))


;; Exercise 2.56

(define (make-exponentiation base exponent)
  (list '** base exponent))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))
