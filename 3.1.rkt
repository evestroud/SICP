#lang sicp

; Exercise 3.1

(define (make-accumulator n)
  (lambda (m) (set! n (+ n m)) n))

; Exercise 3.2

(define (make-monitored f)
  (let ((count 0))
    (define (call arg)
      (set! count (+ 1 count))
      (f arg))
    (define (dispatch m)
      (cond ((eq? m 'call) call)
            ((eq? m 'get-count) count)
            (else (error "make-monitored: Unsupported action " m))))
    dispatch))

; Exercise 3.3 & 3.4

(define (make-account balance password)
  (define count-bad-attempt (make-attempts-counter 7))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request:
                    MAKE-ACCOUNT" m)))
        (lambda (_)
          (count-bad-attempt))))
  dispatch)

(define (make-attempts-counter max-attempts)
  (let ((attempts 0))
    (define (count-attempt)
      (set! attempts (+ attempts 1))
      (if (< attempts max-attempts)
          "Incorrect password."
          (call-the-cops)))
    count-attempt))

(define (call-the-cops)
  "Wee-woo wee-woo!")

; Exercise 3.5

(define (line x y)
  (<= x y))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (monte-carlo trials (integral-test P x1 x2 y1 y2)))

(define (integral-test P x1 x2 y1 y2)
  (lambda ()
    (P (random-in-range x2 x1) (random-in-range y2 y1))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

; Exercise 3.7

(define (make-joint-account account password joint-password)
  (define count-bad-attempt (make-attempts-counter 7))
  (define (dispatch p m)
    (if (eq? p joint-password)
        (account password m)
        (lambda (_)
          (count-bad-attempt))))
  dispatch)

; Exercise 3.8

(define f
  ((lambda ()
     (let ((x 1))
       (lambda (y)
         (set! x (* x y))
         x)))))
