;; 2.3 Sybolic Data

;; 2.3.1 Quotation


;; Exercise 2.53

; (a b c)
; ((george))
; (y1 y2)
; y1
; #f
; #f
; (red shoes blue socks)


;; Exercise 2.54

(define (my-equal? a b)
  (cond
   ((or (nil? a) (nil? b)) (eq? a b))
   ((and (list? a) (list? b))
    (and (my-equal? (car a) (car b))
         (my-equal? (cdr a) (cdr b))))
   ((and (number? a) (number? b)) (= a b))
   (else (eq? a b))))


;; Exercise 2.55

; the evaluator substitutes (quote <expression>) for every quote
; therefore ''abracadabra becomes (quote (quote abracadabra))
; quoting a list just becomes a list so the final result is
; (quote abracdabra), the car of which is quote
