#lang sicp
(#%require sicp-pict)


;; 2.2.4 Example: A Picture Language

(define wave einstein)

(define wave2 (beside wave (flip-vert wave)))
;; (define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2
         (beside painter
                 (flip-vert painter))))
    (below painter2 painter2)))

(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter
                                  (- n 1))))
        (beside painter
                (below smaller smaller)))))


;; Exercise 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter
                                  (- n 1))))
        (below painter
                (beside smaller smaller)))))
;

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right
                                   right))
              (corner (corner-split painter
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right
                         corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter)
                        quarter)))
      (below (flip-vert half) half))))


;; Higher-order operations

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter)
                       (tr painter)))
          (bottom (beside (bl painter)
                          (br painter))))
      (below bottom top))))

(define (flipped-pairs-2 painter)
  (let ((combine4
         (square-of-four identity
                         flip-vert
                         identity
                         flip-vert)))
    (combine4 painter)))

(define (square-limit-2 painter n)
  (let ((combine4
         (square-of-four flip-horiz
                         identity
                         rotate180
                         flip-vert)))
    (combine4 (corner-split painter n))))


;; Exercise 2.45

(define (split d1 d2)
  (define (helper painter n)
    (if (= n 0)
        painter
        (let ((smaller (helper painter (- n 1))))
          (d1 painter
              (d2 smaller smaller)))))
  helper)

(define right-split-2 (split beside below))

(define up-split-2 (split below beside))


;; Frames

;; Exercise 2.46

;; (define (make-vect x y)
;;   (cons x y))

;; (define (xcor-vect v)
;;   (car v))

;; (define (ycor-vect v)
;;   (cdr v))

;; (define (add-vect v1 v2)
;;   (make-vector (+ (xcor-vect v1)
;;                   (xcor-vect v2))
;;                (+ (ycor-vect v1)
;;                   (ycor-vect v2))))

;; (define (sub-vect v1 v2)
;;   (make-vector (- (xcor-vect v1)
;;                   (xcor-vect v2))
;;                (- (ycor-vect v1)
;;                   (ycor-vect v2))))

;; (define (scale-vect s v)
;;   (make-vect (* s (xcor-vect v))
;;              (* s (ycor-vect v))))


;; Exercise 2.47

;; (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))

;; (define (origin-frame frame)
;;   (car frame))

;; (define (edge1-frame frame)
;;   (cadr frame))

;; (define (edge2-frame frame)
;;   (caddr frame))

;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))

;; (define (origin-frame frame)
;;   (car frame))

;; (define (edge1-frame frame)
;;   (cadr frame))

;; (define (edge2-frame frame)
;;   (cddr frame))


;; (define (frame-coord-map frame)
;;   (lambda (v)
;;     (add-vect
;;      (origin-frame frame)
;;      (add-vect
;;       (scale-vect (xcor-vect v)
;;                   (edge1-frame frame))
;;       (scale-vect (ycor-vect v)
;;                   (edge2-frame frame))))))


;; Painters
