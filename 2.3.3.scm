;; 2.3.3 Example: Representing Sets

;; (define (element-of-set? x set)
;;   (cond ((null? set) #f)
;;         ((equal? x (car set)) #t)
;;         (else (element-of-set? x (cdr set)))))

;; (define (adjoin-set x set)
;;   (if (element-of-set? x set)
;;       set
;;       (cons x set)))

;; (define (intersection-set set1 set2)
;;   (cond ((or (null? set1) (null? set2))
;;          '())
;;         ((element-of-set? (car set1) set2)
;;          (cons (car set1)
;;                (intersection-set (cdr set1)
;;                                  set2)))
;;         (else (intersection-set (cdr set1)
;;                                 set2))))


;; Exercise 2.59

;; (define (union-set set1 set2)
;;   (cond ((null? set1)
;;          set2)
;;         ((element-of-set? (car set1) set2)
;;          (union-set (cdr set1) set2))
;;         (else (cons (car set1)
;;                     (union-set (cdr set1)
;;                                set2)))))


;; Exercise 2.60

;; element-of-set? unchanged
;; intersection-set unchanged
;; (define adjoin-set cons)
;; (define union-set append)


;; Sets as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set
                          (cdr set1)
                          set2))
              ((< x2 x1) (intersection-set
                          set1
                          (cdr set2)))))))


;; Exercise 2.61

(define (adjoin-set object set)
  (cond ((null? set) '(object))
        ((> object (car set)) (cons object set))
        ((= object (car set)) set)
        (else (adjoin-set object (cdr set)))))

;; Exercise 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond
            ((< x1 x2)
             (cons x1 (union-set (cdr set1) set2)))
            ((= x1 x2)
             (cons x1 (union-set (cdr set1) (cdr set2))))
            ((> x1 x2)
             (cons x2 (union-set set1 (cdr set2)))))))))


;; Sets as binary trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set?
          x
          (left-branch set)))
        ((> x (entry set))
         (element-of-set?
          x
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))


;; Exercise 2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
       (tree->list-1
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))


;; Exercise 2.64

(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define (print-tree tree)
  (define (helper tree lvl)
    (format #f "~a(~a~a~a)"
            (make-string (* 2 lvl) #\x0020)
            (entry tree)
            (if (nil? (left-branch tree))
                ""
                (format #f "\n~a" (helper (left-branch tree) (+ lvl 1))))
            (if (nil? (right-branch tree))
                ""
                (format #f "\n~a" (helper (right-branch tree) (+ lvl 1))))))
  (display (format #f "~a\n" (helper tree 0))))


;; Exercise 2.65

(define (intersection-set-tree t1 t2)
  (let ((l1 (tree->list-1 t1)) (l2 (tree->list-1 t2)))
    (list->tree (intersection-set l1 l2))))

(define (union-set-tree t1 t2)
  (let ((l1 (tree->list-1 t1)) (l2 (tree->list-1 t2)))
    (list->tree (union-set l1 l2))))


;; Exercise 2.66

(define (lookup val tree)
  (cond
   ((null? tree) #f)
   ((= val (car (entry tree))) (cdr (entry tree)))
   ((< val (car (entry tree))) (lookup val (left-branch tree)))
   ((> val (car (entry tree))) (lookup val (right-branch tree)))))


;; 2.3.4 Example: Huffman Encoding Trees
; Representing Huffman trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


; The decoding procedure

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits)
                current-branch)))
          (if (leaf? next-branch)
              (cons
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit:
               CHOOSE-BRANCH" bit))))


; Sets of weighted elements

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))


;; Exercise 2.67

(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; (A D A B B C A)


;; Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (helper tree)
    (let ((left (left-branch tree)) (right (right-branch tree)))
      (cond
       ((leaf? tree) '())
       ((memq symbol (symbols left))
        (cons 0 (encode-symbol symbol left)))
       ((memq symbol (symbols right))
        (cons 1 (encode-symbol symbol right))))))
  (helper tree))


;; Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (nil? (cdr leaves))
      (car leaves)
      (successive-merge
       (adjoin-set (make-code-tree (car leaves) (cadr leaves))
                   (cddr leaves)))))
