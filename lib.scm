;; to use:
;;  (add-to-load-path ".")
;;  (use-modules (lib))

(define-module (lib)
  #:export (accumulate))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))
