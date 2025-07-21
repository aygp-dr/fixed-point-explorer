;;; y-combinator.scm --- Portable Y combinator implementation
;;; Compatible with Guile, Chez, and Racket

;; Y combinator using explicit self-application
(define Y-explicit
  (lambda (CONTINUE)
    ((lambda (x) (CONTINUE (lambda args (apply (x x) args))))
     (lambda (x) (CONTINUE (lambda args (apply (x x) args)))))))

;; Test if we're running under specific implementations
(define implementation-name
  (cond
   ((string-contains (version) "GNU Guile") 'guile)
   (else 'unknown)))

;; Portable display with newline
(define displayln
  (lambda (x)
    (display x)
    (newline)))

;; Export list for module systems
(define *exports*
  '(Y-explicit implementation-name displayln))
