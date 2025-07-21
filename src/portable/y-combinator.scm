;;; y-combinator.scm --- Portable Y combinator implementation
;;; Compatible with Guile, Chez, and Racket

;; Y combinator using explicit self-application
;; This version handles multiple arguments via apply
(define Y-explicit
  (lambda (CONTINUE)
    ((lambda (x) (CONTINUE (lambda args (apply (x x) args))))
     (lambda (x) (CONTINUE (lambda args (apply (x x) args)))))))

;; Classic Y combinator: Y = λf.(λx.f (x x)) (λx.f (x x))
;; This is the pure version for single-argument functions
(define Y-classic
  (lambda (f)
    ((lambda (x) (f (x x)))
     (lambda (x) (f (x x))))))

;; Test if we're running under specific implementations
(define implementation-name 'portable)

;; Portable display with newline
(define displayln
  (lambda (x)
    (display x)
    (newline)))

;; Export list for module systems
(define *exports*
  '(Y-explicit implementation-name displayln))
