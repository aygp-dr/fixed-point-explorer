;;; list-ops.scm --- List operations using Y combinator

(load "y-combinator.scm")

;; Append
(define CONTINUE-append
  (lambda (f)
    (lambda (lst1 lst2)
      (if (null? lst1)
          lst2
          (cons (car lst1) (f (cdr lst1) lst2))))))

(define append-y (Y-explicit CONTINUE-append))

;; Length
(define CONTINUE-length
  (lambda (f)
    (lambda (lst)
      (if (null? lst)
          0
          (+ 1 (f (cdr lst)))))))

(define length-y (Y-explicit CONTINUE-length))

;; Map
(define CONTINUE-map
  (lambda (f)
    (lambda (g lst)
      (if (null? lst)
          '()
          (cons (g (car lst)) (f g (cdr lst)))))))

(define map-y (Y-explicit CONTINUE-map))

;; Filter
(define CONTINUE-filter
  (lambda (f)
    (lambda (pred lst)
      (cond ((null? lst) '())
            ((pred (car lst)) (cons (car lst) (f pred (cdr lst))))
            (else (f pred (cdr lst)))))))

(define filter-y (Y-explicit CONTINUE-filter))

;; Fold-right
(define CONTINUE-foldr
  (lambda (f)
    (lambda (op init lst)
      (if (null? lst)
          init
          (op (car lst) (f op init (cdr lst)))))))

(define foldr-y (Y-explicit CONTINUE-foldr))

;; Test function
(define test-list-ops
  (lambda ()
    (displayln "\nTesting List Operations:")
    (display "  append-y '(1 2 3) '(4 5 6) = ")
    (displayln (append-y '(1 2 3) '(4 5 6)))
    (display "  length-y '(a b c d e) = ")
    (displayln (length-y '(a b c d e)))
    (display "  map-y (lambda (x) (* x 2)) '(1 2 3 4) = ")
    (displayln (map-y (lambda (x) (* x 2)) '(1 2 3 4)))
    (display "  filter-y even? '(1 2 3 4 5 6) = ")
    (displayln (filter-y even? '(1 2 3 4 5 6)))
    (display "  foldr-y + 0 '(1 2 3 4 5) = ")
    (displayln (foldr-y + 0 '(1 2 3 4 5)))))
