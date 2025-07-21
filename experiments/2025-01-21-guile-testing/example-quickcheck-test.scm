#!/usr/bin/env guile
!#
;;; example-quickcheck-test.scm - Example of property-based testing with Guile-QuickCheck
;;;
;;; NOTE: This requires guile-quickcheck to be installed
;;; Install via GNU Guix: guix install guile-quickcheck
;;;
;;; This file demonstrates property-based testing concepts:
;;; - Properties as specifications
;;; - Random test generation
;;; - Shrinking to find minimal counterexamples

;; NOTE: This is a conceptual example - actual syntax may vary
;; based on the specific version of guile-quickcheck

(use-modules (quickcheck))

;;; Functions to test
(define (reverse-list lst)
  (if (null? lst)
      '()
      (append (reverse-list (cdr lst))
              (list (car lst)))))

(define (sort-list lst)
  (if (null? lst)
      '()
      (let ((pivot (car lst))
            (rest (cdr lst)))
        (append (sort-list (filter (lambda (x) (< x pivot)) rest))
                (list pivot)
                (sort-list (filter (lambda (x) (>= x pivot)) rest))))))

(define (sum-list lst)
  (fold + 0 lst))

;;; Properties to test

;; Property: Reversing a list twice gives the original list
(property "reverse is involutive"
  (forall (lst (arbitrary-list arbitrary-integer))
    (equal? lst (reverse-list (reverse-list lst)))))

;; Property: Length is preserved by reverse
(property "reverse preserves length"
  (forall (lst (arbitrary-list arbitrary-integer))
    (= (length lst) (length (reverse-list lst)))))

;; Property: Sorting is idempotent
(property "sort is idempotent"
  (forall (lst (arbitrary-list arbitrary-integer))
    (equal? (sort-list lst) 
            (sort-list (sort-list lst)))))

;; Property: Sorted list has same elements
(property "sort preserves elements"
  (forall (lst (arbitrary-list arbitrary-integer))
    (let ((sorted (sort-list lst)))
      (and (= (length lst) (length sorted))
           (every (lambda (x) (member x sorted)) lst)
           (every (lambda (x) (member x lst)) sorted)))))

;; Property: Sum of concatenated lists
(property "sum distributes over append"
  (forall (lst1 (arbitrary-list arbitrary-integer))
    (forall (lst2 (arbitrary-list arbitrary-integer))
      (= (sum-list (append lst1 lst2))
         (+ (sum-list lst1) (sum-list lst2))))))

;; Property with custom generators
(property "positive integers remain positive after doubling"
  (forall (n (arbitrary-integer-in-range 1 1000))
    (> (* 2 n) 0)))

;; Property testing string operations
(property "string-append is associative"
  (forall (s1 arbitrary-string)
    (forall (s2 arbitrary-string)
      (forall (s3 arbitrary-string)
        (string=? (string-append s1 (string-append s2 s3))
                  (string-append (string-append s1 s2) s3))))))

;;; Example of finding bugs with properties

;; Buggy implementation of remove-duplicates
(define (buggy-remove-duplicates lst)
  ;; This implementation has a bug - it only removes consecutive duplicates
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (if (equal? (car lst) (cadr lst))
          (buggy-remove-duplicates (cdr lst))
          (cons (car lst) (buggy-remove-duplicates (cdr lst))))))

;; This property would fail and QuickCheck would find a counterexample
(property "remove-duplicates actually removes all duplicates"
  (forall (lst (arbitrary-list arbitrary-integer))
    (let ((deduped (buggy-remove-duplicates lst)))
      ;; Check that no element appears more than once
      (every (lambda (elem)
               (<= (count (lambda (x) (equal? x elem)) deduped) 1))
             deduped))))

;;; Running properties with configuration

;; Run with more test cases
(quickcheck-with-config
  (make-quickcheck-config #:num-tests 1000)
  (property "extensive testing of factorial"
    (forall (n (arbitrary-integer-in-range 0 20))
      (>= (factorial n) 1))))

;;; Helper function for factorial
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

(define (count pred lst)
  (length (filter pred lst)))