#!/usr/bin/env guile
!#
;;; tap-runner.scm - TAP (Test Anything Protocol) output for SRFI-64 tests
;;;
;;; This runner produces TAP-compliant output for integration with CI systems
;;; and other tools that understand TAP format.

(use-modules (srfi srfi-64)
             (ice-9 format))

(define (make-tap-runner)
  "Create a test runner that outputs TAP format"
  (let ((runner (test-runner-null))
        (test-count 0)
        (current-group-stack '()))
    
    ;; Initialize runner
    (test-runner-on-test-begin! runner
      (lambda (runner)
        (set! test-count (+ test-count 1))))
    
    ;; Output TAP header when suite begins
    (test-runner-on-group-begin! runner
      (lambda (runner suite-name count)
        (when (null? current-group-stack)
          ;; This is the top-level suite
          (format #t "TAP version 13~%")
          (when count
            (format #t "1..~a~%" count)))
        (set! current-group-stack 
              (cons suite-name current-group-stack))))
    
    ;; Pop group stack when leaving a group
    (test-runner-on-group-end! runner
      (lambda (runner)
        (unless (null? current-group-stack)
          (set! current-group-stack (cdr current-group-stack)))))
    
    ;; Output test results in TAP format
    (test-runner-on-test-end! runner
      (lambda (runner)
        (let* ((result-kind (test-result-kind runner))
               (test-name (test-runner-test-name runner))
               (group-path (string-join (reverse current-group-stack) " > "))
               (full-name (if (string-null? group-path)
                              test-name
                              (format #f "~a > ~a" group-path test-name))))
          
          (case result-kind
            ((pass)
             (format #t "ok ~a - ~a~%" test-count full-name))
            
            ((fail)
             (format #t "not ok ~a - ~a~%" test-count full-name)
             (format #t "  ---~%")
             (format #t "  message: Test failed~%")
             
             ;; Include expected/actual values if available
             (when (test-result-ref runner 'expected-value #f)
               (format #t "  expected: ~s~%" 
                       (test-result-ref runner 'expected-value)))
             (when (test-result-ref runner 'actual-value #f)
               (format #t "  actual: ~s~%" 
                       (test-result-ref runner 'actual-value)))
             
             ;; Include source location if available
             (let ((file (test-result-ref runner 'source-file #f))
                   (line (test-result-ref runner 'source-line #f)))
               (when (and file line)
                 (format #t "  at: ~a:~a~%" file line)))
             
             (format #t "  ...~%"))
            
            ((xfail xpass)
             (format #t "not ok ~a - ~a # TODO ~a~%" 
                     test-count full-name
                     (if (eq? result-kind 'xfail)
                         "expected failure"
                         "unexpected pass")))
            
            ((skip)
             (format #t "ok ~a - ~a # SKIP~%" test-count full-name))))))
    
    ;; Final summary with diagnostic info
    (test-runner-on-final! runner
      (lambda (runner)
        (let ((pass-count (test-runner-pass-count runner))
              (fail-count (test-runner-fail-count runner))
              (xpass-count (test-runner-xpass-count runner))
              (xfail-count (test-runner-xfail-count runner))
              (skip-count (test-runner-skip-count runner)))
          
          ;; TAP diagnostic output (comments)
          (format #t "# Test suite completed~%")
          (format #t "# Pass: ~a~%" pass-count)
          (when (> fail-count 0)
            (format #t "# Fail: ~a~%" fail-count))
          (when (> xpass-count 0)
            (format #t "# Unexpected pass: ~a~%" xpass-count))
          (when (> xfail-count 0)
            (format #t "# Expected fail: ~a~%" xfail-count))
          (when (> skip-count 0)
            (format #t "# Skip: ~a~%" skip-count))
          
          ;; Set exit code based on results
          (exit (if (and (zero? fail-count) 
                         (zero? xpass-count))
                    0 1)))))
    
    runner))

;;; Example usage functions

(define (run-test-file-with-tap filename)
  "Run a test file with TAP output"
  (test-runner-current (make-tap-runner))
  (load filename))

(define (run-tests-with-tap test-thunk)
  "Run tests with TAP output"
  (test-runner-current (make-tap-runner))
  (test-thunk))

;;; Example test suite to demonstrate TAP output

(define (example-test-suite)
  (test-begin "tap-example")
  
  (test-group "basic-tests"
    (test-assert "simple assertion" #t)
    (test-equal "equality check" 4 (+ 2 2))
    (test-equal "this will fail" 5 (+ 2 2)))
  
  (test-group "advanced-tests"
    (test-skip 1)
    (test-assert "skipped test" #f)
    
    (test-expect-fail 1)
    (test-assert "expected failure" #f)
    
    (test-error "error handling" (/ 1 0)))
  
  (test-end "tap-example"))

;;; Main entry point when run as script

(define (main args)
  (cond
   ;; Run specific test file
   ((= (length args) 2)
    (run-test-file-with-tap (cadr args)))
   
   ;; Run example suite
   ((and (= (length args) 2) 
         (string=? (cadr args) "--example"))
    (run-tests-with-tap example-test-suite))
   
   ;; Show usage
   (else
    (format #t "Usage: ~a <test-file.scm>~%" (car args))
    (format #t "   or: ~a --example~%" (car args))
    (exit 1))))

;; Run main if this is the main module
(when (equal? (car (command-line)) "tap-runner.scm")
  (main (command-line)))