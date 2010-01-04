#!/usr/bin/env gosh

(require "./c42-dynamicchain")
(require "./check")

(set! stack #(some stack values))

(check (save-stack 3) => #(some stack values))

(check (continuation 3) => 
       `((refer (0 . 0) (nuate ,#(some stack values) (return))) ()))

;;; tests for VM

(check (VM 7 '(halt) '() '() 0) => 7)
(check (VM 0 '(refer (0 . 0) (halt)) '((a) (b c d)) '() 0) => 'a)
(check (VM '() '(constant 7 (halt)) '() '() 0) => 7)
(check (VM '() '(close (refer (0 . 0) (return)) (halt)) '*env* '() 0)
        => '((refer (0 . 0) (return)) *env*))
(check (VM '() '(constant 7 (assign (0 . 0) (refer (0 . 0) (halt)))) '((x)) '() 0) => 7)

;;; tests for evaluate

(set! stack (make-vector 1000))

(check (evaluate 7) => 7)
(check (evaluate '(quote hello)) => 'hello)
(check (evaluate '((lambda () 7))) => 7)
(check (evaluate '((lambda (x y) y) 6 7)) => 7)

(check (evaluate '(if #t 7 0)) => 7)
(check (evaluate '(if #f 0 7)) => 7)
(check (evaluate '((lambda (t) ((lambda (x) t) (set! t 7))) 0)) => 7)
;(set! *debug-flag* #t)
(check (evaluate '(call/cc (lambda (c) (0 3 (c 7))))) => 7)
;(set! *debug-flag* #f)
(check (evaluate '((lambda (f x) (f x)) (lambda (x) x) 7)) => 7)
