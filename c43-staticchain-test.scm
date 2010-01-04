#!/usr/bin/env gosh

(require "./c43-staticchain")
(require "./check")

;; ;;; Tests for the dynamic chain

(set! stack #(some stack values))

(check (continuation 3) => 
       `((refer 0 0 (nuate ,#(some stack values) (return 0))) ()))

;;; Tests for environment

;             0 1 2 3 4 5 6 7 8 9 10 11 12
;             | | |       |     |
(set! stack #(f 0 1 e d c 2 b a 6 () () ()))

;;; Tests for VM

(check (VM 7 '(halt) 0 0) => 7)

(check (VM 0 '(refer 0 0 (halt)) 9 10) => 'a)
(check (VM 0 '(refer 0 1 (halt)) 9 10) => 'b)
(check (VM 0 '(refer 1 0 (halt)) 9 10) => 'c)
(check (VM 0 '(refer 1 1 (halt)) 9 10) => 'd)

(check (VM '() '(constant 7 (halt)) 0 0) => 7)
(check (VM '() '(close (refer (0 . 0) (return)) (halt)) 0 0)
       => '((refer (0 . 0) (return)) 0))

;; ;; conti saves the stack
;; (set! stack #(some stack values))
;; (check (VM '() '(conti (halt)) '() '() 3)
;;        => '((refer (0 . 0) (nuate #(some stack values) (return))) ()))

;; ;;; Tests for evaluate

(set! stack (make-vector 1000))

(check (evaluate 7) => 7)
(check (evaluate '(quote hello)) => 'hello)
(check (evaluate '((lambda () 7))) => 7)
(check (evaluate '((lambda (x y) y) 6 7)) => 7)

(check (evaluate '(if #t 7 0)) => 7)
(check (evaluate '(if #f 0 7)) => 7)
;(check (evaluate '((lambda (t) ((lambda (x) t) (set! t 7))) 0)) => 7)
;; (set! *debug-flag* #t)
(check (evaluate '(call/cc (lambda (c) (0 3 (c 7))))) => 7)
;; ;(set! *debug-flag* #f)
(check (evaluate '((lambda (f x) (f x)) (lambda (x) x) 7)) => 7)

;; (set! *debug-flag* #t)
;; (check (evaluate '((lambda (a) a) 7)) => 7)
;; (set! *debug-flag* #f)
