#!/usr/bin/env gosh

(require "./c41-block")
(require "./check")

(define s 0)
(check (set! s (push 5 s)) => 1)
(check (set! s (push 6 s)) => 2)
(check (set! s (push 7 s)) => 3)
(check (index s 0) => 7)
(check (index s 2) => 5)
(index-set! s 0 'hello)
(check (index s 0) => 'hello)

(check (compile-lookup 'a '((a)) cons) => '(0 . 0))
(check (compile-lookup 'a '((a) (a b) (a b c)) cons) => '(0 . 0))
(check (compile-lookup 'b '((a) (a b) (a b c)) cons) => '(1 . 1))
(check (compile-lookup 'c '((a) (a b) (a b c)) cons) => '(2 . 2))

(check (compile 'hello '((hello)) ()) => '(refer 0 0 ()))


(check (compile '(lambda (c) c) '() '(halt))
       =>  '(close (refer 0 0 (return 2)) ; body
		   (halt)))               ; next

(check (compile '(lambda (a b) c) '((c)) '(halt))
       =>  '(close (refer 1 0 (return 3)) ; body
		   (halt)))               ; next

(check (compile '(set! var 7) '((var)) '()) => '(constant 7 (assign 0 0 ())))

;             0 1 2 3 4 5 6 7 8 9 10 11 12
;             | | |       |     |
(set! stack #(f 0 1 e d c 2 b a 6 () () ()))

(check (find-link 0 9) => 9)
(check (find-link 1 9) => 6)
(check (find-link 2 9) => 2)
(check (find-link 3 9) => 1)

(check (find-link 0 6) => 6)
(check (find-link 1 6) => 2)
(check (find-link 2 6) => 1)

(check (VM 0 '(refer 0 0 (halt)) 9 10) => 'a)
(check (VM 0 '(refer 0 1 (halt)) 9 10) => 'b)
(check (VM 0 '(refer 1 0 (halt)) 9 10) => 'c)
(check (VM 0 '(refer 1 1 (halt)) 9 10) => 'd)

(VM 'E '(assign 1 2 (halt)) 9 10)
(check stack => #(f 0 1 E d c 2 b a 6 () () ()))

(VM '() '(constant A (argument (constant B (argument (halt))))) 9 10)
(check stack => #(f 0 1 E d c 2 b a 6 A B ()))

(set! stack (make-vector 1000))

;;; tests for evaluate

(check (evaluate 7) => 7)
(check (evaluate '(quote hello)) => 'hello)
;(print "***")
(check (evaluate '((lambda (x y) y) 6 7)) => 7)
(check (evaluate '(if #t 7 0)) => 7)
(check (evaluate '(if #f 0 7)) => 7)
(check (evaluate '((lambda (t) ((lambda (x) t) (set! t 7))) 0)) => 7)
;; (check (evaluate '(call/cc (lambda (c) (0 3 (c 7))))) => 7)
(check (evaluate '((lambda (f x) (f x)) (lambda (x) x) 7)) => 7)

