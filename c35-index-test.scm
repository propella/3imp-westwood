#!/usr/bin/env gosh

(require "./c35-index")
(require "./check")

(check (extend '((a b)) '(a)) => '((a) (a b)))

(check (compile-lookup 'a '((a))) => '(0 . 0))
(check (compile-lookup 'a '((a) (a b) (a b c))) => '(0 . 0))
(check (compile-lookup 'b '((a) (a b) (a b c))) => '(1 . 1))
(check (compile-lookup 'c '((a) (a b) (a b c))) => '(2 . 2))

(check (compile 'hello '((hello)) ()) => '(refer (0 . 0) ()))

(check (compile '(lambda (c) c) '() '(halt))
       =>  '(close (refer (0 . 0) (return)) ; body
		   (halt)))                 ; next

(check (compile '(lambda (a) b) '((b)) '(halt))
       =>  '(close (refer (1 . 0) (return)) ; body
		   (halt)))                 ; next

(check (compile '(set! var 7) '((var)) '()) => '(constant 7 (assign (0 . 0) ())))

(check (lookup '(0 . 0) '((1) (2 3 4))) => '(1))
(check (lookup '(1 . 1) '((1) (2 3 4))) => '(3 4))
(check (lookup '(1 . 2) '((1) (2 3 4))) => '(4))

;;; tests for evaluate

(check (evaluate 7) => 7)
(check (evaluate '(quote hello)) => 'hello)
(check (evaluate '((lambda (x y) y) 6 7)) => 7)
(check (evaluate '(if #t 7 0)) => 7)
(check (evaluate '(if #f 0 7)) => 7)
(check (evaluate '((lambda (t) ((lambda (x) t) (set! t 7))) 0)) => 7)
(check (evaluate '(call/cc (lambda (c) (0 3 (c 7))))) => 7)
(check (evaluate '((lambda (f x) (f x)) (lambda (x) x) 7)) => 7)

