#!/usr/bin/env gosh

(use gauche.test)
(require "./c34-heap")

;; Tests for compiler

(test* "compile refer" '(refer hello ())
       (compile 'hello '()))

(test* "compile constant" '(constant hello ())
       (compile '(quote hello) '()))

(test* "close" '(close (c)                ; vars
		       (refer c (return)) ; body
		       (halt))            ; next
       (compile '(lambda (c) c) '(halt)))

(test* "compile test" '(constant #t (test (constant 3 ()) (constant 4 ())))
       (compile '(if #t 3 4) '()))

(test* "compile assign" '(constant 7 (assign var ()))
       (compile '(set! var 7) '()))

(test* "compile call/cc" '(frame (halt)
				 (conti (argument (close (c)
							 (refer c (return))
							 (apply)))))
       (compile '(call/cc (lambda (c) c)) '(halt)))

(test* "compile call/cc tail" '(conti (argument (close (c)
						       (refer c (return))
						       (apply))))
       (compile '(call/cc (lambda (c) c)) '(return)))

(test* "compile apply" '(frame (halt)
			       (refer b (argument (refer a (argument (refer func (apply)))))))
       (compile '(func a b) '(halt)))

(test* "compile apply tail" '(refer b (argument (refer a (argument (refer func (apply))))))
       (compile '(func a b) '(return)))

(test* "compile constant" '(constant 7 ())
       (compile 7 '()))

;; Tests for VM

(test* "halt" 7 (VM 7 '(halt) '() '() '()))

(test* "refer" 7 (VM '() '(refer b (halt)) '(((a b c) . (6 7 8))) '() '()))

(test* "constant" 7 (VM '() '(constant 7 (halt)) '() '() '()))

(test* "close" '((a) () (a)) (VM '() '(close (a) (a) (halt)) '() '() '()))

(test* "assign" 7 (VM '() '(constant 7 (assign x (refer x (halt)))) '(((x) . (0))) '() '()))

(test* "return" 7
       (VM 0
	   '(return)
	   '()
	   '()
	   '((refer x (halt))
	     (((x) . (7)))
	     ()
	     ())))

;(test* "frame" () (VM 7 '(frame (halt) (halt)) '() '() '()))

;; tests for evaluate

(test* "evaluate constant" 7 (evaluate 7))
(test* "evaluate quote" 'hello (evaluate '(quote hello)))
(test* "evaluate lambda" 7 (evaluate '((lambda (x y) y) 6 7)))
(test* "evaluate if #t" 7 (evaluate '(if #t 7 something)))
(test* "evaluate if #f" 7 (evaluate '(if #f something 7)))
(test* "evaluate set!" 7 (evaluate '((lambda (t) ((lambda (x) t) (set! t 7))) 0)))
(test* "evaluate call/cc" 7 (evaluate '(call/cc (lambda (c) (something 3 (c 7))))))
(test* "evaluate apply" 7 (evaluate '((lambda (f x) (f x)) (lambda (x) x) 7)))

(test-end)
