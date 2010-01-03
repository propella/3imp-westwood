#!/usr/bin/env gosh

(require "./c34-heap")
(require "./check")

;;; Tests for compiler

(check (compile 'hello '()) => '(refer hello ()))

(check (compile '(quote hello) '()) => '(constant hello ()))

(check (compile '(lambda (c) c) '(halt))
       =>  '(close (c)                ; vars
		   (refer c (return)) ; body
		   (halt)))           ; next

(check (compile '(if #t 3 4) '()) =>
       '(constant #t (test (constant 3 ()) (constant 4 ()))))

(check (compile '(set! var 7) '()) => '(constant 7 (assign var ())))

(check (compile '(call/cc (lambda (c) c)) '(halt))
       => '(frame (halt)
		  (conti (argument (close (c)
					  (refer c (return))
					  (apply))))))
       
(check (compile '(call/cc (lambda (c) c)) '(return))
       => '(conti (argument (close (c)
				   (refer c (return))
				   (apply)))))
      
(check (compile '(func a b) '(halt))
       =>  '(frame (halt)
		   (refer b (argument (refer a (argument (refer func (apply))))))))
       
(check (compile '(func a b) '(return))
       => '(refer b (argument (refer a (argument (refer func (apply)))))))

(check (compile 7 '()) => '(constant 7 ()))

;;; Tests for VM

(check (VM 7 '(halt) '() '() '()) => 7)
(check (VM '() '(refer b (halt)) '(((a b c) . (6 7 8))) '() '()) => 7)
(check (VM '() '(constant 7 (halt)) '() '() '()) => 7)
(check (VM '() '(close (a) (a) (halt)) '() '() '()) => '((a) () (a)))
(check (VM '() '(constant 7 (assign x (refer x (halt)))) '(((x) . (0))) '() '()) => 7)

;; conti saves the stack
(check (VM '() '(conti (halt)) '() '() '(STACK)) => '((nuate (STACK) v) () (v)))

;; nuate resumes the stack
(check (VM '()                            ; acc
	   '(nuate ((halt) () () ()) arg) ; next
	   '(((arg) . (7)))               ; env
	   '()                            ; rub
	   '())                           ; stack
       => 7)

;; return
(check (VM 0                                        ; acc
	   '(return)                                ; next
	   '()                                      ; env
	   '()                                      ; rub
	   '((refer x (halt)) (((x) . (7))) () ())) ; stack (next env rub stack)
       => 7)

;; Trace a behavior of ((lambda (a) a) 7)
(check (VM '() '(frame (halt)
		       (constant 7 (argument (close (a)
						    (refer a (return))
						    (apply)))))
	   '() '() '())
       => 7)

(check (VM '() '(constant 7 (argument (close (a)
					     (refer a (return))
					     (apply))))
	   '() '() '((halt) () () ()))
       => 7)

(check (VM 7 '(argument (close (a)
			       (refer a (return))
			       (apply)))
	   '() '() '((halt) () () ()))
       => 7)

(check (VM '() '(close (a) (refer a (return)) (apply))
	   '() '(7) '((halt) () () ()))
       => 7)

(check (VM '((refer a (return)) () (a)) '(apply)
	   '() '(7) '((halt) () () ()))
       => 7)

;;; tests for evaluate

(check (evaluate 7) => 7)
(check (evaluate '(quote hello)) => 'hello)
(check (evaluate '((lambda (x y) y) 6 7)) => 7)
(check (evaluate '(if #t 7 0)) => 7)
(check (evaluate '(if #f 0 7)) => 7)
(check (evaluate '((lambda (t) ((lambda (x) t) (set! t 7))) 0)) => 7)
(check (evaluate '(call/cc (lambda (c) (0 3 (c 7))))) => 7)
(check (evaluate '((lambda (f x) (f x)) (lambda (x) x) 7)) => 7)
