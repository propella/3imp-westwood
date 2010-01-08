#!/usr/bin/env gosh

(require "./c45-assign")
(require "./check")

(check (find-sets 'hello '(a b c)) => '())
(check (find-sets '(quote hello) '(a b c)) => '())
(check (find-sets '(set! a 10) '(a b c)) => '(a))
(check (find-sets '(set! d 10) '(a b c)) => '())
(check (find-sets '(set! a (set! b 10)) '(a b c)) => '(a b))
(check (find-sets '(lambda (a) (set! b 10)) '(a b c)) => '(b))
(check (find-sets '(lambda (a) (set! a 10)) '(a b c)) => '())
(check (find-sets '(if (set! a 30) (set! b 10) (set! c 10)) '(a b c)) => '(a b c))
(check (find-sets '(call/cc (lambda () (set! b 10))) '(a b c)) => '(b))
(check (find-sets '(func (set! a 10) (set! b 10)) '(a b)) => '(a b))

;;; Tests for find-free

(check (find-free 'hello '(a b c)) => '(hello))
(check (find-free '(quote hello) '(a b c)) => '())
(check (find-free '(lambda (x) (x y)) '()) => '(y))
(check (find-free '(lambda (x) (x y)) '(y)) => '())
(check (find-free '(if x y z) '(x y)) => '(z))
(check (find-free '(call/cc (lambda (x) (x y))) '()) => '(y))
(check (find-free '(lambda (x) (func x y)) '()) => '(func y))
(check (find-free 7 '()) => '())

(check (find-free '(set! a 10) '(a b c)) => '())
(check (find-free '(set! d 10) '(a b c)) => '(d))

(check (make-boxes '(a b c) '() '*next*) => '*next*)
(check (make-boxes '(a b c) '(b c d) '*next*) => '(box 0 (box 1 *next*)))

;;; Tests for variable reference (same as c44)

(check (compile-lookup 'x '((x) . ())
	  (lambda (n) (list 'local n))
	  (lambda (n) (list 'free n))) => '(local 0))

(check (compile-lookup 'x '((x) . (x))
	  (lambda (n) (list 'local n))
	  (lambda (n) (list 'free n))) => '(local 0))

(check (compile-lookup 'y '((x) . (x y))
	  (lambda (n) (list 'local n))
	  (lambda (n) (list 'free n))) => '(free 1))

(check (compile-refer 'x '((x) . ()) '*next*) => '(refer-local 0 *next*))
(check (compile-refer 'x '((x) . (x)) '*next*) => '(refer-local 0 *next*))
(check (compile-refer 'y '((x) . (x y)) '*next*) => '(refer-free 1 *next*))

(check (collect-free '(x y) '((x) . (x y)) '*next*)
       => '(refer-free 1 (argument (refer-local 0 (argument *next*)))))

;;; Tests for compile

(check (compile 7 '_ '() '_) => '(constant 7 _))

(check (compile 'hello '((hello) . ()) '() '*next*) => '(refer-local 0 *next*))
(check (compile 'hello '(() . (hello)) '() '*next*) => '(refer-free 0 *next*))
(check (compile 'hello '(() . (hello)) '(hello) '*next*) => '(refer-free 0 (indirect *next*)))
(check (compile 'hello '((hello) . ()) '(hello) '*next*) => '(refer-local 0 (indirect *next*)))
(check (compile '(quote hello) '_ '_ '*next*) => '(constant hello *next*))

(check (compile '(lambda (a) a) '((a) . ()) '() '*next*)
       => '(close 0 (refer-local 0 (return 1)) *next*))

(check (compile '(lambda (a) b) '((b) . ()) '() '*next*)
       => '(refer-local 0 (argument (close 1 (refer-free 0 (return 1)) *next*))))

(check (compile '(lambda (a) b) '(() . (b)) '() '*next*)
       => '(refer-free 0 (argument (close 1 (refer-free 0 (return 1)) *next*))))

(check (compile '(lambda (a) (set! b 10)) '((b) . ()) '() '*next*)
       => '(refer-local 0 (argument (close 1 (constant 10 (assign-free 0 (return 1))) *next*))))

(check (compile '(lambda (a) (set! b 10)) '(() . (b)) '() '*next*)
       => '(refer-free 0 (argument (close 1 (constant 10 (assign-free 0 (return 1))) *next*))))

;; box test
(check (compile '(lambda (b) (lambda (a) (set! b 10))) '() '() '*next*)
       => '(close 0 (box 0 (refer-local 0 (argument
	     (close 1 (constant 10 (assign-free 0 (return 1))) (return 1))))) *next*))

(check (compile '(if #t 3 4) '_ '_ '*next*)
       => '(constant #t (test (constant 3 *next*) (constant 4 *next*))))

(check (compile '(call/cc (lambda (c) c)) '_ '() '*next*)
       => '(frame *next*
		  (conti (argument (close 0 (refer-local 0 (return 1)) (apply))))))
      
(check (compile '(func 1 2) '((func) . ()) '() '(halt))
       => '(frame (halt)
		  (constant 2 (argument (constant 1 (argument (refer-local 0 (apply))))))))
      
(check (compile '(func 1 2) '((func) . ()) '() '(return))
       => '(frame (return) ;; no tail call
		  (constant 2 (argument (constant 1 (argument (refer-local 0 (apply))))))))

;;; Tests for free variable

(check (index-closure #(*func* 0 1 2) 2) => 2)
(check (closure-body #(*func* 0 1 2)) => '*func*)

(set! stack #(5 4 3 2 1))
(check (closure '*body* 3 5) => #(*body* 1 2 3))

;;; Tests for VM

(check (VM 7 '(halt) 0 '_ 0) => 7)

(set! stack #(3 2 1 0 *stack-top* _))

(check (VM '_ '(refer-local 1 (halt)) 4 '_ 5) => 1)
(check (VM '_ '(refer-free 2 (halt)) '_ #(*func* 0 1 2) 5) => 2)
(check (VM '(hello) '(indirect (halt)) '_ '_ 5) => 'hello)
(check (VM '_ '(constant 7 (halt)) '_ '_ 5) => 7)

;; box
(check (VM '*acc* '(box 1 (halt)) '_ '_ 5) => '*acc*)
(check stack => #(3 2 1 (0) *stack-top* _))

(check (VM 7 '(assign-local 0 (halt)) '4 '_ 5) => 7)
(check stack => #(3 2 1 (7) *stack-top* _))

(set! stack #())
(check (VM 7 '(assign-free 2 (refer-free 2 (indirect (halt)))) '_ #(*func* 0 1 (2)) 0)
       => 7)

;; close test
(set! stack #(9 8 7 6 5 4 3 2 1))
(check (VM '_ '(close 4 (refer-local 0 (return 3)) (halt)) '_ '_ 6)
       => #((refer-local 0 (return 3)) 4 5 6 7))

;; test test
(check (VM #t '(test (constant 1 (halt)) (constant 2 (halt))) '_ '_ 0) => 1)
(check (VM #f '(test (constant 1 (halt)) (constant 2 (halt))) '_ '_ 0) => 2)

;; conti test
;; return doesn't need to pop its argument (refer-local 0) because
;; nuate modifies entire stack.
(check (VM '_ '(conti (halt)) '_ '_ 5)
       => #((refer-local 0 (nuate #(9 8 7 6 5) (return 0)))))

(check (continuation 3) => #((refer-local 0 (nuate #(9 8 7) (return 0)))))

;; nuate test
(set! stack #(_ _ _))
(check (VM '*acc* `(nuate ,#(some stack values) (*next*)) '*frame* '*closure* 0)
       => '(invalid-state *acc* (*next*) *frame* *closure* 3))
(check stack => #(some stack values))

;; frame test
(set! stack #(_ _ _))
(VM '() '(frame (*resume-inst*) (halt)) 0 0 0)
(check stack => #(0 0 (*resume-inst*)))

;; argument test
(set! stack #(_ _))
(check (VM 7 '(argument (halt)) '_ '_ 0) => 7)
(check stack => #(7 _))

;; apply test
(set! stack #(*frame* *closure* (halt) *local1* *local0*))
(check (VM #((refer-local 1 (return 2))) '(apply) '_ '_ 5) => '*local1*)

;; return test
(set! stack #(*old-frame* *old-closure* (halt) *arg2* *arg1* *arg0*))
(check (VM 7 '(return 3) '*frame* '*closure* 6) => 7)

;;; Tests for evaluate

(set! stack (make-vector 1000))

(check (evaluate 7) => 7)
(check (evaluate '(quote hello)) => 'hello)
(check (evaluate '((lambda () 7))) => 7)
(check (evaluate '((lambda (x y) y) 6 7)) => 7)
(check (evaluate '(if #t 7 0)) => 7)
(check (evaluate '(if #f 0 7)) => 7)
(check (evaluate '(call/cc (lambda (c) (0 3 (c 7))))) => 7)
(check (evaluate '((lambda (f x) (f x)) (lambda (x) x) 7)) => 7)

(check (evaluate '((lambda (a) a) 0)) => 0)
(check (evaluate '((lambda (a) ((lambda (b c) c) 5 7) ) 0)) => 7)

(check (evaluate '((lambda (t) ((lambda (x) t) (set! t 7))) 0)) => 7)
;; evaluation seems to be right to left in a function application
(check (evaluate '((lambda (a) ((lambda (b c) b) a (set! a 7))) 'init)) => 7)
