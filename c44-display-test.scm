#!/usr/bin/env gosh

(require "./c44-display")
(require "./check")

;;; Tests for variable reference

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

(check (compile 7 '_ '_) => '(constant 7 _))

(check (compile 'hello '((hello) . ()) '_) => '(refer-local 0 _))
(check (compile 'hello '(() . (hello)) '_) => '(refer-free 0 _))

(check (compile '(quote hello) '_ '_) => '(constant hello _))

(check (compile '(lambda (x y) y) '() '(halt))
       => '(close 0 (refer-local 1 (return 2)) (halt)))

(check (compile '(lambda (x y) y) '(() . (y)) '(halt))
       => '(close 0 (refer-local 1 (return 2)) (halt)))

(check (compile '(lambda (x y) z) '(() . (z)) '(halt))
       => '(refer-free 0 (argument (close 1 (refer-free 0 (return 2)) (halt)))))

(check (compile '(lambda (x y) z) '((z)) '(halt))
       => '(refer-local 0 (argument (close 1 (refer-free 0 (return 2)) (halt)))))

(check (compile '(if #t 3 4) '() '_) =>
       '(constant #t (test (constant 3 _) (constant 4 _))))

(check (compile '(call/cc (lambda (c) c)) '() '(halt))
       => '(frame (halt)
		  (conti (argument (close 0 (refer-local 0 (return 1)) (apply))))))

(check (compile '(call/cc (lambda (c) c)) '() '(return))
       => '(frame (return) ;; non tail call yet
		  (conti (argument (close 0 (refer-local 0 (return 1)) (apply))))))
      
(check (compile '(func 1 2) '((func)) '(halt))
       => '(frame (halt)
		  (constant 2 (argument (constant 1 (argument (refer-local 0 (apply))))))))
      
(check (compile '(func 1 2) '((func)) '(return))
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

;; refer-local points value from the frame register (= 4)
(check (VM '_ '(refer-local 1 (halt)) 4 '_ 5) => 1)

;; refer-free points from the closure (= #(*func* 0 1 2))
(check (VM '_ '(refer-free 2 (halt)) '_ #(*func* 0 1 2) 5) => 2)

(check (VM '_ '(constant 7 (halt)) '_ '_ 5) => 7)

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

;; Trace a behavior of ((lambda (a) a) 7)
;; *frame* is a number and *closure* is a global closure.
;; are not recognized by the VM.

(set! stack (make-vector 1000))

(set! stack #(_ _ _ _))
(check (VM '_ '(frame (halt) (constant 7 (argument (close 0 (refer-local 0 (return 1)) (apply))))) '*frame* '*closure* 0) => 7)

(set! stack #(*closure* *frame* (halt) '_))
(check (VM '_ '(constant 7 (argument (close 0 (refer-local 0 (return 1)) (apply)))) '*frame* '*closure* 3) => 7)

(set! stack #(*frame* *closure* (halt) '_))
(check (VM 7 '(argument (close 0 (refer-local 0 (return 1)) (apply))) '*frame* '*closure* 3) => 7)

(set! stack #(*frame* *closure* (halt) 7))
(check (VM '_ '(close 0 (refer-local 0 (return 1)) (apply)) '*frame* '*closure* 4) => 7)

(set! stack #(*frame* *closure* (halt) 7))
(check (VM #((refer-local 0 (return 1))) '(apply) '*frame* '*closure* 4) => 7)

(set! stack #(*frame* *closure* (halt) 7))
(check (VM '_ '(refer-local 0 (return 1)) 4 #((refer-local 0 (return 1))) 4) => 7)

(set! stack #(*frame* *closure* (halt) 7))
(check (VM 7 '(return 1) 4 #((refer-local 0 (return 1))) 4) => 7)

(set! stack #())
(check (VM 7 '(halt) '*frame* '*closure* 0) => 7)

;; ;; ;;; Tests for evaluate

(set! stack (make-vector 1000))

(check (evaluate 7) => 7)
(check (evaluate '(quote hello)) => 'hello)
(check (evaluate '((lambda () 7))) => 7)
(check (evaluate '((lambda (x y) y) 6 7)) => 7)

(check (evaluate '(if #t 7 0)) => 7)
(check (evaluate '(if #f 0 7)) => 7)
; (check (evaluate '((lambda (t) ((lambda (x) t) (set! t 7))) 0)) => 7)
;(set! *debug-flag* #t)
(check (evaluate '(call/cc (lambda (c) (0 3 (c 7))))) => 7)
;(set! *debug-flag* #f)
(check (evaluate '((lambda (f x) (f x)) (lambda (x) x) 7)) => 7)
