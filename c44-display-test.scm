#!/usr/bin/env gosh

(require "./c44-display")
(require "./check")

;;; Tests for set membership and set union operations.

(check (set-member? 1 '(1 2 3)) => #t)
(check (set-member? 0 '(1 2 3)) => #f)

(check (set-cons 1 '(1 2 3)) => '(1 2 3))
(check (set-cons 0 '(1 2 3)) => '(0 1 2 3))

(check (set-union '(1 2 3) '(2 3 4)) => '(1 2 3 4))
(check (set-union '(1 2) '(3 4)) => '(2 1 3 4))

(check (set-minus '(1 2 3) '(1 2)) => '(3))
(check (set-minus '(1 2 3) '(3 4 5)) => '(1 2))

(check (set-intersect '(1 2 3) '(3 4 5)) => '(3))
(check (set-intersect '(1 2) '(4 5)) => '())

;;; Tests for find-free

(check (find-free 'hello '(a b c)) => '(hello))
(check (find-free '(quote hello) '(a b c)) => '())
(check (find-free '(lambda (x) (x y)) '()) => '(y))
(check (find-free '(lambda (x) (x y)) '(y)) => '())
(check (find-free '(if x y z) '(x y)) => '(z))
(check (find-free '(call/cc (lambda (x) (x y))) '()) => '(y))
(check (find-free '(lambda (x) (func x y)) '()) => '(func y))
(check (find-free 7 '()) => '())

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

;; ;;; Tests for compile

;; (check (compile 7 '() '_) => '(constant 7 _))
;; (check (compile 'hello '((hello)) '_) => '(refer 0 0 _))
;; (check (compile '(quote hello) '() '_) => '(constant hello _))

;; (check (compile '(lambda (c) c) '() '(halt))
;;        => '(close (refer 0 0 (return 2)) (halt)))

;; (check (compile '(if #t 3 4) '() '_) =>
;;        '(constant #t (test (constant 3 _) (constant 4 _))))

;; (check (compile '(call/cc (lambda (c) c)) '() '(halt))
;;        => '(frame (halt)
;; 		  (conti (argument (close (refer 0 0 (return 2))
;; 					  (apply))))))

;; (check (compile '(call/cc (lambda (c) c)) '() '(return))
;;        => '(frame (return) ;; no tail call
;; 		  (conti (argument (close (refer 0 0 (return 2))
;; 					  (apply))))))
      
;; (check (compile '(func 1 2) '((func)) '(halt))
;;        =>  '(frame (halt)
;; 		   (constant 2 (argument (constant 1 (argument (refer 0 0 (apply))))))))
      
;; (check (compile '(func 1 2) '((func)) '(return))
;;        =>  '(frame (return)
;; 		   (constant 2 (argument (constant 1 (argument (refer 0 0 (apply))))))))

;; ;; ;;; Tests for the dynamic chain

;; (set! stack #(some stack values))

;; (check (continuation 3) => 
;;        `((refer 0 0 (nuate ,#(some stack values) (return 0))) ()))

;; ;;; Tests for VM

;; ;; halt, refer, and constant test
;; ;             0 1 2 3 4 5 6 7 8 9 10 11 12
;; ;             | | |       |     |
;; (set! stack #(f 0 1 e d c 2 b a 6 () () ()))
;; (check (VM 7 '(halt) '_ 0) => 7)
;; (check (VM 0 '(refer 1 1 (halt)) 9 10) => 'd)
;; (check (VM '() '(constant 7 (halt)) '_ 0) => 7)

;; ;; close test
;; (check (VM '() '(close (refer (0 . 0) (return)) (halt)) 0 0)
;;        => '((refer (0 . 0) (return)) 0))

;; ;; test test
;; (check (VM #t '(test (constant 1 (halt)) (constant 2 (halt))) '_ 0) => 1)
;; (check (VM #f '(test (constant 1 (halt)) (constant 2 (halt))) '_ 0) => 2)

;; ;; conti test
;; (check (VM '_ '(conti (halt)) '_ 0) => '((refer 0 0 (nuate #() (return 0))) ()))

;; ;; nuate test
;; (set! stack #(_ _ _))
;; (check (VM 7 `(nuate ,#(some stack values) (halt)) 1 1) => 7)
;; (check stack => #(some stack values))

;; ;; frame test
;; (set! stack #(_ _))
;; (VM '() '(frame (*resume-inst*) (halt)) 0 0)
;; (check stack => #(0 (*resume-inst*)))

;; ;; argument test
;; (set! stack #(_ _))
;; (check (VM 7 '(argument (halt)) 0 0) => 7)
;; (check stack => #(7 _))

;; ;; apply test
;; (set! stack #(*dynamic* (halt) *arg1* *arg0* _))
;; (check (VM '((refer 0 1 (return 3)) 0) '(apply) '*env* 4) => '*arg1*)

;; ;; return test
;; (set! stack #(*dynamic* (halt) *arg2* *arg1* *arg0* *static*))
;; (check (VM 7 '(return 4) '*env* 6) => 7)

;; ;; Trace a behavior of ((lambda (a) a) 7)
;; ;; *dynamic* and *static* are real numbers in real situation, but they
;; ;; are not recognized by the VM.

;; ;(set! *debug-flag* #t)
;; ;(check (evaluate '((lambda (a) a) 7)) => 7)
;; ;(set! *debug-flag* #f)

;; (set! stack #(_ _ _ _))
;; (check (VM '_ '(frame (halt) (constant 7 (argument (close (refer 0 0 (return 2)) (apply))))) 0 0) => 7)

;; (set! stack #(*dynamic* (halt) _ _))
;; (check (VM '_ '(constant 7 (argument (close (refer 0 0 (return 2)) (apply)))) 0 2) => 7)

;; (set! stack #(*dynamic* (halt) _ _))
;; (check (VM 7 '(argument (close (refer 0 0 (return 2)) (apply))) 0 2) => 7)

;; (set! stack #(*dynamic* (halt) 7 _))
;; (check (VM '_ '(close (refer 0 0 (return 2)) (apply)) 0 3) => 7)

;; (set! stack #(*dynamic* (halt) 7 _))
;; (check (VM '((refer 0 0 (return 2)) 0) '(apply) 0 3) => 7)

;; (set! stack #(*dynamic* (halt) 7 *static*))
;; (check (VM '_ '(refer 0 0 (return 2)) 3 4) => 7)

;; (set! stack #(*dynamnic* (halt) 7 *static*))
;; (check (VM 7 '(return 2) 3 4) => 7)

;; (set! stack #())
;; (check (VM 7 '(halt) 0 0) => 7)

;; ;; ;;; Tests for evaluate

;; (set! stack (make-vector 1000))

;; (check (evaluate 7) => 7)
;; (check (evaluate '(quote hello)) => 'hello)
;; (check (evaluate '((lambda () 7))) => 7)
;; (check (evaluate '((lambda (x y) y) 6 7)) => 7)

;; (check (evaluate '(if #t 7 0)) => 7)
;; (check (evaluate '(if #f 0 7)) => 7)
;; ;; (check (evaluate '((lambda (t) ((lambda (x) t) (set! t 7))) 0)) => 7)
;; (check (evaluate '(call/cc (lambda (c) (0 3 (c 7))))) => 7)
;; (check (evaluate '((lambda (f x) (f x)) (lambda (x) x) 7)) => 7)
