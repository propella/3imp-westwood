#!/usr/bin/env gosh

(require "./check")
(require "./c21-prims")

;; The rec syntctic form allows the creation of self-recursive
;; closures. (p25) (srfi-31)

(check
 ((rec count
       (lambda (x)
	 (if (null? x)
	     0
	     (+ (count (cdr x)) 1)))) '(1 2 3 4 5)) => 5)

(check
 (recur count ([x '(a b c d e)])
	(if (null? x)
	    0
	    (+ (count (cdr x)) 1))) => 5)

(check (record (x y) '(3 4) (+ x y)) => 7)

(check (record (a b c) '(1 2 3) (list c b a)) => '(3 2 1))

(define calc
  (lambda (x)
    (if (integer? x)
	x
	(record-case x
	   (add (x y) (+ (calc x) (calc y)))
	   (mul (x y) (* (calc x) (calc y)))
	   (neg (x) (- 0 (calc x)))
	   (else (error "invalid expression"))))))

(check (calc '(neg (mul (add 3 4) 6))) => -42)
