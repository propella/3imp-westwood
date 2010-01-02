#!/usr/bin/env gosh

(require "./c21-prims")
(use gauche.test)

;; The rec syntctic form allows the creation of self-recursive
;; closures. (p25) (srfi-31)
(test* "rec" 5
   ((rec count
      (lambda (x)
	(if (null? x)
	    0
	    (+ (count (cdr x)) 1)))) '(1 2 3 4 5)))

(test* "recur" 5
   (recur count ([x '(a b c d e)])
      (if (null? x)
	  0
	  (+ (count (cdr x)) 1))))

(test* "record" 7
   (record (x y) '(3 4) (+ x y)))

(test* "record" '(3 2 1)
   (record (a b c) '(1 2 3) (list c b a)))

(define calc
  (lambda (x)
    (if (integer? x)
	x
	(record-case x
	   (add (x y) (+ (calc x) (calc y)))
	   (mul (x y) (* (calc x) (calc y)))
	   (neg (x) (- 0 (calc x)))
	   (else (error "invalid expression"))))))

(test* "record-case calc" -42 (calc '(neg (mul (add 3 4) 6))))
(test-end)
