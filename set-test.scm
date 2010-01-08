#!/usr/bin/env gosh

(require "./set")
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
