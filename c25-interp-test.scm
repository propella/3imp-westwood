#!/usr/bin/env gosh

(require "./check")
(require "./c25-interp")

(check (lookup 'a '(((a) . (1)) ((a b c) . (2 3 4)))) => '(1))
(check (lookup 'b '(((a) . (1)) ((a b c) . (2 3 4)))) => '(3 4))
(check (lookup 'c '(((a) . (1)) ((a b c) . (2 3 4)))) => '(4))

(check (extend '() '(a b c) '(1 2 3)) => '(((a b c) . (1 2 3))))

(check (exec 'b '(((a b c) . (6 7 8)))) => 7)
(check (exec '((lambda (y) x) (set! x 7)) '(((x) . (0)))) => 7)

(check (meta '(quote hello)) => 'hello)

;; Meta makes a lambda accepting a list of arguments from lambda expression.
(check ((meta '(lambda (x y) y)) '(6 7)) => 7)
(check (meta '((lambda (x y) y) 6 7)) => 7)

(check (meta '(if #t 7 something)) => 7)
(check (meta '(if #f something 7)) => 7)

(check (meta '(call/cc (lambda (c) (something 3 (c 7))))) => 7)

(check (meta '((lambda (f x) (f x)) (lambda (x) x) 7)) => 7)
(check (exec '(+ 3 4) `(((+) . (,(lambda (xs) (apply + xs)))))) => 7)

(check (meta 7) => 7)
