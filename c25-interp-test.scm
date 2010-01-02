#!/usr/bin/env gosh

(use gauche.test)
(require "./c25-interp")

(test* "lookup" '(1)   (lookup 'a '(((a) . (1)) ((a b c) . (2 3 4)))))
(test* "lookup" '(3 4) (lookup 'b '(((a) . (1)) ((a b c) . (2 3 4)))))
(test* "lookup" '(4)   (lookup 'c '(((a) . (1)) ((a b c) . (2 3 4)))))

(test* "extend" '(((a b c) .  (1 2 3))) (extend '() '(a b c) '(1 2 3)))

(test* "exec symbol?" 7 (exec 'b '(((a b c) . (6 7 8)))))
(test* "exec set!" 7 (exec '((lambda (y) x) (set! x 7)) '(((x) . (0)))))

(test* "meta quote" 'hello (meta '(quote hello)))

;; Meta makes a lambda accepting a list of arguments from lambda expression.
(test* "meta lambda" 7 ((meta '(lambda (x y) y)) '(6 7)))
(test* "meta lambda" 7 (meta '((lambda (x y) y) 6 7)))

(test* "meta if #t" 7 (meta '(if #t 7 something)))
(test* "meta if #f" 7 (meta '(if #f something 7)))

(test* "call/cc" 7 (meta '(call/cc (lambda (c) (something 3 (c 7))))))

(test* "apply" 13 (meta '((lambda (f x) (f x)) (lambda (x) x) 13)))
(test* "apply" 7 (exec '(+ 3 4) `(((+) . (,(lambda (xs) (apply + xs)))))))

(test* "itself" 7 (meta 7))
(test-end)
