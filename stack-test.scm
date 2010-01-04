#!/usr/bin/env gosh

(require "./stack")
(require "./check")

(define s 0)
(check (set! s (push 5 s)) => 1)
(check (set! s (push 6 s)) => 2)
(check (set! s (push 7 s)) => 3)
(check (index s 0) => 7)
(check (index s 2) => 5)
(index-set! s 0 'hello)
(check (index s 0) => 'hello)

;             0 1 2 3 4 5 6 7 8 9 10 11 12
;             | | |       |     |
(set! stack #(f 0 1 e d c 2 b a 6 () () ()))

(check (find-link 0 9) => 9)
(check (find-link 1 9) => 6)
(check (find-link 2 9) => 2)
(check (find-link 3 9) => 1)

(check (find-link 0 6) => 6)
(check (find-link 1 6) => 2)
(check (find-link 2 6) => 1)

(check (save-stack 3) => #(f 0 1))
