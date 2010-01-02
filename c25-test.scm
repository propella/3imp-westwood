#!/usr/bin/env gosh

(require "./c25")

(use gauche.test)

;; The environment is a list of pairs of list, two lists in each pair
;; being a list of variables and a corresponding list of values. (p40)
;; Lookup returns the list containing the value as its first element. (p41)
(test* "lookup" '(1)   (lookup 'a '(((a) . (1)) ((a b c) . (2 3 4)))))
(test* "lookup" '(3 4) (lookup 'b '(((a) . (1)) ((a b c) . (2 3 4)))))
(test* "lookup" '(4)   (lookup 'c '(((a) . (1)) ((a b c) . (2 3 4)))))

;; Extend builds the new environment by creating a pair from the
;; variable and value ribs. (p42)
(test* "extend" '(((a b c) .  (1 2 3))) (extend '() '(a b c) '(1 2 3)))



(test-end)
