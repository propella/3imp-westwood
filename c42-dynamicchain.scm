;; 4.2 Stack Allocating the Dynamic Chain

(require "./c21-prims")
(require "./c35-index") ;; tail?, extend, compile-lookup, lookup, compile
(require "./stack")

;; The conti instruction still creates a continuation, but it does so
;; with the help of the function save-stack. (p82)
;; It uses nil environment '().

(define continuation
  (lambda (s)
    (closure
     (list 'refer '(0 . 0) (list 'nuate (save-stack s) '(return)))
     '())))

;; Save-stack creates a Scheme vector to hold the stack, and copies
;; the current stack from its start (at index 0) to the current stack
;; pointer, passed as the argument s. (p83)

(define save-stack
  (lambda (s)
    (let ([v (make-vector s) ])
      (recur copy ([i 0])
	 (unless (= i s)
	    (vector-set! v i (vector-ref stack i))
	    (copy (+ i 1))))
      v)))

;;; nuate instruction, uses the help function restore-stack to restore
;;; the stack saved by saved-stack. (p83)

(define restore-stack
  (lambda (v)
    (let ([s (vector-length v)])
      (recur copy ([i 0])
	 (unless (= i s)
	    (vector-set! stack i (vector-ref v i))
	    (copy (+ i 1))))
	 s)))

;; (p83)
;; a: the accumulator
;; x: the next expression
;; e: the current environment
;; r: the current value rub
;; s: the current stack (p51)

(define VM
  (lambda (a x e r s)
    (debug "\nSTACK: ~a\n" (stack-up-to s))
    (debug "(VM ~a ~a ~a ~a ~a)\n" a x e r s)
    (record-case x
       [halt () a]
       [refer (var x)
	(VM (car (lookup var e)) x e r s)]
       [constant (obj x)
	(VM obj x e r s)]
       [close (body x)
	(VM (closure body e) x e r s)]
       [test (then else)
	(VM a (if a then else) e r s)]
       [assign (var x)
	(set-car! (lookup var e) a)
	(VM a x e r s)]
       [conti (x)
	(VM (continuation s) x e r s)]
       [nuate (stack x)
	(VM a x e r (restore-stack stack))]
       [frame (ret x)
	(VM a x e '() (push ret (push e (push r s))))]
       [argument (x)
	(VM a x e (cons a r) s)]
       [apply ()
	(record (body e) a
	  (VM a body (extend e r) '() s))]
       [return ()
	(VM a (index s 0) (index s 1) (index s 2) (- s 3))]
       [else (list 'invalid-state a x e s)])))

;; (p84)
(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) '() '() 0)))
