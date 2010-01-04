;; 4.1 Stack-Based Implementation of Block-Structured Languages

(require "./c21-prims")
(require "./stack")

;;;;;; Helper functions

;; Extends is from c35

(define extend
  (lambda (env rib)
    (cons rib env)))

;; Closure is from c35

(define closure
  (lambda (body e)
    (list body e)))

;; Find-link is from c41

(define find-link
  (lambda (n e)
    (if (= n 0)
	e
	(find-link (- n 1) (index e -1)))))

;; Evaluate is from c41
(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) 0 0)))


;; Compile-lookup is from c41

(define compile-lookup
  (lambda (var e return)
    (recur nxtrib ([e e] [rib 0])
	   (when (null? e) (error "lookup failed:" var))
	   (recur nxtelt ([vars (car e)] [elt 0])
		  (cond
		   [(null? vars) (nxtrib (cdr e) (+ rib 1))]
		   [(eq? (car vars) var) (return rib elt)]
		   [else (nxtelt (cdr vars) (+ elt 1))])))))

;; copied from c42

(define save-stack
  (lambda (s)
    (let ([v (make-vector s) ])
      (recur copy ([i 0])
	 (unless (= i s)
	    (vector-set! v i (vector-ref stack i))
	    (copy (+ i 1))))
      v)))

;; copied from c42

(define restore-stack
  (lambda (v)
    (let ([s (vector-length v)])
      (recur copy ([i 0])
	 (unless (= i s)
	    (vector-set! stack i (vector-ref v i))
	    (copy (+ i 1))))
	 s)))

;;;;;; Main functions

;; (p86)

(define continuation
  (lambda (s)
    (closure
     (list 'refer 0 0 (list 'nuate (save-stack s) '(return 0)))
     '())))

;; (p87)

(define compile
  (lambda (x e next)
    (cond
     [(symbol? x)
      (compile-lookup x e
	 (lambda (n m)
	   (list 'refer n m next)))]
     [(pair? x)
      (record-case x
         [quote (obj)
	  (list 'constant obj next)]
	 [lambda (vars body)
	  (list 'close
		(compile body
			 (extend e vars) 
			 (list 'return (+ (length vars) 1)))
		next)]
	 [if (test then else)
	  (let ([thenc (compile then e next)]
		[elsec (compile else e next)])
	    (compile test e (list 'test thenc elsec)))]
	 [call/cc (x)
	  (list 'frame
		next
		(list 'conti
		      (list 'argument
			    (compile x e '(apply)))))]
;	 [set! (var x)
;	  (compile-lookup var e
;	     (lambda (n m)
;	       (compile x e (list 'assign n m next))))]
	 [else
	  (recur loop ([args (cdr x)]
		       [c (compile (car x) e '(apply))])
		 (if (null? args)
		     (list 'frame next c)
		     (loop (cdr args)
			   (compile (car args)
				    e
				    (list 'argument c)))))])]
     [else
      (list 'constant x next)])))

;; (p87)

;; a: the accumulator
;; x: the next expression
;; e: the current environment
;; s: the current stack

(define VM
  (lambda (a x e s)
    (debug "\nSTACK: ~a\n" (stack-up-to s))
    (debug "(VM ~a ~a ~a ~a)\n" a x e s)
    (record-case x
       [halt () a]
       [refer (n m x)
	(VM (index (find-link n e) m) x e s)]
       [constant (obj x)
	(VM obj x e s)]
       [close (body x)
	(VM (closure body e) x e s)]
       [test (then else)
	(VM a (if a then else) e s)]
       [conti (x)
	(VM (continuation s) x e s)]
       [nuate (stack x)
	(VM a x e (restore-stack stack))]
;       [assign (n m x)
;	(index-set! (find-link n e) m a)
;	(VM a x e s)]
       [frame (ret x)
	(VM a x e (push ret (push e s)))]
       [argument (x)
	(VM a x e (push a s))]
       [apply ()
	(record (body link) a
	  (VM a body s (push link s)))]
       [return (n)
	(let ([s (- s n)]) ;; old-stack = stack - (args + 1 + 2) : dynamic, next-inst, static
	  ;;     next-inst   old-env     old-stack
	  (VM a (index s 0) (index s 1) (- s 2)))]
       [else (list 'invalid-state a x e s)])))

