;; 4.1 Stack-Based Implementation of Block-Structured Languages

(require "./c21-prims")
(require "./stack")

;; Set membership and set union operation on lists. (p93)

(define set-member?
  (lambda (x s)
    (cond
     [(null? s) #f]
     [(eq? x (car s)) #t]
     [else (set-member? x (cdr s))])))

(define set-cons
  (lambda (x s)
    (if (set-member? x s)
	s
	(cons x s))))

(define set-union
  (lambda (s1 s2)
    (if (null? s1)
	s2
	(set-union (cdr s1) (set-cons (car s1) s2)))))

(define set-minus
  (lambda (s1 s2)
    (if (null? s1)
	'()
	(if (set-member? (car s1) s2)
	    (set-minus (cdr s1) s2)
	    (cons (car s1) (set-minus (cdr s1) s2))))))

(define set-intersect
  (lambda (s1 s2)
    (if (null? s1)
	'()
	(if (set-member? (car s1) s2)
	    (cons (car s1) (set-intersect (cdr s1) s2))
	    (set-intersect (cdr s1) s2)))))

;; Find-free uses a simple algorithm to return the set of free
;; variables of an expression x, given an initial set of bound
;; variables b. (p92)

(define find-free
  (lambda (x b)
    (cond
     [(symbol? x) (if (set-member? x b) '() (list x))]
     [(pair? x)
      (record-case x
	 [quote (obj) '()]
	 [lambda (vars body)
	   (find-free body (set-union vars b))]
	 [if (test then else)
	     (set-union (find-free test b)
			(set-union (find-free then b)
				   (find-free else b)))]
	 [call/cc (exp) (find-free exp b)]
	 [else
	  (recur next ([x x])
	     (if (null? x)
		 '()
		 (set-union (find-free (car x) b)
			    (next (cdr x)))))])]
     [else '()])))

;;; (p94)
		 

(define compile
  (lambda (x e next)
    (cond
     [(symbol? x) (compile-refer x e next)]
     [(pair? x)
      (record-case x
         [quote (obj) (list 'constant obj next)]
	 [lambda (vars body)
          (let ([free (find-free body vars)])
	    (collect-free free e
	       (list 'close
		     (length free)
		     (compile body
			      (cons vars free)
			      (list 'return
				    (length vars)))
		     next)))]
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
	 [else
	  (recur loop ([args (cdr x)]
		       [c (compile (car x) e '(apply))])
		 (if (null? args)
		     (list 'frame next c)
		     (loop (cdr args)
			   (compile (car args)
				    e
				    (list 'argument c)))))])]
     [else (list 'constant x next)])))

;; Collect-free collects these variables for inclusion in the
;; closure. collect-free arranges to push the value of each free
;; variable in turn on the stack. (p95)

(define collect-free
  (lambda (vars e next)
    (if (null? vars)
	next
	(collect-free (cdr vars) e
	   (compile-refer (car vars) e
	       (list 'argument next))))))

;; A compile-time environment (e) is a pair whose car is the list of
;; local variables and whose cdr is the list of free variables. (p95)

(define compile-refer
  (lambda (x e next)
    (compile-lookup x e
       (lambda (n) (list 'refer-local n next))
       (lambda (n) (list 'refer-free n next)))))

(define compile-lookup
  (lambda (x e return-local return-free)
    (recur nxtlocal ([locals (car e)] [n 0])
       (if (null? locals)
	   (recur nxtfree ([free (cdr e)] [n 0])
	      (when (null? free) (error "lookup failed:" x))
	      (if (eq? (car free) x)
		  (return-free n)
		  (nxtfree (cdr free) (+ n 1))))
	   (if (eq? (car locals) x)
	       (return-local n)
	       (nxtlocal (cdr locals) (+ n 1)))))))

;; ;;;;;; Helper functions

;; ;; Extends is from c35

;; (define extend
;;   (lambda (env rib)
;;     (cons rib env)))

;; ;; Closure is from c35

;; (define closure
;;   (lambda (body e)
;;     (list body e)))

;; ;; Evaluate is from c41
;; (define evaluate
;;   (lambda (x)
;;     (VM '() (compile x '() '(halt)) 0 0)))

;; ;; Compile-lookup is from c41

;; (define compile-lookup
;;   (lambda (var e return)
;;     (recur nxtrib ([e e] [rib 0])
;; 	   (when (null? e) (error "lookup failed:" var))
;; 	   (recur nxtelt ([vars (car e)] [elt 0])
;; 		  (cond
;; 		   [(null? vars) (nxtrib (cdr e) (+ rib 1))]
;; 		   [(eq? (car vars) var) (return rib elt)]
;; 		   [else (nxtelt (cdr vars) (+ elt 1))])))))

;; ;;;;;; Main functions

;; ;; (p86)

;; (define continuation
;;   (lambda (s)
;;     (closure
;;      (list 'refer 0 0 (list 'nuate (save-stack s) '(return 0)))
;;      '())))

;; ;; (p87)

;; ;; (p87)

;; ;; a: the accumulator
;; ;; x: the next expression
;; ;; e: the current environment
;; ;; s: the current stack

;; (define VM
;;   (lambda (a x e s)
;;     (debug "\nSTACK: ~a\n" (stack-up-to s))
;;     (debug "(VM ~a ~a ~a ~a)\n" a x e s)
;;     (record-case x
;;        [halt () a]
;;        [refer (n m x)
;; 	(VM (index (find-link n e) m) x e s)]
;;        [constant (obj x)
;; 	(VM obj x e s)]
;;        [close (body x)
;; 	(VM (closure body e) x e s)]
;;        [test (then else)
;; 	(VM a (if a then else) e s)]
;;        [conti (x)
;; 	(VM (continuation s) x e s)]
;;        [nuate (stack x)
;; 	(VM a x e (restore-stack stack))]
;;        [frame (ret x)
;; 	(VM a x e (push ret (push e s)))]
;;        [argument (x)
;; 	(VM a x e (push a s))]
;;        [apply ()
;; 	(record (body link) a
;; 	  (VM a body s (push link s)))]
;;        [return (n)
;; 	(let ([s (- s n)]) ;; old-stack = stack - (args + 1 + 2) : dynamic, next-inst, static
;; 	  ;;     next-inst   old-env     old-stack
;; 	  (VM a (index s 0) (index s 1) (- s 2)))]
;;        [else (list 'invalid-state a x e s)])))

