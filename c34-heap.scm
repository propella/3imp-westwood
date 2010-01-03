;; 3.4 Implementing the Heap-Based Model

(require "./c21-prims")

;; Both applications and call/cc expressions are treated slightly
;; differently if they appear in the tail position. (p59)
(define tail?
  (lambda (next)
    (eq? (car next) 'return)))

;; 3.4.2 Translation. (p56)

(define compile
  (lambda (x next)
    (cond
     [(symbol? x)
      (list 'refer x next)]
     [(pair? x)
      (record-case x
         [quote (obj)
	  (list 'constant obj next)]
	 [lambda (vars body)
	  (list 'close vars (compile body '(return)) next)]
	 [if (test then else)
	  (let ([thenc (compile then next)]
		[elsec (compile else next)])
	    (compile test (list 'test thenc elsec)))]
	 [set! (var x)
	  (compile x (list 'assign var next))]
	 [call/cc (x)
	  (let ([c (list 'conti
			 (list 'argument
			       (compile x '(apply))))])
	    (if (tail? next)
		c
		(list 'frame next c)))]
	 [else
	  (recur loop ([args (cdr x)]
		       [c (compile (car x) '(apply))])
		 (if (null? args)
		     (if (tail? next)
			 c
			 (list 'frame next c))
		     (loop (cdr args)
			   (compile (car args)
				    (list 'argument c)))))])]
     [else
      (list 'constant x next)])))


;; Extend builds the new environment by creating a pair from the
;; variable and value ribs. (p42, p62)
(define extend
  (lambda (env vars vals)
    (cons (cons vars vals) env)))

;; 3.4.3 Evaluation. (p59)

;; a: the accumulator
;; x: the next expression
;; e: the current environment
;; r: the current value rub
;; s: the current stack (p51)

(define VM
  (lambda (a x e r s)
    (record-case x
       [halt () a]
       [refer (var x)
	(VM (car (lookup var e)) x e r s)]
       [constant (obj x)
	(VM obj x e r s)]
       [close (vars body x)
	(VM (closure body e vars) x e r s)]
       [test (then else)
	(VM a (if a then else) e r s)]
       [assign (var x)
	(set-car! (lookup var e) a)
	(VM a x e r s)]
       [conti (x)
;(print (continuation s))
	(VM (continuation s) x e r s)] ;; Save stack to the accumulator.
       [nuate (s var)
	(VM (car (lookup var e)) '(return) e r s)]
       [frame (ret x)
	(VM a x e '() (call-frame ret e r s))]
       [argument (x)
	(VM a x e (cons a r) s)]
       [apply ()
	(record (body e vars) a
	  (VM a body (extend e vars r) '() s))]
       [return ()
	(record (x e r s) s
	   (VM a x e r s))]
       [else (error "undefined instruction" (car x))])))

;; Lookup is same as chapter 2.5

(define lookup
  (lambda (var e)
    (recur nxtrib ([e e])
	   (when (null? e) (error "lookup failed:" var))
	   (recur nxtelt ([vars (caar e)] [vals (cdar e)])
		  (cond
		   [(null? vars) (nxtrib (cdr e))]
		   [(eq? (car vars) var) vals]
		   [else (nxtelt (cdr vars) (cdr vals))])))))

;; Closure creates a new closure object, which is simply a list of a
;; body, an environment, and a list of variables. (p61)

(define closure
  (lambda (body e vars)
    (list body e vars)))

;; Continuation creates a new continuation object. (p61)
(define continuation
  (lambda (s)
    (closure (list 'nuate s 'v) '() '(v))))

;; Call-frame makes a list of its arguments, a return address, an
;; environment, a rib, and a stack, i.e., the next frame in the
;; stack. (p61)

(define call-frame
  (lambda (x e r s)
; (print 'call-frame (list x e r s))
    (list x e r s)))

;; Evaluate starts things off. (p62)
(define evaluate
  (lambda (x)
    (VM '() (compile x '(halt)) '() '() '())))
