;; 3.4 Implementing the Heap-Based Model

(require "./c21-prims")

;; Both applications and call/cc expressions are treated slightly
;; differently if they appear in the tail position. (p59)

(define tail?
  (lambda (next)
    (eq? (car next) 'return)))

;; Extends only adds one rib (names or values) to the enviroment.
;; This is used as well by the new virtual mathine given later, except
;; there rib refers to the value rib rather than to the variable
;; rib. (p65)

(define extend
  (lambda (env rib)
    (cons rib env)))

;; Compile-lookup returns a pair of the rib and element indices. (p65)

(define compile-lookup
  (lambda (var e)
    (recur nxtrib ([e e] [rib 0])
	   (when (null? e) (error "lookup failed:" var))
	   (recur nxtelt ([vars (car e)] [elt 0])
		  (cond
		   [(null? vars) (nxtrib (cdr e) (+ rib 1))]
		   [(eq? (car vars) var) (cons rib elt)]
		   [else (nxtelt (cdr vars) (+ elt 1))])))))

;; 3.5.2 Translation. (p64)

(define compile
  (lambda (x e next)
    (cond
     [(symbol? x)
      (list 'refer (compile-lookup x e) next)]
     [(pair? x)
      (record-case x
         [quote (obj)
	  (list 'constant obj next)]
	 [lambda (vars body)
	  (list 'close
		(compile body (extend e vars) '(return))
		next)]
	 [if (test then else)
	  (let ([thenc (compile then e next)]
		[elsec (compile else e next)])
	    (compile test e (list 'test thenc elsec)))]
	 [set! (var x)
	  (let ([access (compile-lookup var e)])
	    (compile x e (list 'assign access next)))]
	 [call/cc (x)
	  (let ([c (list 'conti
			 (list 'argument
			       (compile x e '(apply))))])
	    (if (tail? next)
		c
		(list 'frame next c)))]
	 [else
	  (recur loop ([args (cdr x)]
		       [c (compile (car x) e '(apply))])
		 (if (null? args)
		     (if (tail? next)
			 c
			 (list 'frame next c))
		     (loop (cdr args)
			   (compile (car args)
				    e
				    (list 'argument c)))))])]
     [else
      (list 'constant x next)])))

;; Evaluation. (p66)

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
       [close (body x)
	(VM (closure body e) x e r s)]
       [test (then else)
	(VM a (if a then else) e r s)]
       [assign (var x)
	(set-car! (lookup var e) a)
	(VM a x e r s)]
       [conti (x)
	(VM (continuation s) x e r s)]
       [nuate (s var)
	(VM (car (lookup var e)) '(return) e r s)]
       [frame (ret x)
	(VM a x e '() (call-frame ret e r s))]
       [argument (x)
	(VM a x e (cons a r) s)]
       [apply ()
	(record (body e) a
	  (VM a body (extend e r) '() s))]
       [return ()
	(record (x e r s) s
	   (VM a x e r s))]
       [else (error "undefined instruction" (car x))])))

;; Closure now takes only a body and an environment. (p66)

(define closure
  (lambda (body e)
    (list body e)))

;; Continuation includes an explicit reference to the first argument
;; of the closet rib, (0 . 0). (p67)

(define continuation
  (lambda (s)
    (closure (list 'nuate s '(0 . 0)) '())))

;; Lookup simply moves directly to the specified rib and returns the
;; specified list cell within that rib. (p67)

(define lookup
  (lambda (access e)
    (recur nxtrib ([e e] [rib (car access)])
       (if (= rib 0)
	   (recur nxtelt ([r (car e)] [elt (cdr access)])
	      (if (= elt 0)
		  r
		  (nxtelt (cdr r) (- elt 1))))
	   (nxtrib (cdr e) (- rib 1))))))

;; Call-frame makes a list of its arguments, a return address, an
;; environment, a rib, and a stack, i.e., the next frame in the
;; stack. (p61)

(define call-frame
  (lambda (x e r s)
    (list x e r s)))

;; Evaluate starts things off. (p62)
(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) '() '() '())))
