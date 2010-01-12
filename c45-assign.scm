;; 4.5 Supporting Assignments

(require "./c21-prims")
(require "./stack")
(require "./set")

;; Find-sets looks for assignments to any of the set of variables
;; v. (p101)

(define find-sets
  (lambda (x v)
    (cond
     [(symbol? x) '()]
     [(pair? x)
      (record-case x
	 [quote (obj) '()]
	 [lambda (vars body)
	  (find-sets body (set-minus v vars))]
	 [if (test then else)
          (set-union (find-sets test v)
			(set-union (find-sets then v)
				   (find-sets else v)))]
	 [set! (var x)
           (set-union (if (set-member? var v) (list var) '())
		      (find-sets x v))]
	 [call/cc (exp) (find-sets exp v)]
	 [else
	  (recur next ([x x])
	     (if (null? x)
		 '()
		 (set-union (find-sets (car x) v)
			    (next (cdr x)))))])]
     [else '()])))

;; Make-boxes generates a code from a list of assigned variables
;; (sets) and a list of arguments (vars). (p102)

(define make-boxes
  (lambda (sets vars next)
    (recur f ([vars vars] [n 0])
       (if (null? vars)
	   next
	   (if (set-member? (car vars) sets)
	       (list 'box n (f (cdr vars) (+ n 1)))
	       (f (cdr vars) (+ n 1)))))))

;; (p103)

;; x: 
;; e: ((local variables) . (free variables))
;; s: a set of assigned free variables
;; next: next expression
		 
(define compile
  (lambda (x e s next)
    (debug "(compile ~a ~a ~a ~a)\n" x e s next)
    (cond
     [(symbol? x)
      (compile-refer x e
	 (if (set-member? x s)
	     (list 'indirect next)
	     next))]
     [(pair? x)
      (record-case x
         [quote (obj) (list 'constant obj next)]
	 [lambda (vars body)
          (let ([free (find-free body vars)]
		[sets (find-sets body vars)])
	    (collect-free free e
	       (list 'close
		     (length free)
		     (make-boxes sets vars
		        (compile body
				 (cons vars free)
				 (set-union
				  sets
				  (set-intersect s free))
				 (list 'return (length vars))))
		     next)))]
	 [if (test then else)
	  (let ([thenc (compile then e s next)]
		[elsec (compile else e s next)])
	    (compile test e s (list 'test thenc elsec)))]
	 [set! (var x)
	  (compile-lookup var e
	     (lambda (n)
	       (compile x e s (list 'assign-local n next)))
	     (lambda (n)
	       (compile x e s (list 'assign-free n next))))]
	 [call/cc (x)
	  (list 'frame
		next
		(list 'conti
		      (list 'argument
			    (compile x e s '(apply)))))]
	 [else
	  (recur loop ([args (cdr x)]
		       [c (compile (car x) e s '(apply))])
		 (if (null? args)
		     (list 'frame next c)
		     (loop (cdr args)
			   (compile (car args)
				    e
				    s
				    (list 'argument c)))))])]
     [else (list 'constant x next)])))

;; Same as c44 except supporting set!

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
	 [set! (var exp)
	  (set-union (if (set-member? var b) '() (list var))
		     (find-free exp b))]
	 [call/cc (exp) (find-free exp b)]
	 [else
	  (recur next ([x x])
	     (if (null? x)
		 '()
		 (set-union (find-free (car x) b)
			    (next (cdr x)))))])]
     [else '()])))

;; Same as c44 (p94)

(define collect-free
  (lambda (vars e next)
    (if (null? vars)
	next
	(collect-free (cdr vars) e
	   (compile-refer (car vars) e
	       (list 'argument next))))))

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

;; (p105)
;; a: the accumulator
;; x: the next expression
;; f: the frame
;; c: the closure
;; s: the current stack

(define VM
  (lambda (a x f c s)
    (debug "\nstack ~a\n" (stack-up-to s))
    (debug "(VM ~a ~a ~a ~a ~a)\n" a x f c s)
    (record-case x
       [halt () a]
       [refer-local (n x)
	(VM (index f n) x f c s)]
       [refer-free (n x)
        (VM (index-closure c n) x f c s)]
       [indirect (x)
	(VM (unbox a) x f c s)]
       [constant (obj x)
	(VM obj x f c s)]
       [close (n body x)
	(VM (closure body n s) x f c (- s n))]
       [box (n x)
	(index-set! s n (box (index s n)))
	(VM a x f c s)]
       [test (then else)
	(VM a (if a then else) f c s)]
       [assign-local (n x)
	(set-box! (index f n) a)
	(VM a x f c s)]
       [assign-free (n x)
        (set-box! (index-closure c n) a)
	(VM a x f c s)]
       [conti (x)
	(VM (continuation s) x f c s)]
       [nuate (stack x)
	(VM a x f c (restore-stack stack))]
       [frame (ret x)
	(VM a x f c (push ret (push f (push c s))))]
       [argument (x)
	(VM a x f c (push a s))]
       [apply ()
	(VM a (closure-body a) s a s)]
       [return (n)
	(let ([s (- s n)])
	  (VM a (index s 0) (index s 1) (index s 2) (- s 3)))]
       [else (list 'invalid-state a x f c s)])))

;; (106)

(define evaluate
  (lambda (x)
    (VM '() (compile x '() '() '(halt)) 0 '() 0)))

;; copied from c44

(define closure
  (lambda (body n s)
    (let ([v (make-vector (+ n 1))])
      (vector-set! v 0 body)
      (recur f ([i 0])
	 (unless (= i n)
	    (vector-set! v (+ i 1) (index s i))
	    (f (+ i 1))))
      v)))

;; copied from c44

(define closure-body
  (lambda (c)
    (vector-ref c 0)))

;; copied from c44

(define index-closure
  (lambda (c n)
    (vector-ref c (+ n 1))))

;; ;;;;;; Helper functions

;; copied from c44

(define continuation
  (lambda (s)
    (closure
     (list 'refer-local 0 (list 'nuate (save-stack s) '(return 0))) 0 0)))

;; Box primitives

(define box (lambda (x) (list x)))
(define unbox (lambda (x) (car x)))
(define set-box! (lambda (b x)  (set-car! b x)))
