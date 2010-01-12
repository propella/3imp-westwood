;; 4.6 Tail Calls

(require "./c21-prims")
(require "./stack")
(require "./set")
(require "./c45-assign")

;; same as c35

(define tail?
  (lambda (next)
    (eq? (car next) 'return)))

;; (p110)

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
	  (let ([c (list 'conti
			 (list 'argument
			       (compile x e s
					(if (tail? next)
					    (list 'shift
						  1
						  (cadr next)
						  '(apply))
					    '(apply)))))])
	    (if (tail? next)
		c
		(list 'frame next c)))]
	 [else
	  (recur loop ([args (cdr x)]
		       [c (compile (car x) e s
			     (if (tail? next)
				 (list 'shift
				       (length (cdr x))
				       (cadr next)
				       '(apply))
				 '(apply)))])
		 (if (null? args)
		     (if (tail? next)
			 c
			 (list 'frame next c))
		     (loop (cdr args)
			   (compile (car args)
				    e
				    s
				    (list 'argument c)))))])]
     [else (list 'constant x next)])))

;; (shift n m x) The shift instruction moves the top n stack elements
;; m places down the stack. The shifting is performed by the function
;; shift-args. (p111)

(define shift-args
  (lambda (n m s)
    (recur nxtarg ([i (- n 1)])
       (unless (< i 0)
	       (index-set! s (+ i m) (index s i))
	       (nxtarg (- i 1))))
    (- s m)))

;; (p112)
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
       [shift (n m x)
	(VM a x f c (shift-args n m s))]
       [apply ()
	(VM a (closure-body a) s a s)]
       [return (n)
	(let ([s (- s n)])
	  (VM a (index s 0) (index s 1) (index s 2) (- s 3)))]
       [else (list 'invalid-state a x f c s)])))
