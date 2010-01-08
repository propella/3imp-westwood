;; 4.1 Stack-Based Implementation of Block-Structured Languages

(require "./c21-prims")
(require "./stack")
(require "./set")

;;; (p94)
		 
(define compile
  (lambda (x e next)
    (debug "(compile ~a ~a ~a)\n" x e next)
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

;; (p97)

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
       [constant (obj x)
	(VM obj x f c s)]
       [close (n body x)
	(VM (closure body n s) x f c (- s n))]
       [test (then else)
	(VM a (if a then else) f c s)]
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

;; Closure is responsible for building the vector representing the
;; display closure. (p97)

(define closure
  (lambda (body n s)
    (let ([v (make-vector (+ n 1))])
      (vector-set! v 0 body)
      (recur f ([i 0])
	 (unless (= i n)
	    (vector-set! v (+ i 1) (index s i))
	    (f (+ i 1))))
      v)))

;; Closure-body and index-closure reference the body or argument
;; values of a display clojure. (p98)

(define closure-body
  (lambda (c)
    (vector-ref c 0)))

(define index-closure
  (lambda (c n)
    (vector-ref c (+ n 1))))

;; (p98)

(define evaluate
  (lambda (x)
    (VM '() (compile x '() '(halt)) 0 '() 0)))

;;;;;; Helper functions

(define continuation
  (lambda (s)
    (closure
     (list 'refer-local 0 (list 'nuate (save-stack s) '(return 0))) 0 0)))
