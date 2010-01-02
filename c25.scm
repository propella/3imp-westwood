;; 2.5 A Meta-Circular Interpreter

(require "./3imp")


(define extend
  (lambda (env vars vals)
    (cons (cons vars vals) env)))

(define meta
  (lambda (exp)
    (exec exp '())))

(define exec
  (lambda (exp env)
    (cond
     [(symbol? exp) (car (lookup exp env))]
      [(pair? exp)
       (record-case exp
 	 [quote (obj) obj]
	 [lambda (vars body)
	   (lambda (vals)
	     (exec body (extend env vars vals)))]
	 [if (test then else)
	     (if (exec test env)
		 (exec then env)
		 (exec else env))]
	 [set! (var val)
	       (set-car! (lookup var env) (exec val env))]
 	 [call/cc (exp)
 		  (call/cc
 		   (lambda (k)
 		     ((exec exp env)
 		      (list (lambda (args) (k (car args)))))))]
 	 [call/cc (exp) (call/cc (exec exp env))]
 	 [else
 	  ((exec (car exp) env)
 	   (map (lambda (x) (exec x env)) (cdr exp)))])]
     [else exp])))

(define lookup
  (lambda (var e)
    (recur nxtrib ([e e])
	   (recur nxtelt ([vars (caar e)] [vals (cdar e)])
		  (cond
		   [(null? vars) (nxtrib (cdr e))]
		   [(eq? (car vars) var) vals]
		   [else (nxtelt (cdr vars) (cdr vals))])))))
