;; 2.5 A Meta-Circular Interpreter

(require "./c21-prims")

;; The environment is a list of pairs of list, two lists in each pair
;; being a list of variables and a corresponding list of values. (p40)
;; Lookup returns the list containing the value as its first element. (p41)

(define lookup
  (lambda (var e)
    (recur nxtrib ([e e])
	   (recur nxtelt ([vars (caar e)] [vals (cdar e)])
		  (cond
		   [(null? vars) (nxtrib (cdr e))]
		   [(eq? (car vars) var) vals]
		   [else (nxtelt (cdr vars) (cdr vals))])))))

;; Extend builds the new environment by creating a pair from the
;; variable and value ribs. (p42)

(define extend
  (lambda (env vars vals)
    (cons (cons vars vals) env)))

(define meta
  (lambda (exp)
    (exec exp '())))

;; Exec takes an expression exp and an environment env as input, and
;; perform the evaluation. (p40)

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

	 ;; apply
 	 [else
 	  ((exec (car exp) env)
 	   (map (lambda (x) (exec x env)) (cdr exp)))])]

      ;; itself
     [else exp])))

