(require "./c21-prims")

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
