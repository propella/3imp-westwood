;; Chapter 2.1.3 Syntactic Extensions

;; recur is used to implement a loop (same as a named let) (p26)

(define recur let)

;; Record binds a set of variables to the element of a list (this list
;; must contain as many elements as there are variables. (p27)
;; (this is just another form of "let" expression).

(define-syntax record
  (syntax-rules ()
    ((record (var ...) val exp ...)
     (apply (lambda (var ...) exp ...) val))))

;; The cecord-case syntactic extension is a special purpose
;; combination of cond and record. It is useful for restructuring a
;; record based on the "key" that appears as the record's first
;; element. (p28)

(define-syntax record-case
  (syntax-rules (else)
    ((record-case exp1 [else expr3 ...])
     (begin expr3 ...))
    ((record-case exp1 [key vars exp2 ...] case2 ...)
     (let ([r exp1])
       (if (eq? (car r) (quote key))
	   (record vars (cdr r) exp2 ...)
	   (record-case exp1 case2 ...))))))

;;;; Debug print

(define *debug-flag* #f)

(define debug
  (lambda args
    (when *debug-flag*
	  (display (apply format args)))))

;; Useful macro for debug print
;; (debug-with (some) (thing) (to) (do))

(define-syntax debug-with
  (syntax-rules ()
    ((debug-with exp1 ...)
     (begin
       (set! *debug-flag* #t)
       exp1 ...
       (set! *debug-flag* #f)))))
