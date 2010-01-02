;; (record (a b c) ls (+ a b c))
;(define-macro (record vars val . exps)
;              `(apply (lambda ,vars ,@exps) ,val))
;(define-macro (record val vars . exps) ; 3impのrecordの文法
;  `(apply (lambda ,vars ,@exps) ,val))

;(define-syntax record
;  (syntax-rules ()
;	((record (var ...) val exp ...)
;	 (apply (lambda (var ...) exp ...) val))))
;(define-syntax record
;  (syntax-rules ()
;	((record val (var ...) exp ...)
;	 (apply (lambda (var ...) exp ...) val))))

(use util.match)
(define record match-let1)

;(record (x y z) '(1 2 3) (list z y x))

;; (record-case exp1
;;   [key vars exp2..]
;;   ...
;;   [else exp3..]
(define-syntax record-case
  (syntax-rules (else)
    ((record-case exp1 [else exp3 exp4 ...]) ; for else
     (begin exp3 exp4 ...))
    ((record-case exp1 [key vars exp2 ...])  ; only one case
     (record vars (cdr exp1) exp2 ...))
    ((record-case exp1 [key vars exp2 ...] case2 ...) ; more than one case 
     (let ([r exp1])
       (if (eq? (car r) (quote key))
         (record vars (cdr r) exp2 ...)
         (record-case exp1 case2 ...))))))

(define recur let)
