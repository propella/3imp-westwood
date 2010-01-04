;; The stack is implemented as a Scheme vector. (p 75)

(require "./c21-prims")

(define stack (make-vector 1000))

;; Push takes a stack pointer and an object and adds the object to the
;; top of the stack. It returns the updated stack pointer. (p75)

(define push
  (lambda (x s)
    (vector-set! stack s x)
    (+ s 1)))

;; Index takes a stack pointer and an index and returns the object
;; found at the specified offset from the stack pointer. (p75)

(define index
  (lambda (s i)
    (let ((p (- (- s i) 1)))
      (when (or (< p 0) (<= (vector-length stack) p)) (error "index: out of range" s i))
      (vector-ref stack p))))

;; Index-set! takes a stack pointer, an index, and an object and
;; places the object at the specified offset from the stack
;; pointer. (p75)

(define index-set!
  (lambda (s i v)
    (vector-set! stack (- (- s i) 1) v)))

;; Find-link receives two arguments, a number n and a frame pointer e,
;; and locates the nth frame (zero based) in the static frame starting
;; with e. (p80)

(define find-link
  (lambda (n e)
    (if (= n 0)
	e
	(find-link (- n 1) (index e -1)))))

;; Return a copy of the stack up to the stack pointer.

(define stack-up-to
  (lambda (s) (vector-copy stack 0 s)))


;; Save-stack creates a Scheme vector to hold the stack, and copies
;; the current stack from its start (at index 0) to the current stack
;; pointer, passed as the argument s. (p83)

(define save-stack
  (lambda (s)
    (let ([v (make-vector s) ])
      (recur copy ([i 0])
	 (unless (= i s)
	    (vector-set! v i (vector-ref stack i))
	    (copy (+ i 1))))
      v)))

;;; nuate instruction, uses the help function restore-stack to restore
;;; the stack saved by saved-stack. (p83)

(define restore-stack
  (lambda (v)
    (let ([s (vector-length v)])
      (recur copy ([i 0])
	 (unless (= i s)
	    (vector-set! stack i (vector-ref v i))
	    (copy (+ i 1))))
	 s)))
