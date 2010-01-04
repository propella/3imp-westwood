;; The stack is implemented as a Scheme vector. (p 75)

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

;; Return a copy of the stack up to the stack pointer.

(define stack-up-to
  (lambda (s) (vector-copy stack 0 s)))
