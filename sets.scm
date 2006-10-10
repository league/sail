(define empty-set null)

(define (:element-of? x s eq)
  (if (null? s) #f
      (or (eq x (car s))
          (:element-of? x (cdr s) eq))))

(define (:add-to-set x s eq)
  (if (:element-of? x s eq)
      s
      (cons x s)))

(define (:add-to-set* xs s eq)
  (if (null? xs) s
      (:add-to-set* (cdr xs) (:add-to-set (car xs) s eq) eq)))

(define (:remove-from-set x s eq)
  (cond
    ((null? s) null)
    ((eq x (car s)) (cdr s))
    (else (cons (car s) (:remove-from-set x (cdr s) eq)))))

(define (:remove-from-set* xs s eq)
  (if (null? xs) s
      (:remove-from-set (cdr x) (:remove-from-set* (car x) s eq) eq)))

(define (:subset? s t eq)
  (if (null? s) #t
      (and (:element-of? (car s) t eq)
           (:subset? (cdr s) t eq))))

(define (:set=? s t eq)
  (and (:subset? s t eq)
       (:subset? t s eq)))

(define (optional-eq f)  
  (lambda args
    (if (< (length args) (procedure-arity f))
        (apply f (append args (list equal?)))
        (apply f args))))

(define element-of?      (optional-eq :element-of?))
(define add-to-set       (optional-eq :add-to-set))
(define add-to-set*      (optional-eq :add-to-set*))
(define remove-from-set  (optional-eq :remove-from-set))
(define remove-from-set* (optional-eq :remove-from-set*))
(define subset?          (optional-eq :subset?))
(define set=?            (optional-eq :set=?))
