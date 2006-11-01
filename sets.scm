;;; sets.scm -- sets represented as lists, with user-specified equality
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-11-01 11:51:23 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(define empty-set null)

;; The functions starting with colons (:) are meant to be private.
;; They take explicit equality predicates.  The public versions are
;; defined at the bottom, where the equality is made optional.

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

(define (:union s t eq)
  (foldr (lambda (x u) (:add-to-set x u eq)) s t))

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
(define union            (optional-eq :union))
