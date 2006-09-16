;;; stdlib.scm -- some general-purpose helper functions
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-09-12 20:08:21 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(require (lib "list.ss"))

;; A class representing a Model, as in the Model-View-Controller (MVC)
;; architecture.  It keeps a list of dependents, and notifies them with
;; an 'update' message whenever the model is 'changed'.
(define model%
  (class object%
    (public attach detach changed)
    (define dependents null)
    (define (attach d)
      (set! dependents (cons d dependents)))
    (define (detach d)
      (set! dependents (remq d dependents)))
    (define (changed . args)
      (for-each (lambda (d) (send d update . args))
                dependents))
    (super-new)))

(define (for-loop from to body)
  (when (< from to)
    (body from)
    (for-loop (+ from 1) to body)))

(define (vector-tabulate n each)
  (let ((v (make-vector n)))
    (for-loop 0 n (lambda (i) (vector-set! v i (each i))))
    v))

(define (vector-for-each v f)
  (for-loop 0 (vector-length v)
            (lambda (i) (f i (vector-ref v i)))))

(define (vec2d-tabulate r c f)
  (vector-tabulate r (lambda (i)
                       (vector-tabulate c (lambda (j) (f i j))))))

(define (vec2d-for-each matrix f)
  (vector-for-each matrix 
                   (lambda (i row)
                     (vector-for-each row (lambda (j elt)
                                            (f i j elt))))))
(define (double-loop n k outer)
  (for-loop 0 n (lambda (i) 
                  (outer i (lambda (inner) 
                             (for-loop 0 k inner))))))

(define (double-loop* n k outer inner)
  (for-loop 0 n (lambda (i)
                  (let ((r (outer i)))
                    (for-loop 0 k (lambda (j) (inner r i j)))))))

(define (to-string obj)
  (let ((p (open-output-string)))
    (display obj p)
    (get-output-string p)))

(define (log2 n) (/ (log n) (log 2)))
(define (ith i) (lambda (x) (list-ref x i)))

(define (apply-to-cdr f)
  (lambda (pair)
    (cons (car pair) (f (cdr pair)))))
(define (divide-by n)
  (lambda (x) (/ x n)))
