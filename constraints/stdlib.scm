;;; stdlib.scm -- some general-purpose helper functions
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-11-01 11:51:09 league>

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

;; Encapsulate a typical "for i = j to n" loop control structure.
(define (for-loop from to body)
  (when (< from to)
    (body from)
    (for-loop (+ from 1) to body)))

;; Collect the results of (each), called n times.
(define (repeat n each)
  (if (= n 0) null
      (cons (each) (repeat (- n 1) each))))

;; Build a vector of 'n' elements, initializing each element according
;; to the results of the characteristic function 'f'.
(define (vector-tabulate n each)
  (let ((v (make-vector n)))
    (for-loop 0 n (lambda (i) (vector-set! v i (each i))))
    v))

;; This is just a for-each loop applied to vectors instead of lists.
(define (vector-for-each v f)
  (for-loop 0 (vector-length v)
            (lambda (i) (f i (vector-ref v i)))))

(define (vector-foldr f z v)
  (let ((acc z))
    (for-loop 0 (vector-length v)
              (lambda (i) (set! acc (f i (vector-ref v i) acc))))
    acc))

;; Build a vector of vectors ('r' rows by 'c' columns), initializing each
;; element according to the results of the characteristic function 'f'.
(define (vec2d-tabulate r c f)
  (vector-tabulate r (lambda (i)
                       (vector-tabulate c (lambda (j) (f i j))))))

;; Apply 'f' to each of the elements of a vector of vectors, passing
;; the indices i and j along with the element.  'f' is executed for
;; effect only, any returned value is ignored.
(define (vec2d-for-each matrix f)
  (vector-for-each matrix 
                   (lambda (i row)
                     (vector-for-each row (lambda (j elt)
                                            (f i j elt))))))

;; Encapsulate a double for-loop control structure, where 'outer'
;; occurs each time around the outer loop, and its result is passed 
;; to 'inner' along with the indices i and j.
(define (double-loop* n k outer inner)
  (for-loop 0 n (lambda (i)
                  (let ((r (outer i)))
                    (for-loop 0 k (lambda (j) (inner r i j)))))))

;; Convert any object to a string representation, by way of a string port.
(define (to-string obj)
  (let ((p (open-output-string)))
    (display obj p)
    (get-output-string p)))

(define (log2 n) (/ (log n) (log 2)))
(define (ith i) (lambda (x) (list-ref x i)))
(define (square x) (* x x))

(define (apply-to-cdr f)
  (lambda (pair)
    (cons (car pair) (f (cdr pair)))))
(define (divide-by n)
  (lambda (x) (/ x n)))
(define (ith-eq? i v)
  (lambda (entity)
    (eq? (list-ref entity i) v)))

(define (partial-apply proc . a1)
  (lambda a2 (apply proc (append a1 a2))))

(define (assoc-update key alist val)
  (cond
    ((null? alist) (list (cons key val)))
    ((equal? key (caar alist)) (cons (cons key val) (cdr alist)))
    (else (cons (car alist) (assoc-update key (cdr alist) val)))))

;; A dirt-simple representation of a stack using a list.
(define stack%
  (class object%
    (public initial empty? push peek pop find)
    (define (initial) null)
    (define (empty? s) (null? s))
    (define (push x s) (cons x s))
    (define (peek s) (car s))
    (define (pop s) (cdr s))
    (define (find f s)
      (findf f s))
    (super-new)))

;; A functional queue implementation (as described by Chris Okasaki).
(define queue%
  (class object%
    (public initial empty? push peek pop find)
    (define (initial) (cons null null))
    (define (empty? q) (null? (car q)))
    (define (push x q)
      (if (null? (car q))
          (cons (cons x (car q)) (cdr q))
          (cons (car q) (cons x (cdr q)))))
    (define (peek q) (caar q))
    (define (pop q)
      (if (null? (cdar q))
          (cons (reverse (cdr q)) null)
          (cons (cdar q) (cdr q))))
    (define (find f q)
      (or (findf f (car q))
          (findf f (cdr q))))
    (super-new)))
