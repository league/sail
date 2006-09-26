;;; gene-vec.scm -- genetic algorithms applied to vectors of values
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-09-26 12:37:49 league>

(load "gene-algo.scm")

;; A traditional genetic algorithm works on fixed-length bit strings,
;; or vectors of other simple values.  This makes crossover and
;; mutation extremely simple, and they are defined here.

;; Because create and evaluate are also overridden, this forms a
;; complete GA, although the problem it solves is not very
;; interesting.  Individuals are vectors of 5 random numbers, each in
;; the range 0 to 9.  The fitness measure is just the product of the 5
;; numbers, so the highest possible fitness is 9^5 = 59049, with the
;; individual #(9 9 9 9 9), abbreviated 5#(9) by DrScheme.  It should
;; take only 3 or so generations to get there.

(define genetic-vector-algo%
  (class genetic-algo%
    (override create mutate crossover evaluate)
    (define (mutate v)
      (let* ((n (vector-length v))
             (i (random n))
             (x (send this create)))
        (vector-tabulate 
         n (lambda (j) (vector-ref (if (= i j) x v) j)))))
    (define (crossover v1 v2)
      (let* ((n (vector-length v1))
             (i (random n)))
        (vector-tabulate
         n (lambda (j) (vector-ref (if (< j i) v1 v2) j)))))
    (define (create)
      (vector-tabulate 5 (lambda (x) (random 10))))
    (define (evaluate v)
      (let ((prod 1))
        (vector-for-each v (lambda (i x) (set! prod (* prod x))))
        prod))
    (super-new)))

