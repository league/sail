;;; gene-knapsack.scm -- a genetic algorithm to solve "knapsack" problems
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-09-26 12:36:15 league>

(load "gene-vec.scm")

;; from Wikipedia: The knapsack problem is a problem in combinatorial
;; optimization. It derives its name from the maximization problem of 
;; choosing possible essentials that can fit into one bag (of maximum 
;; weight) to be carried on a trip. A similar problem very often appears 
;; in business, combinatorics, complexity theory, cryptography and applied
;; mathematics. Given a set of items, each with a cost and a value, then 
;; determine the number of each item to include in a collection so that 
;; the total cost is less than some given cost and the total value is as 
;; large as possible.
;; http://en.wikipedia.org/w/index.php?title=Knapsack_problem&oldid=72421320

;; We're just doing the BINARY (0-1) knapsack problem: each item either goes 
;; in the bag, or it doesn't.

(define knapsack-algo%
  (class genetic-vector-algo%
    (override create evaluate)
    (public weight-of value-of)
    (init-field (items default-items)
                (limit default-limit))
    (define (create)
      (vector-tabulate 
       (vector-length items)
       (lambda (i) (random 2))))
    (define (accum fn v)
     (vector-foldr 
       (lambda (i x w)
         (+ w (* x (fn (vector-ref items i)))))
       0 v))
    (define (weight-of v) (accum car v))
    (define (value-of v) (accum cdr v))
    (define (evaluate v)
      (let ((val (value-of v))
            (wt (weight-of v)))
        (if (> wt limit) 0 val)))
    (super-new)))

(define default-items  ; each is a pair of (weight . value)
  #((5 . 10)    (8 . 20)    (4 .  5)    (7 . 30)
    (5 . 10)    (8 . 13)    (4 . 10)    (5 . 10)
    (4 .  5)    (7 . 30)    (5 . 10)    (8 . 13)
    (4 . 10)    (5 . 10)    (1 .  8)    (9 .  9)
    (1 .  8)    (4 .  5)    (7 . 30)    (5 . 10)
    (8 . 13)    (4 . 10)    (5 . 10)    (1 .  8)
    (9 .  9)    (1 .  8)    (9 .  9)    (9 .  9)
    (5 . 19)    (10 . 5)    (50 . 100)  (33 . 98)))    

(define default-limit 60)

