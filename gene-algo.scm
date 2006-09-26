;;; gene-algo.scm -- framework for genetic algorithms
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-09-26 12:33:11 league>

(load "stdlib.scm")

;; This class is useless by itself; you must create a subclass that
;; overrides create, evaluate, crossover, and mutate.  It does,
;; however encapsulate the basic framework for genetic algorithms.
;; Send the message 'start' to create the initial population, and
;; 'next' to evolve the next one, or 'loop' to work through several
;; generations at once.
;; 
;; Many parameters can be tweaked, including the settings of
;; how-many-survive (and similar) and the tournament-size.  Also, the
;; method 'select' can be overridden to replace tournament selection
;; with some other strategy.

(define genetic-algo%
  (class model%
    (public size average best start next loop select
            create evaluate crossover mutate)
    ;; Some of the tweakable paramaeters
    (init-field (how-many-survive  100)
                (how-many-children 890)
                (how-many-mutate    10)
                (tournament-size     7)
                (generation-num      0))
    (define pop null)       ; tracks current population
    (define len 0)          ; should always be (length pop)
    (define (how-many-sum)  ; just add up the how-many parameters
      (+ how-many-survive
         how-many-children
         how-many-mutate))
    (define (size) len)     ; return size of current population
    (define (average)       ; compute average fitness of current pop
      (/ (foldr (lambda (i sum) (+ sum (cdr i))) 0 pop)
         (length pop)))
    (define (best-of group)
      (foldr (lambda (i b)
               (if (> (cdr i) (cdr b)) i b))
             (cons #f -inf.0)
             group))
    (define (best)          ; return a pair of the most-fit individual
      (best-of pop))        ; and its score
    (define (make)
      (let ((i (send this create)))
        (cons i (send this evaluate i))))
    (define (start)
      (send this changed 'start)
      (set! len (how-many-sum))
      (set! pop (repeat len make))
      (set! generation-num 0)
      (send this changed 'finish))
    (define (apply-crossover)
      (let* ((mom (select))
             (dad (select))
             (child (send this crossover (car mom) (car dad))))
        (cons child (send this evaluate child))))
    (define (apply-mutation)
      (let* ((parent (select))
             (child (send this mutate (car parent))))
        (cons child (send this evaluate child))))
    (define (next)
      (send this changed 'next 'survivors)
      (let ((p1 (repeat how-many-survive (lambda () (select)))))
        (send this changed 'next 'children)
        (let ((p2 (repeat how-many-children apply-crossover)))
          (send this changed 'next 'mutations)
          (let ((p3 (repeat how-many-mutate apply-mutation)))
            (set! pop (append p1 p2 p3))
            (set! len (how-many-sum))
            (set! generation-num (+ 1 generation-num))
            (send this changed 'finish)))))
    ;; Repeat the (next) operation k times.
    (define (loop k)
      (when (> k 0)
        (send this next)
        (loop (- k 1))))
    (define (pick) (list-ref pop (random len)))
    ;; This is tournament selection; best of 7 (tournament-size)
    ;; randomly chosen individuals.
    (define (select)
      (best-of (repeat tournament-size pick)))
    ;; The following must be overridden for this algorithm to do
    ;; anything interesting.
    (define (create) 0)
    (define (evaluate i) 0)
    (define (crossover i j) i)
    (define (mutate i) i)
    (super-new)))

;; Define a few set- functions for tweaking parameters of the GA.
(define set-how-many-survive 
  (class-field-mutator genetic-algo% how-many-survive))
(define set-how-many-children
  (class-field-mutator genetic-algo% how-many-children))
(define set-how-many-mutate
  (class-field-mutator genetic-algo% how-many-mutate))

;; This view class handles printing brief status reports as the GA
;; does its job.
(define ga-text-view%
  (class object%
    (public update)
    (init-field (ga #f))
    (define (summarize)
      (printf "generation ~a is ~a individuals; avg fitness = ~a, best = ~a~n"
              (get-field generation-num ga)
              (send ga size)
              (exact->inexact (send ga average))
              (cdr (send ga best))))
    (define (update . args)
      (cond
        ((pair? args)
         (case (car args)
           ('start (printf "initializing population... ~n"))
           ('finish (summarize))
           ('next 
             (case (cadr args)
               ('survivors 
                 (printf "selecting ~a survivors... ~n"
                         (get-field how-many-survive ga)))
               ('children
                 (printf "producing ~a children... ~n"
                         (get-field how-many-children ga)))
               ('mutations
                 (printf "applying ~a mutations... ~n"
                         (get-field how-many-mutate ga)))))))))
    (super-new)
    (send ga attach this)))

;; Attaches the above view to a new GA class, and starts it.
(define (setup ga)
  (new ga-text-view% (ga ga))
  (send ga start)
  ga)
