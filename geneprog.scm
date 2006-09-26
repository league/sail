;;; geneprog.scm -- framework for genetic programming
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-09-12 20:08:04 league>

(load "stdlib.scm")

;; Some default parameters for the genetic algorithm.
(define POPULATION_SIZE      1000)
(define NUM_TO_KEEP          (* 10/100 POPULATION_SIZE))
(define NUM_TO_MUTATE        (*  1/100 POPULATION_SIZE))
(define NUM_CHILDREN         (* 89/100 POPULATION_SIZE))  
(define INITIAL_HEIGHT_LIMIT 8)
(define TOURNAMENT_SIZE      7)

;; Compute the size of (number of nodes in) a generated Scheme program.
(define (program-size tr) (program-size-acc tr 0))
(define (program-size-acc tr n)
  (if (pair? tr)
      (foldr program-size-acc n tr)
      (+ n 1)))

;; To specify an instance of a genetic program, we must provide a
;; list of leaf atoms, a list of operators (paired with their arity),
;; and a fitness function.
(define-struct genetic-program (leaves operators fitness))

;; Generate a fully random program from the components in 'leaves' and
;; 'ops', but not to exceed INITIAL_HEIGHT_LIMIT.
(define (random-program gp)
  (letrec ((leaves (genetic-program-leaves gp))
           (ops (genetic-program-operators gp))
           (j (length leaves))
           (k (length ops))
           (n (+ j k))
           (branch (lambda (h r)
                     (let ((op (list-ref ops r)))
                       (cons (car op)
                             (repeat (cdr op) (partial-apply make h))))))
           (make (lambda (h)
                   (if (< h 1) (list-ref leaves (random j))
                       (choose h (random n)))))
           (choose (lambda (h r)
                     (if (< r j)
                         (list-ref leaves r)
                         (branch (- h 1) (- r j))))))
    (make INITIAL_HEIGHT_LIMIT)))

;; Extract the nth sub-tree from the given tree.  This implementation
;; is pretty sophisticated, using a pair of "continuations" to thread
;; the result around a standard recursive tree traversal.  Unless you
;; are familiar with these techniques already, you're not expected to
;; understand this!
(define (nth-subtree tree n return continue)
  (cond
    ((= n 0) (return tree))
    ((pair? tree) 
     (nth-subtree-list (cdr tree) (- n 1) return continue))
    (else (continue (- n 1)))))

(define (nth-subtree-list trees n return continue)
  (if (null? trees) (continue n)
      (nth-subtree (car trees) n return
                   (lambda (k) 
                     (nth-subtree-list (cdr trees) k return continue)))))

;; Using a similar technique, we replace the nth sub-tree with a new
;; tree.
(define (replace-nth-subtree tree n new return)
  (cond
    ((= n 0) (return -1 new))
    ((pair? tree)
     (replace-nth-subtree-list 
      (cdr tree) (- n 1) new
      (lambda (k ts) (return k (cons (car tree) ts)))))
    (else
     (return (- n 1) tree))))

(define (replace-nth-subtree-list trees n new return)
  (if (null? trees)
      (return n null)
      (replace-nth-subtree 
       (car trees) n new
       (lambda (k tr)
         (replace-nth-subtree-list 
          (cdr trees) k new
          (lambda (j ts) (return j (cons tr ts))))))))

;; Now we may define crossover (aka recombination, aka sexual 
;; reproduction) between two programs.  A random sub-tree of 
;; the left tree is replaced with a random sub-tree from the 
;; right one.
(define (random-crossover tree1 tree2)
  (let* ((r1 (random (program-size tree1)))
         (r2 (random (program-size tree2))))
    (replace-nth-subtree tree1 r1 (nth-subtree tree2 r2 I E) snd)))

(define (I x) x)
(define (E x) (error (number->string x)))
(define (snd x y) y)

;; Next, define mutations, which means replacing a random subtree
;; with a newly grown one.
(define (random-mutation gp tree)
  (replace-nth-subtree 
   tree (random (program-size tree))
   (random-program gp) snd))

;; Return a pair of the best individual and its score.
(define (best-of pop)
  (foldr (lambda (i best) 
           (if (> (cdr i) (cdr best)) i best))
         (cons #f -1)
         pop))

;; Return a sum of all scores in the population (used for average).
(define (sum-of-scores pop)
  (foldr (lambda (i sum) (+ sum (cdr i))) 0 pop))

;; Print a one-line summary of the size & fitness of a population.
(define (summarize-population pop)
  (let ((n (length pop)))
    (printf
     "~v individuals, average fitness ~v, best fitness ~v~n" n
     (exact->inexact (/ (sum-of-scores pop) n))
     (cdr (best-of pop)))))

;; Return a brand new population of the specified size.
(define (new-population gp)
  (let* ((lv (genetic-program-leaves gp))
         (op (genetic-program-operators gp))
         (fn (genetic-program-fitness gp))
         (each (lambda ()
                 (let ((p (random-program gp)))
                   (cons p (fn p))))))
    (repeat POPULATION_SIZE each)))

;; Select k random individuals and return the best of them.
(define (tournament-selection pop)
  (let* ((n (length pop))
         (pick (lambda () (list-ref pop (random n))))
         (candidates (repeat TOURNAMENT_SIZE pick)))
    (best-of candidates)))

;; Apply tournament selection to the population to produce k individuals
;; for the next generation.
(define (weed-out-unfit pop)
  (repeat NUM_TO_KEEP (lambda () (tournament-selection pop))))

;; Apply crossover (recombination) to generate k new individuals for the
;; next generation.
(define (apply-crossover gp pop)
  (repeat NUM_CHILDREN 
          (lambda () 
            (let* ((mom (tournament-selection pop))
                   (dad (tournament-selection pop))
                   (child (random-crossover (car mom) (car dad))))
              (cons child ((genetic-program-fitness gp) child))))))
                
(define (apply-mutation gp pop)
  (repeat NUM_TO_MUTATE
          (lambda ()
            (let* ((parent (tournament-selection pop))
                   (child (random-mutation gp (car parent))))
              (cons child ((genetic-program-fitness gp) child))))))

(define (evolve-population gp pop)
  (let ((p1 (weed-out-unfit pop))
        (p2 (apply-crossover gp pop))
        (p3 (apply-mutation gp pop)))
    (append p1 p2 p3)))


;; Here is a convenient top-level interface using global variables 
;; to track the population.

(define CURRENT_POP null)
(define CURRENT_GEN 0)

(define (genesis gp)
  (printf "Creating ~v new individuals... ~n" POPULATION_SIZE)
  (set! CURRENT_POP (new-population gp))
  (printf "Generation ~v: " CURRENT_GEN)
  (summarize-population CURRENT_POP))

(define (next-gen gp)
  (printf "Applying crossover and mutation... ~n")
  (set! CURRENT_POP (evolve-population gp CURRENT_POP))
  (set! CURRENT_GEN (+ 1 CURRENT_GEN))
  (printf "Generation ~v: " CURRENT_GEN)
  (summarize-population CURRENT_POP))

(define (run-gens gp k)
  (when (> k 0)
    (next-gen gp)
    (run-gens gp (- k 1))))
