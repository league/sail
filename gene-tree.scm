;;; gene-tree.scm -- genetic algorithms applied to tree structures
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-09-26 15:31:24 league>

(load "gene-algo.scm")

;; Here, we apply the GA framework to tree structures, which of course
;; can be Scheme programs.  Thus, we are doing Genetic Programming.
;; Crossover and mutation are somewhat trickier to define, but that is
;; the role of the helpers nth-subtree and replace-nth-subtree.

;; The create and evaluate methods are also overridden, so this forms
;; a complete GA, although the problem it solves is not very
;; interesting.  Individuals are trees representing arithmetic
;; expressions consisting of operators +,-,* and constant integers in
;; the range 0-9.  The evaluator measures fitness as the difference
;; between 1009 (a prime number) and the result of evaluating the
;; arithmetic expression.  You should witness the fitness rise UP to
;; -1 after just a few generations; it has generated an expression
;; that produces 1008.  It will take several more tries before it is
;; able to use addition to reach the prime number 1009.

(define genetic-tree-algo%
  (class genetic-algo%
    (override create mutate crossover evaluate)
    (public create-leaf create-branch create-tree)
    (init-field (initial-height-limit 4)
                (leaf-probability .5))
    (define (create-leaf)
      (random 10))
    (define (create-branch h)
      (case (random 3)
        ('0 (list '+ (create-tree h) (create-tree h)))
        ('1 (list '- (create-tree h) (create-tree h)))
        ('2 (list '* (create-tree h) (create-tree h)))))
    (define (create-tree ht)
      (let ((h (- ht 1)))
        (if (or (= h 0)
                (<= (random) leaf-probability))
            (create-leaf)
            (create-branch h))))
    (define (create)
      (create-tree initial-height-limit))
    ;; To mutate, replace a random subtree with a newly created one.
    (define (mutate t)
      (replace-nth-subtree
       t (random (tree-size t))
       (create) snd))
    ;; A random sub-tree of the left tree is replaced with a random
    ;; sub-tree from the right one.
    (define (crossover t1 t2)
      (let ((r1 (random (tree-size t1)))
            (r2 (random (tree-size t2))))
        (replace-nth-subtree t1 r1 (nth-subtree t2 r2 I E) snd)))
    (define (evaluate t)  ;; target is a prime #
      (- (abs (- 1009 (eval t)))))
    (super-new)))

;; Compute the size of (number of nodes in) a generated Scheme program.
(define (tree-size tr) (tree-size-acc tr 0))
(define (tree-size-acc tr n)
  (if (pair? tr)
      (foldr tree-size-acc n tr)
      (+ n 1)))

(define (I x) x)
(define (E x) (error (number->string x)))
(define (snd x y) y)

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

