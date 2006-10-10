;;; search-graph.scm -- breadth-first and depth-first search on graphs
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-10-10 09:50:54 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "stdlib.scm")
(load "sets.scm")

;; This is a base class for problems solved by breadth-first or
;; depth-first search.  You must override the methods start, goal?,
;; state-eq? and successors-of, to tailor them to your problem.  Then
;; the search procedure will do its thing.

(define searchable%
  (class object%
    (public start goal? state-eq? successors-of search return)
    ;; By default, do breadth-first search.  To switch to depth-first, 
    ;; substitute a stack% here.
    (init-field (qm (new queue%))
                (target 51))
    ;; The main search procedure, given the start state.
    (define (search* from)
      ;; q is the queue (or stack) of arcs to visit,
      ;; v is the set of states already accounted for.
      (do ((q (send qm push 
                    (cons (list (cons 'start from)) from) 
                    (send qm initial)))
           (v (list from)))
        ;; Stop the loop whenever the queue becomes empty (which means
        ;; failure) or when we reach a goal state.
        ((or (send qm empty? q)
             (send this goal? (cdr (send qm peek q))))
         (if (send qm empty? q) #f
             (send this return (reverse (car (send qm peek q))))))
        ;; Each time through the loop, do the following
        (let* ((cur (send qm peek q)))
          (set! q (send qm pop q))
          (for-each
           (lambda (arc)
             (when (not (element-of? (cdr arc) v state-eq-fn))
               ;; Mark as visited
               (set! v (add-to-set (cdr arc) v state-eq-fn))
               ;; Add to the queue
               (set! q (send qm push 
                             (cons (cons arc (car cur)) (cdr arc))
                             q))))
           (send this successors-of (cdr cur))))))
    ;; Top-level search method
    (define (search)
      (search* (send this start)))
    ;; By default we display the entire path.
    (define (return plan)
      (let ((i 0))
        (for-each
         (lambda (arc)
           (printf "#~v: ~v -> ~v~n" i (car arc) (cdr arc))
           (set! i (+ i 1)))
         plan)))
    ;; The rest of these are meant to be overridden in subclasses.
    ;; They define a simple (infinite) graph with integers as states
    ;; and edges that can increment, double, or square.  We then
    ;; search for a path to the goal number (51 by default).  Since
    ;; the graph is infinite, avoid depth-first search!
    (define (start) 0)
    (define (goal? n) (= n target))
    (define (state-eq? n k) (= n k))
    (define (state-eq-fn n k) (send this state-eq? n k))
    (define (successors-of n)
      (list (cons 'plus-1 (+ n 1))
            (cons 'double (* n 2))
            (cons 'square (* n n))))
    (super-new)))
