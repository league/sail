(load "stdlib.scm")
(load "sets.scm")

(define searchable%
  (class object%
    (public start goal? state-eq? successors-of search)
    ;; by default, do breadth-first search.  To switch to depth-first, 
    ;; substitute a stack% here.
    (init-field (qm (new queue%))
                (goal 51))
    ;; the main search procedure, given the start state
    (define (search* from)
      ;; q is the queue (or stack) of states to visit,
      ;; v is the set of states already accounted for.
      (do ((q (send qm push (cons null from) (send qm initial)))
           (v (list from)))
        ;; stop the loop whenever the queue becomes empty (which means
        ;; failure) or when we reach a goal state.
        ((or (send qm empty? q)
             (send this goal? (cdr (send qm peek q))))
         (if (send qm empty? q) #f
             (reverse (car (send qm peek q)))))
        ;; each time through the loop, do the following
        (let* ((cur (send qm peek q)))
          (set! q (send qm pop q))
          (for-each
           (lambda (arc)
             (when (not (element-of? (cdr arc) v state-eq-fn))
               (set! v (add-to-set (cdr arc) v state-eq-fn))
               (set! q (send qm push 
                             (cons (cons (car arc) (car cur)) (cdr arc))
                             q))))
           (send this successors-of (cdr cur))))))
    ;; top-level search method
    (define (search)
      (search* (send this start)))
    ;; The rest of these are meant to be overridden in subclasses.
    ;; They define a simple (infinite) graph with integers as states
    ;; and edges that can increment, double, or square.  Since the
    ;; graph is infinite, avoid depth-first search!
    (define (start) 0)
    (define (goal? n) (= n goal))
    (define (state-eq? n k) (= n k))
    (define (state-eq-fn n k) (send this state-eq? n k))
    (define (successors-of n)
      (list (cons 'plus-1 (+ n 1))
            (cons 'double (* n 2))
            (cons 'square (* n n))))
    (super-new)))

(define river-crossing%
  (class searchable%
    (override start goal? state-eq? successors-of)
    (define (start)
      (list '(person fox goose beans) null))
    (define (state-eq? s t)
      (and (set=? (car s) (car t))
           (set=? (cadr s) (cadr t))))
    (define (goal? st)
      (and (null? (car st))
           (set=? (cadr st) '(person fox goose beans))))
    (define (bad-set? s)
      (and (not (element-of? 'person s))
           (element-of? 'goose s)
           (or (element-of? 'fox s)
               (element-of? 'beans s))))
    (define (bad-state? st)
      (or (bad-set? (car st))
          (bad-set? (cadr st))))
    (define (successors-of st)
      (let* (;; which bank is the farmer on?
             (north? (element-of? 'person (car st)))
             ;; objects on farmer's bank
             (objs (remove-from-set 'person (if north? (car st) (cadr st))))
             ;; objects on opposite bank
             (other (cons 'person (if north? (cadr st) (car st))))
             ;; put together my bank w/opposite
             (join (lambda (m o) (if north? (list m o) (list o m)))))
        (filter 
         (lambda (s) (not (bad-state? (cdr s))))
         (foldr (lambda (item alts)
                  (cons (cons item (join (remove-from-set item objs)
                                         (add-to-set item other)))
                        alts))
                (list (cons 'cross (join objs other)))
                objs))))
    (super-new)))

;(define blocks%
;  (class searchable%
;    (