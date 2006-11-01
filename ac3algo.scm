;;; ac3algo.scm -- arc consistency algorithm for constraint propagation
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-11-01 11:52:09 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "stdlib.scm")
(load "sets.scm")

(define ac3algo%
  (class model%
    (public all-pairs arc-reduce constraint-between constraint-on
            domain-of enum-choices initial-domains next-pair
            partners-of propagate propagate-step reset
            reset-domains satisfiable? search search-step
            set-domain-of solved? summary work-left?)
    (init-field
     (variables '(x y))
     (values '(0 1 2 3 4 5)))
    ;; set up initial domains for all variables
    (define (initial-domains)
      (map 
       (lambda (x) (cons x (filter (send this constraint-on x) values)))
       variables))
    (define dom (initial-domains))
    (define (domain-of x)
      (cdr (assoc x dom)))
    (define (set-domain-of x d)
      (set! dom (assoc-update x dom d))
      (set! work (pairs-with x work))
      (send this changed))
    (define (reset-domains)
      (set! dom (initial-domains))
      (set! work (all-pairs))
      (send this changed))
    (define (summary)
      (for-each
       (lambda (d) (printf "~v: ~v~n" (car d) (cdr d)))
       dom))
    ;; problem is satisfiable as long as no domains are empty
    (define (satisfiable?)
      (foldr (lambda (d sat) (and sat (not (null? (cdr d)))))
             #t dom))
    ;; problem is solved if all domains are singletons
    (define (solved?)
      (foldr (lambda (d sat) (and sat (= 1 (length (cdr d)))))
             #t dom))
    ;; set up work list as all distinct pairs
    (define (partners-of x1)
      (filter (lambda (x2) (not (equal? x1 x2))) variables))
    (define (pairs-with x2 L)
      (foldr (lambda (x1 L1) (add-to-set (cons x1 x2) L1))
             L (partners-of x2)))
    (define (all-pairs)
      (foldr pairs-with null variables))
    (define work (all-pairs))
    (define (work-left?)
      (not (null? work)))
    (define (next-pair) (car work))
    ;; constraints to be overridden
    (define (constraint-on var)
      (lambda (val)
        (if (equal? var 'x)
            (even? val)
            #t)))
    (define (constraint-between x1 x2)
      (lambda (v1 v2)
        (= 4 (+ v1 v2))))
    ;; the basic primitive of this algorithm: adjust the domains 
    ;; according to the constraints on one arc.
    (define (arc-reduce dom x1 x2)
      (let ((change #f)
            (d1 (cdr (assoc x1 dom)))
            (d2 (cdr (assoc x2 dom)))
            (constr (send this constraint-between x1 x2)))
        (for-each
         (lambda (v1) 
           (unless (findf (lambda (v2) (constr v1 v2)) d2)
;             (printf "arc-reduce(~v,~v) removes ~v~n" x1 x2 v1)
             (set! d1 (remove v1 d1))
             (set! change #t)))
         d1)
        (cons change d1)))
    ;; run through propagation along one edge.
    (define (propagate-step)
      (when (work-left?)
        (let* ((x (caar work))
               (y (cdar work))
               (r (send this arc-reduce dom x y)))
          (cond
            ((equal? r '(#t))
             (set! dom (assoc-update x dom null))
             (set! work null))
            ((car r)
             (set! dom (assoc-update x dom (cdr r)))
             (set! work (pairs-with x (cdr work))))
            (else
             (set! work (cdr work)))))
        (send this changed)))
    ;; one complete round of propagation, until work list becomes empty.
    (define (propagate)
      (when (work-left?)
        (yield)
        (propagate-step)
        (propagate)))
    (define (enum-choices ds)
      (cond
        ((null? ds) null)         ; reached the end
        ((null? (cdar ds)) null)  ; unsatisfiable
        ((null? (cddar ds))       ; singleton
         (enum-choices (cdr ds)))
        (else
         (enum-choices-helper (caar ds) (cdar ds)))))
    (define (enum-choices-helper x vs)
      (if (null? vs) null
          (cons (cons x (car vs))
                (delay (enum-choices-helper x (cdr vs))))))
;    (define (enum-choices ds vs)
;      (cond
;        ((or (null? vs) (null? (cdr vs)))
;         (cond
;           ((null? ds) null)
;           ((null? (cdar ds))   ; unsatisfiable
;            (enum-choices (cdr ds) null))
;           ((null? (cddar ds))  ; singleton
;            (enum-choices (cdr ds) null))
;           (else
;            (enum-choices (cdr ds) (car ds)))))
;        (else
;         (cons (cons (car vs) (cadr vs))
;               (delay (enum-choices ds (cons (car vs) (cddr vs))))))))
    (define decisions
      (list (cons (delay (enum-choices dom)) dom)))
    (define (reset)
      (set! dom (initial-domains))
      (set! decisions (list (cons (delay (enum-choices dom)) dom)))
      (set! work (all-pairs))
      (send this changed))
    (define (search-step)
      (when (not (work-left?)) ; search only once propagation is done
        (cond
          ((and (satisfiable?) (not (solved?)))
           (let ((cs (enum-choices dom)))
             (set! decisions (cons (cons (cdr cs) dom) decisions))
             (set-domain-of (caar cs) (list (cdar cs)))
;             (printf "trying ~v = ~v~n" (caar cs) (cdar cs))
             ))
          ((null? decisions)
           (printf "search exhausted~n"))
          (else
           (let ((cs (force (caar decisions))))
             (cond
               ((null? cs)
                (set! decisions (cdr decisions))
                (search-step))
               (else
                (set! decisions (cons (cons (cdr cs) (cdar decisions))
                                      (cdr decisions)))
                (set! dom (cdar decisions))
                (set-domain-of (caar cs) (list (cdar cs)))
;                (printf "backtracking to ~v = ~v~n" (caar cs) (cdar cs))
                )))))))
    (define (search)
      (yield)
      (cond
        ((work-left?) (propagate) (search))
        ((null? decisions) #f)
        ((solved?) (summary))
        (else (search-step) (search))))
    (super-new)))

(define ac3controls%
  (class vertical-pane%
    (public update)
    (init-field (m (new ac3algo%)))
    (define (update . args)
      (let ((sol (send m solved?))
            (sat (send m satisfiable?))
            (wk (send m work-left?)))
        (send st set-label 
              (if wk (to-string (send m next-pair))
                  (if sol "solved" 
                      (if sat "satisfiable" "failed"))))
        (send p1 enable wk)
        (send pa enable wk)
        (send s1 enable (not wk))
        (send se enable (not sol))))
    (super-new)
    (define rs (new button% (label "Reset") (parent this)
                    (callback (lambda (b e) (send m reset)))))
    (define p1 (new button% (label "Propagate 1") (parent this)
                    (callback (lambda (b e) (send m propagate-step)))))
    (define pa (new button% (label "Propagate all") (parent this)
                    (callback (lambda (b e) (send m propagate)))))
    (define s1 (new button% (label "Search step") (parent this)
                    (callback (lambda (b e) (send m search-step)))))
    (define se (new button% (label "Search") (parent this)
                    (callback (lambda (b e) (send m search)))))
    (define st (new message% (label "__________________") (parent this)))
    (send m attach this)
    (update)))

;(define fr (new frame% (label "OK")))
;(define ac (new ac3controls% (parent fr)))
;(send fr show #t)
