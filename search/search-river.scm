;;; search-river.scm -- the fox/goose/beans river-crossing problem
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-10-10 09:42:56 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "search-graph.scm")

;; Problem description from Wikipedia
;; http://en.wikipedia.org/wiki/Fox%2C_goose_and_bag_of_beans_puzzle
;; 
;; Once upon a time a farmer went to market and purchased a fox, a
;; goose, and a bag of beans. On his way home, the farmer came to
;; the bank of a river and hired a boat. But in crossing the river
;; by boat, the farmer could carry only himself and a single one of
;; his purchases - the fox, the goose, or the bag of beans.
;; 
;; If left alone, the fox would eat the goose, and the goose would
;; eat the beans.
;; 
;; The farmer's challenge was to carry himself and his purchases to
;; the far bank of the river, leaving each purchase intact. How did
;; he do it?

(define river-crossing%
  (class searchable%
    (override start goal? state-eq? successors-of)
    (public bad-set? bad-state?)
    ;; Represent a state as two sets, the things on the north bank
    ;; and those on the south back.  Initially, everything is north.
    (define objects '(person fox goose beans))
    (define (start) (list objects null))
    ;; Two states are equivalent if the set of objects on each bank
    ;; is the same.  We must use set equality.
    (define (state-eq? s t)
      (and (set=? (car s) (car t))
           (set=? (cadr s) (cadr t))))
    ;; We reach the goal when the north bank is empty, and the south
    ;; contains all the objects.
    (define (goal? st)
      (and (null? (car st))
           (set=? (cadr st) objects)))
    ;; A set of items is bad if the goose is left alone either with
    ;; the fox or with the beans.
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
        ;; filter out arcs that would be bad
        (filter 
         (lambda (arc) (not (bad-state? (cdr arc))))
         ;; go through objects on farmer's side, to decide what to take
         (foldr 
          (lambda (item alts)
            (cons (cons item (join (remove-from-set item objs)
                                   (add-to-set item other)))
                  alts))
          (list (cons 'cross (join objs other)))
          objs))))
    (super-new)))

(define rc (new river-crossing%))
(send rc search)
