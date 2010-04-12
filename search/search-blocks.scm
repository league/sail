;;; search-blocks.scm -- the block-stacking problem
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-10-10 09:52:35 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "search-graph.scm")

(define blocks%
  (class searchable%
    (override start goal? state-eq? successors-of)
    (public move-to-floor move-to)
    (init-field (init '((a) (b) (c)))
                (goal '((a b c))))
    (define (start) init)
    (define (goal? s) (set=? s (get-field goal this)))
    (define (state-eq? s t) (set=? s t))
    (define (move-to-floor block st)
      (if (eq? (caar st) block)
          (cons (cdar st) (cons (list block) (cdr st)))
          (cons (car st) (move-to-floor block (cdr st)))))
    (define (join x xs)
      (if (null? x) xs
          (cons x xs)))
    (define (move-to block dest st)
      (cond
        ((null? st) null)
        ((eq? (caar st) block)
         (join (cdar st) (move-to block dest (cdr st))))
        ((eq? (caar st) dest)
         (cons (cons block (car st)) (move-to block dest (cdr st))))
        (else
         (cons (car st) (move-to block dest (cdr st))))))
    (define (successors-of st)
      (let ((tops (map car st)))
        (foldr
         (lambda (stack xs)
           (foldr
            (lambda (dest ys)
              (if (eq? dest (car stack)) ys
                  (cons (cons (list 'move (car stack) dest)
                              (move-to (car stack) dest st))
                        ys)))
            (if (null? (cdr stack)) xs
                (cons (cons (list 'move (car stack) 'floor)
                            (move-to-floor (car stack) st))
                      xs))
            tops))
         null st)))
    (super-new)))

(define (search-blocks from to)
  (if (set=? (apply append from) (apply append to))
      (time (send (new blocks% (init from) (goal to)) search))
      (printf "Error: start and goal do not contain the same blocks!~n")))

;(search-blocks '((a b c d)) '((b c d) (a)))
;(search-blocks '((a b c d)) '((c d) (b a)))
;(search-blocks '((a b c d)) '((d) (c b a)))
;(search-blocks '((a b c d)) '((d c b a)))
;(search-blocks '((a b c d)) '((b c d a)))
;(search-blocks '((a b c d e)) '((b c d e a)))
