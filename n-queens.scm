;;; n-queens.scm -- solving N queens puzzle with constraint propagation
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-10-10 15:33:41 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "ac3algo.scm")

(define n-queens%
  (class ac3algo%
    (override constraint-on constraint-between)    
    (public enum)
    (init-field (n 4))
    (define (constraint-on r) (lambda (c) #t))
    (define (constraint-between r1 r2)
      (lambda (c1 c2)
        (cond
          ((= r1 r2) #f)  ; same row
          ((= c1 c2) #f)  ; same column
          ((= (abs (- r1 r2)) (abs (- c1 c2))) #f) ; diagonal
          (else #t))))
    (define (enum i)
      (if (= i n) (list i)
          (cons i (enum (+ i 1)))))
    (super-new
     (variables (enum 1))
     (values (enum 1)))))

(define f (new n-queens%))
(define q (new n-queens% (n 8)))

(define queens-header%
  (class horizontal-pane%
    (init-field (q (new n-queens%)))
    (super-new)
    (new message% (label "") (parent this) 
         (min-width 16) (min-height 8))
    (for-loop
     1 (+ (get-field n q) 1)
     (lambda (c) 
       (new message% (label (number->string c)) (parent this)
            (min-width 16) (min-height 8))))))

(define queens-row-view%
  (class horizontal-pane%
    (public update)
    (init-field (q (new n-queens%)) (row 1))
    (define (update . args)
      (let ((d (send q domain-of row)))
        (vector-for-each 
         v (lambda (i x)
             (send x set-label (if (element-of? (+ 1 i) d) "X" " "))))))
    (super-new)
    (new message% (label (number->string row)) (parent this)
         (min-width 16) (min-height 8))
    (define v 
      (vector-tabulate 
       (get-field n q)
       (lambda (c) (new message% (label "X") (min-height 8)
                        (min-width 16) (parent this)))))
    (send q attach this)))

(define queens-board-view%
  (class vertical-pane%
    (init-field (q (new n-queens%)))
    (super-new)
    (new queens-header% (q q) (parent this))
    (for-loop 
     1 (+ 1 (get-field n q))
     (lambda (i) (new queens-row-view% (q q) (row i) (parent this))))))

(define queens-frame%
  (class frame%
    (init-field (n 4) (q (new n-queens% (n n))))
    (super-new (label "queens"))
    (define h (new horizontal-pane% (parent this)))
    (new queens-board-view% (q q) (parent h))
    (new ac3controls% (m q) (parent h))))

(define fr (new queens-frame% (n 8)))
(send fr show #t)
