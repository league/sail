;;; sudoku.scm -- solving Sudoku games with constraint propagation
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-11-01 11:50:48 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "ac3algo.scm")

(define sudoku%
  (class ac3algo%
    (override constraint-on constraint-between partners-of)
    (init-field (n 2))
    (define (enum i)
      (if (= i 0) null
          (cons i (enum (- i 1)))))
    (define vals (reverse (enum (* n n))))
    (define vars 
      (foldr
       (lambda (x1 L1)
         (foldr
          (lambda (x2 L2) (cons (cons x1 x2) L2))
          L1 vals))
       null vals))
    (define (constraint-on c) (lambda (v) #t))
    (define (cluster-div i)
      (floor (/ (- i 1) n)))
    (define (cluster-id c)
      (cons (cluster-div (car c))
            (cluster-div (cdr c))))
    (define (arc? c1)
      (lambda (c2)
        (and (not (equal? c1 c2))
             (or (= (car c1) (car c2))
                 (= (cdr c1) (cdr c2))
                 (equal? (cluster-id c1) (cluster-id c2))))))
    (define (partners-of c)
      (filter (arc? c) vars))
    (define (constraint-between c1 c2)
      (lambda (v1 v2) (not (= v1 v2))))
    (super-new
     (values vals)
     (variables vars))))

(define s (new sudoku%))

(define sudoku-cell-view%
  (class canvas%
    (public update)
    (override on-event)
    (init-field 
     (puz (new sudoku%))
     (id '(1 . 1)))
    (define cache (send puz domain-of id))
    (define (update . args)
      (let ((d (send puz domain-of id)))
        (when (not (equal? d cache))
          (set! cache d)
          (send this refresh))))
    (define (paint-me cv dc)
      (cond
        ((null? cache)        ; no solution
         (send dc set-brush "red" 'solid)
         (send dc draw-rectangle 0 0
               (send this get-width) (send this get-height)))
        ((null? (cdr cache))  ; singleton
         (send dc draw-text (number->string (car cache)) 4 4))
        (else
         (for-each
          (lambda (k)
            (case k
              ('1 (send dc draw-rectangle  4  4 2 2))
              ('2 (send dc draw-rectangle 10  4 2 2))
              ('3 (send dc draw-rectangle 16  4 2 2))
              ('4 (send dc draw-rectangle  4 10 2 2))
              ('5 (send dc draw-rectangle 10 10 2 2))
              ('6 (send dc draw-rectangle 16 10 2 2))
              ('7 (send dc draw-rectangle  4 16 2 2))
              ('8 (send dc draw-rectangle 10 16 2 2))
              ('9 (send dc draw-rectangle 16 16 2 2))))
          cache))))
    (define n2 (square (get-field n puz)))
    (define (on-event ev)
      (case (send ev get-event-type)
        ('left-down 
          (let ((d (send puz domain-of id))
                (vs (get-field values puz)))
            (send puz set-domain-of id
                  (cond
                    ((null? d) vs)
                    ((null? (cdr d))
                     (if (= (car d) n2) vs
                         (list (+ (car d) 1))))
                    (else (list (car d)))))))))
    (super-new
     (min-width 32) (min-height 32)
     (paint-callback paint-me) (style '(border)))
    (send puz attach this)))
     
(define sudoku-row-view%
  (class horizontal-pane%
    (init-field (puz (new sudoku%)) (row 1))
    (super-new)
    (new message% (label (number->string row)) (parent this))
    (for-loop 
     1 (+ (square (get-field n puz)) 1)
     (lambda (c) 
       (new sudoku-cell-view% (puz puz) (id (cons row c))
            (parent this))
       (when (zero? (modulo c (get-field n puz)))
         (new canvas% (parent this) (min-width 2) (min-height 2)))))))

(define sudoku-grid-view%
  (class vertical-pane%
    (init-field (puz (new sudoku%)))
    (super-new)
    (for-loop
     1 (+ (square (get-field n puz)) 1)
     (lambda (r) 
       (new sudoku-row-view% (puz puz) (row r) (parent this))
       (when (zero? (modulo r (get-field n puz)))
         (new canvas% (parent this) (min-width 2) (min-height 2)))))))

(define sudoku-frame%
  (class frame%
    (init-field (n 3) (puz (new sudoku% (n n))))
    (super-new (label "Sudoku"))
    (define h (new horizontal-pane% (parent this)))
    (new sudoku-grid-view% (puz puz) (parent h))
    (new ac3controls% (m puz) (parent h))))

(define fr (new sudoku-frame%))
(define p (get-field puz fr))
(send fr show #t)