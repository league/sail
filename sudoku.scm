;;; sudoku.scm -- solving Sudoku games with constraint propagation
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-11-01 11:50:48 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "ac3algo.scm")

(define sudoku%
  (class ac3algo%
    (override constraint-on constraint-between partners-of)
    (init-field (n 2) (nn (* n n)))
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
    (override on-char on-focus)
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
      (when (send this has-focus?)
        (send dc set-brush "yellow" 'solid)
        (send dc draw-rectangle 0 0
              (send this get-width) (send this get-height)))
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
    (define (on-char ch)
      (case (send ch get-key-code)
        ((#\1 numpad1) (set-domain '(1)))
        ((#\2 numpad2) (set-domain '(2)))
        ((#\3 numpad3) (set-domain '(3)))
        ((#\4 numpad4) (set-domain '(4)))
        ((#\5 numpad5) (set-domain '(5)))
        ((#\6 numpad6) (set-domain '(6)))
        ((#\7 numpad7) (set-domain '(7)))
        ((#\8 numpad8) (set-domain '(8)))
        ((#\9 numpad9) (set-domain '(9)))
        ((#\space) (set-domain (get-field values puz)))
        ((up) (refocus 'up))
        ((down) (refocus 'down))
        ((left) (refocus 'left))
        ((right) (refocus 'right))))
    (define (set-domain d)
      (send puz set-domain-of id d)
      (refocus 'next))
    (define (refocus dir)
      (send (send (send this get-parent) get-parent) refocus id dir))
    (define (on-focus on?)
      (send this refresh))
    (super-new
     (min-width 32) (min-height 32)
     (paint-callback paint-me) (style '(border)))
    (send puz attach this)))
     
(define sudoku-row-view%
  (class horizontal-pane%
    (init-field (puz (new sudoku%)) (row 1) (cells #f))
    (define (each-cell i)
      (let* ((j (+ i 1))
             (c (new sudoku-cell-view% (puz puz) (id (cons row j))
                     (parent this))))
        (when (zero? (modulo j (get-field n puz)))
          (new canvas% (parent this) (min-width 2) (min-height 2)))
        c))
    (super-new)
    (new message% (label (number->string row)) (parent this))
    (set! cells (vector-tabulate (get-field nn puz) each-cell))))

(define sudoku-grid-view%
  (class vertical-pane%
    (public refocus)
    (init-field (puz (new sudoku%)) (rows #f))
    (define (refocus id dir)
      (let ((r (- (car id) 1))
            (c (- (cdr id) 1)))
        (case dir
          ((right next) (set! c (+ c 1)))
          ((left) (set! c (- c 1)))
          ((up) (set! r (- r 1)))
          ((down) (set! r (+ r 1))))
        (when (and (eq? dir 'next) (>= c (get-field nn puz)))
          (set! c 0)
          (set! r (+ r 1)))
        (set! r (modulo r (get-field nn puz)))
        (set! c (modulo c (get-field nn puz)))
        (send (vector-ref (get-field cells (vector-ref rows r)) c) focus)))
    (define (each-row i)
      (let* ((j (+ i 1))
             (r (new sudoku-row-view% (puz puz) (row j) (parent this))))
       (when (zero? (modulo j (get-field n puz)))
         (new canvas% (parent this) (min-width 2) (min-height 2)))
        r))
    (super-new)
    (set! rows (vector-tabulate (get-field nn puz) each-row))))

(define sudoku-controls%
  (class ac3controls%
    (super-new)
    (define (set-row puz r vs)
      (foldl (lambda (v i)
               (when v
                 (send puz set-domain-of (cons r i) (list v)))
               (+ 1 i))
             1 vs))        
    ;; a few hard-coded puzzles from websudoku.com, until I learn to generate.
    ;; Easy Puzzle 1,660,624,960
    (define (easy b e)
      (let ((puz (get-field m this)))
        (send puz reset)
        (set-row puz 1 '(#f #f #f  #f #f #f   9  1 #f))
        (set-row puz 2 '(#f  6 #f   2 #f #f  #f  3 #f))
        (set-row puz 3 '( 8  3 #f   6  7  1   4  5 #f))
        (set-row puz 4 '(#f #f #f  #f #f  9  #f #f  8))
        (set-row puz 5 '( 4 #f #f   1  2  5  #f #f  9))
        (set-row puz 6 '( 2 #f #f   7 #f #f  #f #f #f))
        (set-row puz 7 '(#f  2  8   4  5  7  #f  9  3))
        (set-row puz 8 '(#f  7 #f  #f #f  3  #f  2 #f))
        (set-row puz 9 '(#f  5  4  #f #f #f  #f #f #f))))
    ;; Medium Puzzle 238,363,500
    (define (medium b e)
      (let ((puz (get-field m this)))
        (send puz reset)
        (set-row puz 1 '(#f #f  2  #f #f  4  #f  9 #f))
        (set-row puz 2 '(#f #f  5  #f #f  3   6 #f #f))
        (set-row puz 3 '( 3 #f #f  #f  7 #f  #f  2  5))
        (set-row puz 4 '(#f #f #f   4 #f #f   1 #f #f))
        (set-row puz 5 '( 4 #f  6   5 #f  1   8 #f  9))
        (set-row puz 6 '(#f #f  8  #f #f  2  #f #f #f))
        (set-row puz 7 '( 8  9 #f  #f  1 #f  #f #f  6))
        (set-row puz 8 '(#f #f  3   8 #f #f   9 #f #f))
        (set-row puz 9 '(#f  6 #f   2 #f #f   7 #f #f))))
    ;; Evil Puzzle 6,017,119,400
    (define (evil b e)
      (let ((puz (get-field m this)))
        (send puz reset)
        (set-row puz 1 '(#f  5 #f   9 #f  4   6  3 #f))
        (set-row puz 2 '(#f #f  9  #f #f #f  #f #f #f))
        (set-row puz 3 '( 3 #f #f  #f  5 #f   7 #f #f))
        (set-row puz 4 '( 4 #f  1  #f #f #f  #f  7 #f))
        (set-row puz 5 '(#f #f #f   3 #f  8  #f #f #f))
        (set-row puz 6 '(#f  3 #f  #f #f #f   2 #f  5))
        (set-row puz 7 '(#f #f  4  #f  1 #f  #f #f  6))
        (set-row puz 8 '(#f #f #f  #f #f #f   9 #f #f))
        (set-row puz 9 '(#f  8  2   5 #f  7  #f  4 #f))))
        
    (new button% (label "Easy") (parent this) (callback easy))
    (new button% (label "Medium") (parent this) (callback medium))
    (new button% (label "Evil") (parent this) (callback evil))))
  
  
(define sudoku-frame%
  (class frame%
    (init-field (n 3) (puz (new sudoku% (n n))))
    (super-new (label "Sudoku"))
    (define h (new horizontal-pane% (parent this)))
    (new sudoku-grid-view% (puz puz) (parent h))
    (new sudoku-controls% (m puz) (parent h))))

(define fr (new sudoku-frame%))
(define p (get-field puz fr))
(send fr show #t)