;;; crossword.scm -- constraint solver for generating crossword puzzles
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-10-10 15:33:41 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "ac3algo.scm")

(define crossword%
  (class ac3algo%
    (override constraint-on constraint-between)    
    (public all-columns all-rows)
    (init-field 
     (rows 5)
     (columns 6)
     (words '("apple" "banana" "carrot" "date" "nap" "cab")))
    (define (constraint-on word) 
      (lambda (pos)
        (case (caddr pos)
          ('down (<= (+ (car pos) (string-length word) -1) rows))
          ('across (<= (+ (cadr pos) (string-length word) -1) columns)))))
    (define (constraint-between w1 w2)
      (lambda (p1 p2)
        (let* ((d1 (caddr p1))
               (d2 (caddr p2))
               (x1 (if (eq? d1 'across) (cadr p1) (car p1)))
               (x2 (if (eq? d2 'across) (cadr p2) (car p2)))
               (y1 (if (eq? d1 'down) (cadr p1) (car p1)))
               (y2 (if (eq? d2 'down) (cadr p2) (car p2)))
               (x1e (+ x1 (string-length w1) -1))
               (x2e (+ x2 (string-length w2) -1)))
          (cond
            ((eq? d1 d2)  
             ; words go in same direction
             (cond
               ; do they run too close together?
               ((< (abs (- y1 y2)) 2) #f)
               (else #t)))
            ; words go in opposite direction; do they intersect?
            ((and (<= x2 y1) (<= y1 x2e)
                  (<= x1 y2) (<= y2 x1e))
             ; they intersect, do the letters match up?
             (char=? (string-ref w1 (- y2 x1))
                     (string-ref w2 (- y1 x2))))
            ; words don't intersect, but do they come too close?
            ((or (= 1 (- y2 x1e))
                 (= 1 (- x1 y2))
                 (= 1 (- y1 x2e))
                 (= 1 (- x2 y1))) #f)
            (else #t)))))
    (define (all-columns r c L)
      (if (> c columns)
          L
          (all-columns r (+ 1 c)
                       (cons (list r c 'across)
                             (cons (list r c 'down) L)))))
    (define (all-rows r L)
      (if (> r rows) L
          (all-rows (+ 1 r) (all-columns r 1 L))))
    (super-new
     (variables words)
     (values (all-rows 1 null)))))

(define crossword-row%
  (class horizontal-pane%
    (public clear set)
    (init-field (puz (new crossword%)) (row 1))
    (define (clear)
      (vector-for-each v (lambda (i m) (send m set-label ""))))
    (define (set i ch)
      (send (vector-ref v (- i 1)) set-label (make-string 1 ch)))
    (super-new)
    (new message% (label (number->string row)) (parent this)
         (min-width 32))
    (define v 
      (vector-tabulate 
       (get-field columns puz)
       (lambda (i) (new message% (label "ZZ") (parent this)))))))

(define crossword-grid%
  (class vertical-pane%
    (public update clear)
    (init-field (puz (new crossword%)))
    (define (update . args)
      (clear)
      (for-each each (get-field variables puz)))
    (define (each w)
      (let ((d (send puz domain-of w)))
        (when (and (not (null? d)) (null? (cdr d)))
          (if (eq? 'down (caddar d))
              (write-down w (caar d) (cadar d))
              (write-across w (caar d) (cadar d))))))
    (define (write-across w r c)
      (let ((rv (vector-ref v (- r 1))))
        (for-loop 
         0 (string-length w)
         (lambda (i)
           (send rv set (+ c i) (string-ref w i))))))
    (define (write-down w r c)
      (for-loop
       0 (string-length w)
       (lambda (i)
         (send (vector-ref v (+ r i -1))
               set c (string-ref w i)))))
    (define (clear)
      (vector-for-each v (lambda (i r) (send r clear))))
    (super-new)
    (define v
      (vector-tabulate
       (get-field rows puz)
       (lambda (i) (new crossword-row% (puz puz) 
                        (row (+ i 1)) (parent this)))))
    (clear)
    (send puz attach this)))

(define crossword-frame%
  (class frame%
    (init-field (puz (new crossword%)))
    (super-new (label "crossword"))
    (define h (new horizontal-pane% (parent this)))
    (new crossword-grid% (puz puz) (parent h))
    (new ac3controls% (m puz) (parent h))))

(define z 
  (new crossword% (rows 6) (columns 6)
       (words '("do" "let" "run" "car" "cdr" 
                     "case" "cond"))))
(define f (new crossword-frame% (puz z)))
(send f show #t)
;(send z search)

