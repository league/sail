;;; c4view.scm -- the graphics and user interface
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-11-13 11:46:30 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "c4model.scm")

;; Displays a cell in the Connect-4 grid as a small square canvas. Marked
;; cells show a heavy ring around them.
(define c4-cell-view%
  (class canvas%
    (public update)
    (init-field (cell (new c4-cell%)))
    (define (update . args)
      (send this refresh))
    (define (paint-cell cv dc)
      (let ((w (send cv get-width))
            (h (send cv get-height))
            (c (case (send cell get-value)
                 ('1 *p1-color*) 
                 ('2 *p2-color*) 
                 (else *blank-color*)))
            (p (if (send cell marked?) 4 1))
            (n (send cell get-num)))
        (send dc set-pen *pen-color* p 'solid)
        (send dc set-brush c 'solid)
        (send dc draw-ellipse 4 4 (- w 9) (- h 9))
        (when (> n 0)
          (send dc draw-text (number->string n) 12 12))))
    (super-new (min-width 48) (min-height 48)
               (paint-callback paint-cell))
    (send this set-canvas-background *board-color*)
    (send cell attach this)))

;; Color defaults
(define *board-color* (send the-color-database find-color "yellow"))
(define *blank-color* "white")
(define *p1-color* "red")
(define *p1-label* "Red")
(define *p2-color* "blue")
(define *p2-label* "Blue")
(define *pen-color* "black")

;; Displays a clickable vertical column of cells.  Clicking the column drops
;; a piece into that column.
(define c4-column-ctrl%
  (class vertical-panel%
    (public enable!)
    (override on-subwindow-event)
    (init-field
     (game (new c4-game%))
     (col 0)
     (on #t))
    (define (enable! flag)
      (set! on flag))
    (define (on-subwindow-event rc ev)
      (when on
        (case (send ev get-event-type)
          ('left-down (send game drop-into! col)))))
    (super-new)
    ; Add cells in reverse order (so that #0 ends up on bottom).
    (do ((r (- (get-field height game) 1) (- r 1)))
      ((< r 0))
      (new c4-cell-view% 
           (cell (send game get-cell col r))
           (parent this)))))

;; Display the grid
(define c4-frame%
  (class frame%
    (public update clickable!)
    (init-field
     (game (new c4-game%))
     (players null)
     (heuristic #f))  ; for debugging heuristics
    (define (update . args)
      (let* ((p (send game get-player))
             (ov (send game over?))
             (st1 (case ov
                    ('1 (string-append *p1-label* " wins"))
                    ('2 (string-append *p2-label* " wins"))
                    ('#t "No winner")
                    (else
                     (if (= 1 p)
                         (string-append *p1-label* "'s turn")
                         (string-append *p2-label* "'s turn")))))
             (st2 (if heuristic
                      (number->string 
                       (heuristic (send game extract)))
                      st1)))
        (send msg set-label st2)
        (send un enable (send game can-undo?))
        (cond
          ((= 0 (send (if (= 1 p) p1 p2) get-selection)) ; human
           (clickable! #t))
          (else
           (clickable! #f)
           (when (not ov)
             (yield)
             (send game take-turn!))))))
    ; Turn the clickable columns on or off.  They should be on only when it is
    ; a human player's turn.
    (define (clickable! flag)
      (for-each 
       (lambda (c) (send c enable! flag))
       (send gr get-children)))
    (super-new (label "Connect 4"))
    ; Grid contains each column.
    (define gr (new horizontal-pane% (parent this)))
    (do ((w (get-field width game))
         (i 0 (+ i 1)))
      ((= i w))
      (new c4-column-ctrl%
           (game game) (col i) (parent gr)))
    ; Toolbar 1 contains buttons, messages.
    (define tb1 (new horizontal-pane% (parent this)))
    (define clr (new button% (label "Clear") (parent tb1) 
                     (callback (lambda (b e) (send game clear!)))))
    (define un (new button% (label "Undo") (parent tb1) (enabled #f)
                    (callback (lambda (b e) (send game undo!)))))
    (define msg (new message% (label "©2006 Chris League") (parent tb1)))
    ; Toolbar 2 contains player choice selectors.
    (define tb2 (new horizontal-pane% (parent this)))
    (define player-fns (cons #f (cons random-player (map cdr players))))
    (define player-names (cons "human" (cons "random" (map car players))))
    (define (set-player! i) 
      (lambda (c e)
        (let ((f (list-ref player-fns (send c get-selection))))
          (send game set-player! i f)
          (when (= i (send game get-player))
            (clickable! (not f))
            (when f
              (yield)
              (send game take-turn!))))))
    (define p1 (new choice% (label "") (parent tb2)
                    (choices player-names) (callback (set-player! 1))))
    (define p2 (new choice% (label "vs.") (parent tb2)
                    (choices player-names) (callback (set-player! 2))))
    ; Subscribe to game model.
    (send game attach this)))

(define (play)
  (let ((fr (new c4-frame%)))
    (send fr show #t)
    fr))
