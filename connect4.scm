;;; connect4-model.scm -- the game-playing engine and user interface
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-10-10 15:33:41 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "stdlib.scm")

;; Represent a single cell in the Connect-4 grid.  It can be empty (0), or
;; hold a piece of player 1 or player 2.  Also, cells can be marked, which 
;; means they should be highlited in some fashion, to emphasize the last 
;; piece played, or to reveal a win.
(define c4-cell%
  (class model%
    (public set-value! get-value mark! marked?)
    (init-field (value 0) (mark #f))
    (define (set-value! v) 
      (set! value v)
      (send this changed))
    (define (get-value) value)
    (define (mark! flag)
      (set! mark (not (not flag)))
      (send this changed))
    (define (marked?) mark)
    (super-new)))

;; Group cells together into a column.  The column keeps track of the top-
;; most piece, so new pieces can be dropped in efficiently.
(define c4-column%
  (class object%
    (public clear! drop! extract get-cell space-left?)
    (init-field (height 6))
    ; Private fields
    (define vec (vector-tabulate height (lambda (i) (new c4-cell%))))
    (define top 0)
    ; Empty out all cells in this column.
    (define (clear!)
      (vector-for-each vec clear-one!)
      (set! top 0))    
    (define (clear-one! i c) 
      (send c set-value! 0)
      (send c mark! #f))
    ; Drop a piece for player X.  Unchecked precondition: there must be
    ; space left!  Returns the index of cell to which the piece dropped.
    (define (drop! x)
      (let ((t top))
        (set! top (+ top 1))
        (send (vector-ref vec t) set-value! x)
        t))
    ; Extract the values of this column's cells as a vector.
    (define (extract) (vector-tabulate height value-at))
    (define (value-at i) (send (vector-ref vec i) get-value))
    (define (get-cell i) (vector-ref vec i))
    (define (space-left?) (< top height))
    (super-new)))

(define (random-player m)
  (let ((w (vector-length m))
        (h (- (vector-length (vector-ref m 0)) 1)))
    (do ((i (random w) (random w)))
      ((= (vector-ref (vector-ref m i) h) 0)
       i))))

;; Group columns together into a grid, and provide the rest of the game-
;; playing logic.
(define c4-game%
  (class model%
    (public clear! extract full? get-column get-cell get-player 
            over? drop-into! take-turn! set-player!)
    (init-field (width 7) (height 6) 
                (p1 random-player) 
                (p2 random-player))
    ; Private fields
    (define turn 1)
    (define last #f)
    (define moves 0)
    (define winner #f)
    (define (make i) (new c4-column% (height height)))
    (define vec (vector-tabulate width make))
    (define (set-player! i f)
      (let ((g (if f f random-player)))
        (if (= i 1)
            (set! p1 g)
            (set! p2 g))))
    ; Empty out all cells and restart game.
    (define (clear!)
      (vector-for-each vec clear-column!)
      (set! turn 1)
      (set! moves 0)
      (set! winner #f)
      (send this changed))
    (define (clear-column! i k) (send k clear!))
    ; Produce a 2D vector with the values from the grid.
    (define (extract) (vector-tabulate width vector-at))
    (define (vector-at i) (send (vector-ref vec i) extract))
    (define (full?) (>= moves (* width height)))
    ; Selectors
    (define (get-column i) (vector-ref vec i))
    (define (get-cell c r) (send (vector-ref vec c) get-cell r))
    (define (get-player) turn)
    (define (over?)
      (or winner (full?)))
    ; One player takes a turn.
    (define (take-turn!)
      (drop-into! ((if (= turn 1) p1 p2) (extract))))
    (define (drop-into! c)
      (when (and (not winner)
                 (send (get-column c) space-left?))
        (let* ((r (send (get-column c) drop! turn))
               (k (get-cell c r)))
          (when last (send last mark! #f))
          (cond
            ((detect-win c r)
             (set! winner turn))
            (else
             (set! turn (- 3 turn))
             (set! moves (+ 1 moves))
             (send k mark! #t)
             (set! last k)))
          (send this changed))))
    ; Detecting wins after placing piece at (c,r)
    (define (detect-win c r)
      (let ((w (check-compass c r)))
        (if w
            (let ((x (car w))
                  (dc (cadr w))
                  (dr (caddr w)))
              (mark-offset c r dc dr (+ x 0))
              (mark-offset c r dc dr (+ x 1))
              (mark-offset c r dc dr (+ x 2))
              (mark-offset c r dc dr (+ x 3))
              #t)
            #f)))
    (define (check-compass c r)
      (or (check-4 c r 1 0)    ; E/W
          (check-4 c r 0 1)    ; N/S
          (check-4 c r 1 1)    ; NW/SE
          (check-4 c r 1 -1))) ; NE/SW (?)
    (define (check-4 c r dc dr)
      (let ((f-3 (check-offset c r dc dr -3))
            (f-2 (check-offset c r dc dr -2))
            (f-1 (check-offset c r dc dr -1))
            (f-0 (check-offset c r dc dr  0))
            (f+1 (check-offset c r dc dr  1))
            (f+2 (check-offset c r dc dr  2))
            (f+3 (check-offset c r dc dr  3)))
        (cond
          ((and f-3 f-2 f-1 f-0) (list -3 dc dr))
          ((and f-2 f-1 f-0 f+1) (list -2 dc dr))
          ((and f-1 f-0 f+1 f+2) (list -1 dc dr))
          ((and f-0 f+1 f+2 f+3) (list  0 dc dr))
          (else #f))))
    (define (check-offset c r dc dr x)
      (safe-check (+ c (* dc x)) (+ r (* dr x))))
    (define (mark-offset c r dc dr x)
      (send (get-cell (+ c (* dc x)) (+ r (* dr x))) mark! #t))
    (define (safe-check c r)
      (if (and (>= c 0) (< c width)
               (>= r 0) (< r height))
          (= turn (send (get-cell c r) get-value))
          #f))
    (super-new)))

;;;;; VIEWS

;; Displays a cell in the Connect-4 grid as a small square canvas.
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
            (p (if (send cell marked?) 4 1)))
        (send dc set-pen *pen-color* p 'solid)
        (send dc set-brush c 'solid)
        (send dc draw-ellipse 4 4 (- w 9) (- h 9))))
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

;; Displays a vertical column of cells.
(define c4-column-ctrl%
  (class vertical-panel%
    (override on-subwindow-event)
    (init-field
     (game (new c4-game%))
     (col 0))
    (define (on-subwindow-event rc ev)
      (case (send ev get-event-type)
        ('left-down (send game drop-into! col))))
    (super-new)
    ; Add cells in reverse order (so that #0 ends up on bottom).
    (do ((r (- (get-field height game) 1) (- r 1)))
      ((< r 0))
      (new c4-cell-view% 
           (cell (send game get-cell col r))
           (parent this)))))

;; Display the grid
(define c4-grid-view%
  (class horizontal-pane%
    (public update)
    (init-field (game (new c4-game%)))
    (define (update . args) #t)
    (super-new)
    (do ((w (get-field width game))
         (i 0 (+ i 1)))
      ((= i w))
      (new c4-column-ctrl%
           (game game) (col i)
           (parent this)))
    (send game attach this)))

(define c4-tool-bar%
  (class vertical-pane%
    (public update)
    (init-field (game (new c4-game%))
                (players null))
    (define (update . args)
      (send msg set-label
            (case (send game over?)
              ('1 (string-append *p1-label* " wins"))
              ('2 (string-append *p2-label* " wins"))
              ('#t "No winner")
              (else
               (if (= 1 (send game get-player))
                   (string-append *p1-label* "'s turn")
                   (string-append *p2-label* "'s turn"))))))
    (super-new)
    (define all-players 
      (append (list (cons "human" #f)
                    (cons "random" random-player))
              players))
    (define names (map car all-players))
    (define r1 (new horizontal-pane% (parent this)))
    (define r2 (new horizontal-pane% (parent this)))
    (define (set-player! i) 
      (lambda (c e)
        (send game set-player! i
              (cdr (list-ref all-players (send c get-selection))))))
    (define p1 (new choice% (label "") (parent r2)
                    (choices names) (callback (set-player! 1))))
    (define p2 (new choice% (label "vs.") (parent r2)
                    (choices names) (callback (set-player! 2))))
    (define clr (new button% (label "Clear") (parent r1) 
                     (callback (lambda (b e) (send game clear!)))))
    (define go (new button% (label "Go") (parent r1)))
    (define msg (new message% (label "©2006 Chris League") (parent r1)))
    (send game attach this)))

(define c4-frame%
  (class frame%
    (init-field 
     (players null)
     (game (new c4-game%)))
    (super-new (label "Connect 4"))
    (new c4-grid-view% (parent this) (game game))
    (new c4-tool-bar% (parent this) (game game) (players players))))

(define bogo-players
  (list (cons "zero" (lambda (m) 0))
        (cons "one" (lambda (m) 1))))
(define fr (new c4-frame% (players bogo-players)))
(define gm (get-field game fr))
(send fr show #t)
