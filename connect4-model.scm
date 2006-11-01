;;; connect4-model.scm -- the underlying game-playing engine
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
    (public set-value! get-value mark! marked? set-num! get-num clear!)
    (init-field (value 0) (mark #f) (num 0))
    (define (clear!)
      (set-value! 0)
      (set-num! 0)
      (mark! #f))
    (define (set-value! v) 
      (set! value v)
      (send this changed))
    (define (get-value) value)
    (define (set-num! x) 
      (set! num x)
      (send this changed))
    (define (get-num) num)
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
    ; Empty out all cells in this column.
    (define (clear!)
      (vector-for-each vec clear-one!))    
    (define (clear-one! i c) 
      (send c clear!))
    ; Drop a piece for player X.  Unchecked precondition: there must be
    ; space left!  Returns the index of cell to which the piece dropped.
    (define (drop! x)
      (do ((t height (- t 1)))
        ((or (= t 0) (not (= 0 (value-at (- t 1)))))
         (send (vector-ref vec t) set-value! x)
         t)))
    ; Extract the values of this column's cells as a vector.
    (define (extract) (vector-tabulate height value-at))
    (define (value-at i) (send (vector-ref vec i) get-value))
    (define (get-cell i) (vector-ref vec i))
    (define (space-left?) (= 0 (value-at (- height 1))))
    (super-new)))

;; This player chooses a column randomly (but if the chosen column is
;; full, it chooses again).
(define (random-player p m)
  (let ((w (vector-length m))
        (h (- (vector-length (vector-ref m 0)) 1)))
    (do ((i (random w) (random w)))
      ((= (vector-ref (vector-ref m i) h) 0)
       i))))

(define (other-player p) (- 3 p))

;; Group columns together into a grid, and provide the rest of the game-
;; playing logic.
(define c4-game%
  (class model%
    (public clear! extract full? get-column get-cell get-player 
            over? drop-into! take-turn! set-player! can-undo? undo!)
    (init-field 
     (width 7) 
     (height 6) 
     (p1 random-player) 
     (p2 random-player))
    ; Private fields
    (define turn 1)
    (define history null)
    (define moves 0)
    (define winner #f)
    (define (make i) (new c4-column% (height height)))
    (define vec (vector-tabulate width make))
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
    (define (can-undo?) (not (null? history)))
    ; Four possibilities: 1 => player 1 wins, 2 => player 2 wins,
    ; #t => board full, #f => game not over yet.
    (define (over?)
      (or winner (full?)))
    ; Replace the auto-play function for player i.
    (define (set-player! i f)
      (let ((g (if f f random-player)))
        (if (= i 1)
            (set! p1 g)
            (set! p2 g)))
      (send this changed))
    (define (undo!)
      (send (car history) clear!)
      (set! history (cdr history))
      (when (not (null? history))
        (send (car history) mark! #t))
      (set! moves (- moves 1))
      (set! turn (other-player turn))
      (set! winner #f)
      (send this changed))
    ; One player takes a turn.
    (define (take-turn!)
      (drop-into! ((if (= turn 1) p1 p2) turn (extract))))
    (define (drop-into! c)
      (when (and (not winner)
                 (send (get-column c) space-left?))
        (let* ((r (send (get-column c) drop! turn))
               (k (get-cell c r)))
          (when (not (null? history))
            (send (car history) mark! #f))
          (cond
            ((detect-win c r)
             (set! winner turn))
            (else
             (send k mark! #t)))
          (set! moves (+ 1 moves))
          (set! turn (other-player turn))
          (set! history (cons k history))
          (send k set-num! moves)
          (send this changed))))
    ; Detecting wins after placing piece at (c,r)
    (define (detect-win c r)
      (let ((w (check-compass c r)))
        (if w (mark-win c r (cadr w) (caddr w) (car w))
            #f)))
    (define (mark-win c r dc dr x)
      (mark-offset c r dc dr (+ x 0))
      (mark-offset c r dc dr (+ x 1))
      (mark-offset c r dc dr (+ x 2))
      (mark-offset c r dc dr (+ x 3))
      #t)
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
