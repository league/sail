;;; c4minimax.scm -- the minimax algorithm and primitives for heuristics
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-11-13 11:46:36 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "c4view.scm")

;; Return last element of vector.
(define (vector-last v)
  (vector-ref v (- (vector-length v) 1)))

;; Functional update of an element in a vector.  (Does not change vector,
;; just returns a new one.)
(define (vector-replace v i x)
  (vector-tabulate 
   (vector-length v)
   (lambda (j)
     (if (= i j) x (vector-ref v j)))))

;; Is there space for another piece at the end of this vector?
(define (space-in-vec? v)
  (= 0 (vector-last v)))

;; Drop a piece into the end of this vector, letting it fall toward 0.
(define (drop-in-vec player v)
  (do ((i (- (vector-length v) 1) (- i 1)))
    ((or (< i 0)
         (not (= 0 (vector-ref v i))))
     (vector-replace v (+ i 1) player))))

;; Is there space for another piece in column #c of the board?
(define (space-in-column? board c)
  (space-in-vec? (vector-ref board c)))

;; Drop a piece into the top of column #c.
(define (drop-in-column player board c)
  (vector-replace board c (drop-in-vec player (vector-ref board c))))

;; Consider all moves, score them, and return a pair of best-move
;; and best-score.
(define (consider-moves score player board)
  (do ((best-move #f)
       (best-score #f)
       (c 0 (+ c 1)))
    ((= c 7)
     (cons best-move best-score))
    (when (space-in-column? board c)
      (let* ((new-board (drop-in-column player board c))
             (new-score (score new-board)))
;        (printf "~v: ~v~n" c new-score)
        (when (or (not best-move)
                  ((if (= player 1) > <) new-score best-score))
          (set! best-move c)
          (set! best-score new-score))))))

;; Here is the main iteration of the minimax algorithm.  When depth hits
;; zero, it's the same as consider-moves, using the given heuristic on 
;; just one next move.  Otherwise, we call recursively and minimax after 
;; one move, scoring with respect to the opposite player.
(define (minimax-iter heuristic depth player board)
  (if (= 0 depth)
      (consider-moves heuristic player board)
      (consider-moves
       (lambda (b) 
         (cdr (minimax-iter heuristic (- depth 1) (- 3 player) b)))
       player board)))

;; Top-level entry for minimax algorithm.
(define (minimax heuristic depth)
  (lambda (player board)
    (car (minimax-iter heuristic depth player board))))

;;; ===================================
;;;   TOOLS FOR DEVELOPING HEURISTICS
;;; ===================================

;; An empty board, for testing.
(define empty-board
  (make-vector 7 (make-vector 6 0)))

(define vr vector-ref)
(define (get board c r)
  (if (and (>= c 0) (< c 7)
           (>= r 0) (< r 6))
      (vector-ref (vector-ref board c) r)
      -1))

  
(define (play h)
  (let ((fr (new c4-frame%
                 (heuristic h)
                 (players (list (cons "H0" (minimax h 0))
                                (cons "H2" (minimax h 2))
                                (cons "H4" (minimax h 4)))))))
    (send fr show #t)
    (get-field game fr)))

(define (h0 b) 0)


(define (xy-heuristic b)
  (cond
    ((= 1 (get b 1 2)) 999)   ; red wins
    ((= 2 (get b 1 2)) -999)  ; blue wins
    (else 0)))

