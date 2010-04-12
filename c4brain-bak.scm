;;; c4minimax.scm -- primitives for creating smart computer players
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-11-13 07:38:49 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "c4view.scm")

;; This makes coding easier by not worrying about boundary conditions:
;; just return 1/2 if space is occupied, 0 if empty, or -1 if off board.
(define (safe-get board c r)
  (if (and (>= c 0) (< c 7)
           (>= r 0) (< r 6))
      (vector-ref (vector-ref board c) r)
      -1))

(define (relative-get board c r x y)
  (lambda (i)
    (safe-get board (+ c (* i x)) (+ r (* i y)))))

(define (to-n  b c r) (relative-get b c r 0 1))
(define (to-ne b c r) (relative-get b c r 1 1))
(define (to-e  b c r) (relative-get b c r 1 0))
(define (to-se b c r) (relative-get b c r 1 -1))
(define (to-s  b c r) (relative-get b c r 0 -1))
(define (to-sw b c r) (relative-get b c r -1 -1))
(define (to-w  b c r) (relative-get b c r -1 0))
(define (to-nw b c r) (relative-get b c r -1 1))

;; Classification of threats:
;;   * Win:          XXXX             'max
;;   * Near win:     ?XXX-           32768
;;   * Major threat: ?XXX X?XX        4096
;;   * Minor threat: ?XX- X?X- -?XX    512
;;                   ?-XX ?X-X X?-X     64
;;   * Consecutive:  ?X-- X?--           8
;;   * Room to grow: ?--- -?--           1
;; With bonus points for even threats by player 1 (rows 0 2 4)
;; and odd threats by player 2 (rows 1 3 5), and for squares that
;; participate in multiple threats.
(define (threat-class b p get)
  (cond
    ((= p (get 0) (get 1) (get 2) (get 3)) 2147483648) ; already a win
    ((not (= 0 (get 0))) 0)                         ; occupied -> not threat
    ((and (= 0 (get 4))                             ; near win
          (= p (get 1) (get 2) (get 3))) 131072)
    ((or (= p (get 1) (get 2) (get 3))              ; major threat
         (= p (get -1) (get 1) (get 2))) 4096)
    ((or (and (= 0 (get 3)) (= p (get 1) (get 2)))  ; minor threat I
         (and (= 0 (get 2)) (= p (get -1) (get 1)))
         (and (= 0 (get -1)) (= p (get 1) (get 2)))) 512)
    ((or (and (= 0 (get 1)) (= p (get 2) (get 3)))  ; minor threat II
         (and (= 0 (get 2)) (= p (get 1) (get 3)))
         (and (= 0 (get 1)) (= p (get -1) (get 2)))) 64) 
    ((or (and (= p (get 1)) (= 0 (get 2) (get 3)))  ; consecutive
         (and (= p (get -1)) (= 0 (get 1) (get 2)))) 8)
    ((or (= 0 (get 1) (get 2) (get 3))
         (= 0 (get -1) (get 1) (get 2))) 1)
    (else 0)))

;; Enumerate scores for threats in all directions from position (c,r).
(define (list-threats-from b p c r)
  (list (threat-class b p (to-n  b c r))
        (threat-class b p (to-ne b c r))
        (threat-class b p (to-e  b c r))
        (threat-class b p (to-se b c r))
        (threat-class b p (to-s  b c r))
        (threat-class b p (to-sw b c r))
        (threat-class b p (to-w  b c r))
        (threat-class b p (to-nw b c r))))

;; Combine the above threats by adding them.  If there is >1 threat,
;; incorporate a multiplicative factor.  Also consider odd/even rows.
(define (score-threats-from b p c r)
  (let* ((ls (list-threats-from b p c r))
         (pos (filter (lambda (x) (> x 0)) ls))
         (sum (foldr + 0 pos))
         (mult (+ 1 (* 0.1 (length pos))))
         (bonus (if (or (and (= p 1) (even? r))
                        (and (= p 2) (odd? r)))
                    1.1 1)))
    (* sum mult bonus)))

(define (score-all-threats b p)
  (let ((x 0))
    (vec2d-for-each
     b (lambda (c r z)
         (set! x (+ x (score-threats-from b p c r)))))
    x))

(define (other p) (- 3 p))

(define (new-h p b)
  (round 
   (- (score-all-threats b p)
      (* 1.4 (score-all-threats b (other p))))))


;;; =====================
;;;   MINIMAX ALGORITHM
;;; =====================

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

;; An empty board, for testing.
(define empty-board
  (make-vector 7 (make-vector 6 0)))

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
             (new-score (score player new-board)))
        (when (or (not best-move)
                  (> new-score best-score))
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
       (lambda (p b) 
         (cdr (minimax-iter heuristic (- depth 1) (- 3 p) b)))
       player board)))

;; Top-level entry for minimax algorithm.
(define (minimax heuristic depth)
  (lambda (player board)
    (car (minimax-iter heuristic depth player board))))

;; A totally stupid heuristic that always returns 0.
(define (h0 p b) 0)

;;; ===================================
;;;   TOOLS FOR DEVELOPING HEURISTICS
;;; ===================================

(define (quad-from get x f)
  (f (get x)
     (get (+ x 1))
     (get (+ x 2))
     (get (+ x 3))))

(define (n-quads-from n get x f)
  (if (= 1 n)
      (quad-from get x f)
      (+ (quad-from get x f)
         (n-quads-from (- n 1) get (+ x 1) f))))

(define (horiz-quads-in board row f)
  (let ((get (lambda (c) (vector-ref (vector-ref board c) row))))
    (n-quads-from 4 get 0 f)))

(define (sum-from n g)
  (if (= 0 n)
      (g 0)
      (+ (g n)
         (sum-from (- n 1) g))))

(define (all-horiz-quads board f)
  (sum-from 5 (lambda (r) (horiz-quads-in board r f))))
                                
(define (vert-quads-in board col f)
  (let ((get (lambda (r) (vector-ref (vector-ref board col) r))))
    (n-quads-from 3 get 0 f)))

(define (all-vert-quads board f)
  (sum-from 6 (lambda (c) (vert-quads-in board c f))))

(define (all-northeast-quads board f)
  (let ((get (lambda (c r)
               (lambda (i)
                 (vector-ref (vector-ref board (+ c i)) (+ r i))))))
    (+ (n-quads-from 1 (get 0 2) 0 f)
       (n-quads-from 2 (get 0 1) 0 f)
       (n-quads-from 3 (get 0 0) 0 f)
       (n-quads-from 3 (get 1 0) 0 f)
       (n-quads-from 2 (get 2 0) 0 f)
       (n-quads-from 1 (get 3 0) 0 f))))

(define (all-northwest-quads board f)
  (let ((get (lambda (c r)
               (lambda (i)
                 (vector-ref (vector-ref board (- c i)) (+ r i))))))
    (+ (n-quads-from 1 (get 3 0) 0 f)
       (n-quads-from 2 (get 4 0) 0 f)
       (n-quads-from 3 (get 5 0) 0 f)
       (n-quads-from 3 (get 6 0) 0 f)
       (n-quads-from 2 (get 6 1) 0 f)
       (n-quads-from 1 (get 6 2) 0 f))))

(define (all-quads board f)
  (+ (all-horiz-quads board f)
     (all-vert-quads board f)
     (all-northeast-quads board f)
     (all-northwest-quads board f)))

(define (how-many p a b c d)
  (+ (if (= p a) 1 0)
     (if (= p b) 1 0)
     (if (= p c) 1 0)
     (if (= p d) 1 0)))

(define (magnitude-for n)
  (case n
    ('0 0)
    ('1 4)
    ('2 16)
    ('3 64)
    ('4 32768)))

(define defense-factor 1.5)

(define (score-quad p)
  (lambda (a b c d)
    (let ((mine (how-many p a b c d))
          (his (how-many (- 3 p) a b c d)))
      (cond
        ; If we both occupy, nobody can win here
        ((and (> mine 0) (> his 0)) 0)
        ; If I have pieces here, score is positive
        ((> mine 0) (magnitude-for mine))
        ; If he has pieces here, score is negative    
        ((> his 0) (- (* defense-factor (magnitude-for his))))
        ; If nobody has pieces here, score is zero
        (else 0)))))

(define (my-heuristic player board)
  (all-quads board (score-quad player)))

(define fr
  (new c4-frame%
       (heuristic new-h)
       (players (list (cons "dumb 1" (minimax h0 1))
                      (cons "new 0" (minimax new-h 0))
                      (cons "old 0" (minimax my-heuristic 0))
                      (cons "new 2" (minimax new-h 2))
                      (cons "old 2" (minimax my-heuristic 2))))))
(define gm
  (get-field game fr))
(send fr show #t)

