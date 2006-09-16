;;; grid-model.scm -- models representing grid worlds and inhabitants
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-09-12 20:08:04 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "stdlib.scm")

;; In the grid world, a 'room' is made up of 'cells' and 'walls'.
;; We don't need a model for walls, because they never change.
;; A cell can hold a robot, or a trace of where the robot went.
(define cell-model%
  (class model%
    (public clear place-robot robot? set-direction get-direction)
    (define robot #f)
    (define dir #f)
    (define (robot?) robot)
    (define (place-robot) 
      (set! robot #t)
      (send this changed))
    (define (get-direction) dir)
    (define (set-direction d)
      (set! dir d)
      (send this changed))
    (define (clear)
      (set! robot #f)
      (set! dir #f)
      (send this changed))
    (super-new)))

;; To define a room, we provide a template in the form of a list
;; of strings.  Within each string, a space represents an open cell
;; and 'x' indicates an obstacle.  The room is automatically surrounded
;; by walls on 4 sides, so you need not include those.
(define room-1
  '("           xxx"
    "           xxx"
    "           xxx"
    "           xxx"
    "  xxxxxx      "
    "  xx  xx      "
    "  xx  xx      "
    "           xxx"
    "           xxx"
    "     xxx   xxx"
    "     xxx   xxx"))

(define room-2
  '("xxxx  xx     "
    "xxx   xx     "
    "xx     x    x"
    "x      x    x"
    "     xxx     "
    "             "
    "             "
    "      xx    x"
    "     xx      "
    "             "
    "xx           "
    "xxx          "))

;; The brain of the robot is a decision tree made of if/and/or/not,
;; sensors s1-s8, and the directions north/east/south/west.  Here is
;; a pretty dumb controller that goes south until it hits a boundary.
(define controller-1 '(if s6 'north 'south))

(define (prep-controller tree)
  (eval (list 'lambda '(s1 s2 s3 s4 s5 s6 s7 s8) tree)))
  
;; A room is modelled by a 2D vector of cells or walls (represented
;; just by #f).
(define room-model%
  (class model%
    (public get-nrows get-ncols get-cell set-controller
            robot? place-robot take-one-step take-n-steps clear)
    (init-field (template room-1)
                (controller controller-1))
    (define control-fn (prep-controller controller))
    (define lastr (+ 1 (length template)))              ; last row
    (define lastc (+ 1 (string-length (car template)))) ; last column
    (define nrows (+ 1 lastr))
    (define ncols (+ 1 lastc))
    (define botr #f)    ; position of the robot (if applicable)
    (define botc #f)
    (define (robot?) (if botr #t #f))
    (define (cell-from-tmpl r c)
      (if (and (> r 0) (> c 0) (< r lastr) (< c lastc)
               (let* ((row (list-ref template (- r 1)))
                      (ch (string-ref row (- c 1))))
                 (eq? #\space ch)))
          (new cell-model%)
          #f))
    (define cells (vec2d-tabulate nrows ncols cell-from-tmpl))
    (define (get-nrows) nrows)
    (define (get-ncols) ncols)
    (define (get-cell r c) (vector-ref (vector-ref cells r) c))
    (define (set-controller tree) 
      (set! controller tree)
      (set! control-fn (prep-controller tree)))
    (define (remove-robot)
      (cond
        (botr (send (get-cell botr botc) clear)
              (set! botr #f)
              (set! botc #f))))
    (define (place-robot r c)
      (let ((cm (get-cell r c)))
        (cond 
          (cm (remove-robot)
              (send cm place-robot)
              (set! botr r)
              (set! botc c)
              (send this changed))
          (else (error "Attempt to place robot in wall:" r c)))))
    (define (move-robot dir)
      (cond
        (botr 
         (let ((br botr) 
               (bc botc)
               (r (+ botr (case dir ('north -1) ('south +1) (else 0))))
               (c (+ botc (case dir ('east +1) ('west -1) (else 0)))))
           (send this place-robot r c)
;;           (printf "Robot moved ~s to ~s,~s~n" dir r c)
           (send (get-cell br bc) set-direction dir)))
        (else (error "There is no robot here to move"))))
    (define (clear)
      (vec2d-for-each cells (lambda (i j elt)
                              (when elt (send elt clear))))
      (set! botr #f)
      (set! botc #f)
      (send this changed))
    (define (take-one-step)
      (cond
        (botr
         (let ((s1 (not (get-cell (- botr 1) (- botc 1))))
               (s2 (not (get-cell (- botr 1)    botc   )))
               (s3 (not (get-cell (- botr 1) (+ botc 1))))
               (s4 (not (get-cell    botr    (+ botc 1))))
               (s5 (not (get-cell (+ botr 1) (+ botc 1))))
               (s6 (not (get-cell (+ botr 1)    botc   )))
               (s7 (not (get-cell (+ botr 1) (- botc 1))))
               (s8 (not (get-cell    botr    (- botc 1)))))
           (move-robot (apply control-fn (list s1 s2 s3 s4 s5 s6 s7 s8)))))
        (else (error "There is no robot here to move"))))
    (define (take-n-steps n)
      (for-loop 0 n (lambda (i) (send this take-one-step))))
    (super-new)))

