;;; grid-view.scm -- widgets that display grid worlds and inhabitants
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-09-12 20:08:07 league>

(load "grid-model.scm")

(define generic-cell-view%
  (class canvas%
    (super-new (min-width 32) (min-height 32) (style '(border)))))

(define *wall-color*
  (send the-color-database find-color "DarkRed"))

(define *robot-color*
  (send the-color-database find-color "DarkGoldenrod"))

(define wall-view%
  (class generic-cell-view%
    (super-new)
    (send this set-canvas-background *wall-color*)))

(define PI 3.14159)

(define cell-view%
  (class generic-cell-view%
    (public update)
    (override on-event)
    (init-field (room #f) (row #f) (col #f))
    (define cell (send room get-cell row col))
    (define (update . args)
      (send this refresh))
    (define (paint-cell cv dc)
      (cond
        ((send cell robot?)
         (send dc set-brush *robot-color* 'solid)
         (send dc draw-ellipse 4 4
               (- (send cv get-width) 9)
               (- (send cv get-height) 9)))
        ((send cell get-direction)
         (send dc set-brush "black" 'solid)
         (let* ((bfrac (case (send cell get-direction)
                        ('south (/ 3 8))
                        ('east (/ 7 8))
                        ('north (/ 11 8))
                        ('west (/ 15 8))))
                (efrac (+ bfrac (/ 1 4))))
           (send dc draw-arc 4 4 
                 (- (send cv get-width) 9)
                 (- (send cv get-height) 9)
                 (* bfrac PI) (* efrac PI))))))
    (define (on-event ev)
      (case (send ev get-event-type)
        ('left-down (send room place-robot row col))))
    (super-new (paint-callback paint-cell))
    (send cell attach this)))

(define room-view%
  (class vertical-pane%
    (init-field (room #f))
    (super-new)
    (double-loop*
     (send room get-nrows) (send room get-ncols)
     (lambda (i) (new horizontal-pane% (parent this)))
     (lambda (pane i j)
       (if (send room get-cell i j)
           (new cell-view% (parent pane) (room room) (row i) (col j))
           (new wall-view% (parent pane)))))))

(define grid-world-frame%
  (class frame%
    (public update)
    (init-field (room #f))
    (super-new (label "Grid world"))
    (new room-view% (parent this) (room room))
    (define toolbar (new horizontal-pane% (parent this)))
    (define clear (new button% (label "Clear") (parent toolbar) 
                       (callback (lambda (b e) (send room clear)))))
    (define step (new button% (label "Step") (parent toolbar) (enabled #f)
                      (callback (lambda (b e) (send room take-one-step)))))
    (define run (new button% (label "10 steps") (parent toolbar) (enabled #f)
                     (callback (lambda (b e) (send room take-n-steps 10)))))
;    (define editor (new editor-canvas% (parent this) (min-height 100)))
;    (define text (new text%))
    (define (update . args)
      (let ((r (send room robot?)))
        (send step enable r)
        (send run enable r)))
;    (send editor set-editor text)
;    (send text insert (to-string (get-field controller room)))
    (send room attach this)))

(define (open-world tmpl)
  (let* ((r (new room-model% (template tmpl)))
         (f (new grid-world-frame% (room r))))
    (send f show #t)
    r))

(define (change-robot r ctrl)
  (send r set-controller ctrl))

