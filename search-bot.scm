;;; search-bot.scm -- a room-exploring robot with planning
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-10-10 09:50:54 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "grid-view.scm")
(load "search-graph.scm")

;; This robot has state -- it keeps a map of everywhere it has been.
;; By default it sweeps up and down the room, one column at a time,
;; but when it gets stuck, it searches for a path to some location
;; it has not yet visited, and executes the path.

(define (new-state)
  (list (cons 'current (cons 0 0))
        (cons 'visited (list (cons 0 0)))
        (cons 'to-visit empty-set)
        (cons 'walls empty-set)
        (cons 'plan empty-set)))

(define (addq! binding item)
  (set-cdr! binding (add-to-set item (cdr binding))))

(define (rmq! binding item)
  (set-cdr! binding (remove-from-set item (cdr binding))))
         
;; Record the locations of walls and free space, as indicated by sensors.

(define (update-map sensors state)
  (let ((w (assq 'walls state))
        (t (assq 'to-visit state))
        (v (cdr (assq 'visited state)))
        (c (cdr (assq 'current state))))
    (let ((frob 
           (lambda (s dr dc)
             (let ((p (cons (+ (car c) dr) (+ (cdr c) dc))))
               (cond
                 (s (addq! w p))          ; it's a wall
                 ((not (element-of? p v))  ; not visited yet
                  (addq! t p)))))))
      (frob (list-ref sensors 0) -1 -1)   ; NW
      (frob (list-ref sensors 1) -1  0)   ; N
      (frob (list-ref sensors 2) -1 +1)   ; NE
      (frob (list-ref sensors 3)  0 +1)   ;  E
      (frob (list-ref sensors 4) +1 +1)   ; SE
      (frob (list-ref sensors 5) +1  0)   ; S
      (frob (list-ref sensors 6) +1 -1)   ; SW
      (frob (list-ref sensors 7)  0 -1)   ;  W
      )))

;; Update the map after moving in given direction.

(define (record-movement dir state)
  (let* ((b (assq 'current state))
         (r (cadr b))
         (c (cddr b)))
    (case dir
      ('north (set-cdr! b (cons (- r 1) c)))
      ('south (set-cdr! b (cons (+ r 1) c)))
      ('east  (set-cdr! b (cons r (+ c 1))))
      ('west  (set-cdr! b (cons r (- c 1)))))
    (addq! (assq 'visited state) (cdr b))
    (rmq! (assq 'to-visit state) (cdr b))))

;; Here is the planning engine.
(define visitation-plan%
  (class searchable%
    (override start goal? state-eq? successors-of return)
    (init-field (state #f))
    (define (start) (cdr (assq 'current state)))
    (define (goal? p) (element-of? p (cdr (assq 'to-visit state))))
    (define (state-eq? p q) (equal? p q))
    (define (successors-of p)
      (let ((r (car p))
            (c (cdr p))
            (w (cdr (assq 'walls state))))
        (filter
         (lambda (arc) (not (element-of? (cdr arc) w)))
         (list (cons 'north (cons (- r 1) c))
               (cons 'south (cons (+ r 1) c))
               (cons 'east (cons r (+ c 1)))
               (cons 'west (cons r (- c 1)))))))
    (define (return path)
      (map car (cdr path)))
    (super-new)))

(define *global-state* #f)

(define (bot-fn sensors state)
  (unless state (set! state (new-state)))
  (update-map sensors state)
  (let* ((p (assq 'plan state))
         (t (cdr (assq 'to-visit state)))
         (q (cdr (assq 'current state)))
         (r (car q))  ; current row
         (c (cdr q))  ; current column
         (dir #f))
    (cond
      ;; Is there currently a plan? If so, execute that.
      ((not (null? (cdr p)))
       (set! dir (cadr p))
       (set-cdr! p (cddr p)))
      ;; Try S,N,E,W, as long as they're scheduled for visits.
      ((element-of? (cons (+ r 1) c) t) (set! dir 'south))
      ((element-of? (cons (- r 1) c) t) (set! dir 'north))
      ((element-of? (cons r (+ c 1)) t) (set! dir 'east))
      ((element-of? (cons r (- c 1)) t) (set! dir 'west))
      ;; Done: visited entire map!
      ((null? t) (printf "Done!~n"))
      ;; Stuck: now we have to make a plan to find nearest unvisited cell.
      (else
       (printf "Planning... ")
       (let* ((vp (new visitation-plan% (state state)))
              (seq (send vp search)))
         (printf "~v~n" seq)
         (set! dir (car seq))
         (set-cdr! p (cdr seq)))))
    (when dir (record-movement dir state))
    (cons dir state)))

(define room-3
  '("          xx  "
    "xxxxx xxx    x"
    "         xxxxx"
    "          xx  "
    "xxxxx xxx    x"
    "    x    xxxxx"
    "    x     xx  "
    "xx xx xxx    x"
    " x  x     xxxx"
    "      x x     "))

(define w (open-world room-3))
(send w set-fn bot-fn)
