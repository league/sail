(load "search-graph.scm")

(define square-puzzle%
  (class searchable%
    (override start goal? state-eq? successors-of)
    (public determine-size index-of swap move offset)
    (init-field (s 3))
    (define (determine-size)
      (cond
        ((integer? s) s)
        ((list? s)
         (let ((n (sqrt (length s))))
           (if (integer? n) n
               (error "Length of list must be perfect square"))))
        (else (error "Unexpected form for initializer"))))
    (define (make-solution i n)
      (if (= i n) '(x)
          (cons i (make-solution (+ i 1) n))))
    (define size (determine-size))
    (define solution (make-solution 1 (* size size)))
    (define (start) (if (list? s) s solution))
    (define (goal? l) (equal? l solution))
    (define (state-eq? l m) (equal? l m))
    (define (index-of tile l i)
      (if (eq? (car l) tile) i
          (index-of tile (cdr l) (+ i 1))))
    (define (swap tile ti hj l k)
      (if (null? l) null
          (cons (cond
                  ((eq? k ti) 'x)
                  ((eq? k hj) tile)
                  (else (car l)))
                (swap tile ti hj (cdr l) (+ k 1)))))
    (define (move ti hj l)
      (swap (list-ref l ti) ti hj l 0))
    (define (offset dir i)
      (case dir
        ('down (if (< i size) #f (- i size)))
        ('up (if (>= i (- (* size size) size)) #f (+ i size)))
        ('right (if (zero? (modulo i size)) #f (- i 1)))
        ('left (if (zero? (modulo (+ i 1) size)) #f (+ i 1)))))
    (define (successors-of l)
      (let* ((hj (index-of 'x l 0))
             (di (offset 'down hj))
             (da (if di (list (cons 'down (move di hj l))) null))
             (ui (offset 'up hj))
             (ua (if ui (cons (cons 'up (move ui hj l)) da) da))
             (ri (offset 'right hj))
             (ra (if ri (cons (cons 'right (move ri hj l)) ua) ua))
             (li (offset 'left hj))
             (la (if li (cons (cons 'left (move li hj l)) ra) ra)))
        la))
    (super-new)))

;(define 8p (new square-puzzle%))
(define 15p (new square-puzzle% (s 4)))

(define square-puz-model%
  (class model%
    (public at move show slide get-size)
    (init-field (s 3))
    (define sp (new square-puzzle% (s s)))
    (define st (send sp start))
    (define size (send sp determine-size))
    (define (get-size) size)
    (define (at i) (list-ref st i))
    (define (show) (printf "~v~n" st))
    (define (move dir)
      (let* ((hj (send sp index-of 'x st 0))
             (ti (send sp offset dir hj)))
        (when ti
          (set! st (send sp move ti hj st))
          (send this changed))))
    (define (slide ti)
      (let ((hj (send sp index-of 'x st 0)))
        (for-each
         (lambda (dir)
           (when (eq? ti (send sp offset dir hj))
             (set! st (send sp move ti hj st))
             (send this changed)))
         '(up down left right))))
    (super-new)))

(define puz-tile-view%
  (class canvas%
    (public update)
;    (override on-event)
    (init-field (m #f) (i 0))
    (define (update . args)
      (send this refresh))
    (define (paint-cell v dc)
      (let ((k (send m at i)))
        (cond
          ((eq? k 'x)
           (send dc set-brush "black" 'solid)
           (send dc draw-rectangle 0 0
                 (send v get-width)
                 (send v get-height))))))
    (super-new (paint-callback paint-cell)
               (min-width 32) (min-height 32)
               (style '(border)))
    (send m attach this)))

(define square-puz-view%
  (class vertical-pane%
    (public update)
    (init-field (m $f))
    (define size (send m get-size))
    (define buttons null)
    (define (update . args)
      (for-loop 
       0 (* size size)
       (lambda (i)
         (send (list-ref buttons i) set-label 
               (to-string (send m at i))))))
    (super-new)
    (double-loop*
     size size
     (lambda (i) (new horizontal-pane% (parent this)))
     (lambda (pane i j)
       (let* ((k (+ (* i size) j))
              (l (to-string (send m at k)))
              (g (lambda (b e) (send m slide k)))
              (b (new puz-tile-view% (parent pane)
                      (m m) (i k))))
         (set! buttons (cons b buttons)))))
    (set! buttons (reverse buttons))))
     
(define 8p (new square-puz-model%))
(define fr (new frame% (label "Puzzle")))
(define v (new square-puz-view% (m 8p) (parent fr)))
(send fr show #t)

