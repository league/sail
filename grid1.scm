(define model%
  (class object%
    (public subscribe notify)
    (define listeners null)
    (define (subscribe aView)
      (set! listeners (cons aView listeners))
      (send aView update this 'initial))
    (define (notify . args)
      (for-each (lambda (i) (send i update this . args)) listeners))
    (super-new)))

(define cell-model%
  (class model%
    (public wall?)
    (init-field (kind 1))
    (define wallp (= kind 1))
    (define (wall?) wallp)
    (super-new)))

(define *grid-template*
  '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
    (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    (1 0 0 1 1 1 1 1 1 0 0 0 0 0 0 1)
    (1 0 0 1 1 0 0 1 1 0 0 0 0 0 0 1)
    (1 0 0 1 1 0 0 1 1 0 0 0 0 0 0 1)
    (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    (1 0 0 0 0 0 1 1 1 0 0 0 1 1 1 1)
    (1 0 0 0 0 0 1 1 1 0 0 0 1 1 1 1)
    (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

(define (list2d-map f m)
  (map (lambda (v) (map f v)) m))

(define (list2d-for-each f m)
  (for-each (lambda (v) (for-each f v)) m))

(define grid-model%
  (class model%
    (init-field (template *grid-template*))
    (field (cells (list2d-map (lambda (i) (new cell-model% (kind i))) template)))
    (super-new)))

(define *wall-color* (send the-color-database find-color "DarkRed"))

(define cell-view%
  (class canvas%
    (public update)
    (define (update cm . args)
      (when (send cm wall?)
        (send this set-canvas-background *wall-color*)))
    (super-new (min-width 32) (min-height 32) (style '(border)))))

(define grid-view%
  (class vertical-panel%
    (public update)
    (define (update gm . args)
      (for-each 
       (lambda (v) 
         (let ((row (new horizontal-panel% (parent this))))
           (for-each 
            (lambda (m) (send m subscribe (new cell-view% (parent row))))
            v)))
       (get-field cells gm)))                
    (super-new)))

;        1         2         3         4         5         6         7
;234567890123456789012345678901234567890123456789012345678901234567890123456
(define mm (new grid-model%))
(define fr (instantiate frame% ("Grid world")))
(define gv (new grid-view% (parent fr)))
(send mm subscribe gv)
(send fr show #t)

