(define *grid-template*
  #("xxxxxxxxxxxxxxxx"
    "x           xxxx"
    "x           xxxx"
    "x           xxxx"
    "x           xxxx"
    "x  xxxxxx      x"
    "x  xx  xx      x"
    "x  xx  xx      x"
    "x           xxxx"
    "x           xxxx"
    "x     xxx   xxxx"
    "x     xxx   xxxx"
    "xxxxxxxxxxxxxxxx"
    
(define *grid-world*
  #(#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 1 1 1 1 1 1 0 0 0 0 0 0 1)
    #(1 0 0 1 1 0 0 1 1 0 0 0 0 0 0 1)
    #(1 0 0 1 1 0 0 1 1 0 0 0 0 0 0 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 1 1 1 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 1 1 1 0 0 0 1 1 1 1)
    #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

(define *wall-thickness* 32)
(define *wall-color* (send the-color-database find-color "DarkRed"))

(define (new-location p)
  (new canvas% (parent p) (style '(border))
       (min-width *wall-thickness*)
       (min-height *wall-thickness*)))

(define (new-wall p)
  (let ((w (new-location p)))
    (send w set-canvas-background *wall-color*)
    w))

(define frame (new frame% (label "Grid World")))
(define row0 (new horizontal-panel% (parent frame)))
(define row1 (new horizontal-panel% (parent frame)))
(define row2 (new horizontal-panel% (parent frame)))
(define but00 (new-wall row0))
(define but01 (new-wall row0))
(define but02 (new-wall row0))
(define but10 (new-wall row1))
(define but11 (new-location row1))
(define but12 (new-wall row1))
(define but20 (new-wall row2))
(define but21 (new-wall row2))
(define but22 (new-location row2))
(send frame show #t)
