(load "grid-view.scm")

(define w (open-world room-1))
(define my-robot '(if s6 'north 'south))
(change-robot w my-robot)
