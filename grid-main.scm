(load "grid-view.scm")

(define w (open-world room-2))

;;;;; this is the solution
(define my-robot
  '(if (and (or s2 s3) (not (or s4 s5))) 'east
       (if (and (or s4 s5) (not (or s6 s7))) 'south
           (if (and (or s6 s7) (not (or s8 s1))) 'west
               (if (and (or s8 s1) (not (or s2 s3))) 'north
                   'north)))))

(change-robot w my-robot)
