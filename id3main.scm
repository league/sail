(load "grid-view.scm")
(load "id3tree.scm")

(define examples
  '(('south #T #T #T #T #T #F #F #F)
    ('south #F #F #T #T #T #F #F #F)
    ('south #F #F #T #T #F #F #F #F)
    ('south #F #F #F #F #T #F #F #F)
    ('south #F #F #F #T #F #F #F #F)
    
    ('east  #F #F #T #F #F #F #F #F)
    ('east  #F #T #T #F #F #F #F #F)
    ('east  #T #T #T #F #F #F #F #F)
    ('east  #T #T #T #F #F #F #T #T)
    ('east  #T #T #F #F #F #F #T #T)
    
    ('west  #F #F #T #T #T #T #T #F)
    ('west  #F #F #F #F #T #T #T #F)
    ('west  #F #F #F #F #T #T #F #F)
    ('west  #F #F #F #F #F #F #T #F)
    
    ('north #T #F #F #F #T #T #T #T)
    ('north #T #F #F #F #F #F #T #T)
    ('north #F #F #F #F #F #F #T #T)
    ('north #T #F #F #F #F #F #F #F)
    ))

(define complete-examples
  '(
    ('south #T #T #T #T #F #F #F #F) ;1234
    ('south #T #T #T #T #T #F #F #F) ;12345
    ('south #F #T #T #T #F #F #F #F) ;234
    ('south #F #T #T #T #T #F #F #F) ;2345
    ('south #F #F #T #T #F #F #F #F) ;34
    ('south #F #F #T #T #T #F #F #F) ;345
    ('south #F #F #F #T #F #F #F #F) ;4
    ('south #F #F #F #T #T #F #F #F) ;45
    ('south #F #F #F #F #T #F #F #F) ;5

    ('east  #T #T #F #F #F #F #F #F) ;12
    ('east  #T #T #T #F #F #F #F #F) ;123
    ('east  #F #T #F #F #F #F #F #F) ;2
    ('east  #F #T #T #F #F #F #F #F) ;23
    ('east  #F #F #T #F #F #F #F #F) ;3
    ('east  #T #T #F #F #F #F #T #T) ;7812
    ('east  #T #T #T #F #F #F #T #T) ;78123
    ('east  #T #T #F #F #F #F #F #T) ;812
    ('east  #T #T #T #F #F #F #F #T) ;8123

    ('west  #F #F #T #T #T #T #F #F) ;3456
    ('west  #F #F #T #T #T #T #T #F) ;34567
    ('west  #F #F #F #T #T #T #F #F) ;456
    ('west  #F #F #F #T #T #T #T #F) ;4567
    ('west  #F #F #F #F #T #T #F #F) ;56
    ('west  #F #F #F #F #T #T #T #F) ;567
    ('west  #F #F #F #F #F #T #F #F) ;6
    ('west  #F #F #F #F #F #T #T #F) ;67
    ('west  #F #F #F #F #F #F #T #F) ;7
    
    ('north #T #F #F #F #F #F #F #F) ;1
    ('north #F #F #F #F #T #T #T #T) ;5678
    ('north #T #F #F #F #T #T #T #T) ;56781
    ('north #F #F #F #F #F #T #T #T) ;678
    ('north #T #F #F #F #F #T #T #T) ;6781
    ('north #F #F #F #F #F #F #T #T) ;78
    ('north #T #F #F #F #F #F #T #T) ;781
    ('north #F #F #F #F #F #F #F #T) ;8
    ('north #T #F #F #F #F #F #F #T) ;81
    ))

(define sensors
  '((s1 . 1) (s2 . 2) (s3 . 3) (s4 . 4)
    (s5 . 5) (s6 . 6) (s7 . 7) (s8 . 8)))

(define my-robot-1 (build-tree examples sensors))
(define fn1 (prep-controller my-robot-1))

;; the above seems to work for room-1, room-2, but let's see if we 
;; can get a complete one:
(define eg2
  '(('east  #F #T #F #F #F #F #F #F)
    ('east  #F #F #T #F #F #F #F #F)
    ('south #F #F #F #T #F #F #F #F)
    ('south #F #F #F #F #T #F #F #F)
    ('west  #F #F #F #F #F #T #F #F)
    ('west  #F #F #F #F #F #F #T #F)
    ('north #F #F #F #F #F #F #F #T)
    ('north #T #F #F #F #F #F #F #F)
    ))

;xxx  
;xo
;x

(define my-robot-2 (build-tree eg2 sensors))
(define fn2 (prep-controller my-robot-2))

(define w (open-world room-2))
(change-robot w my-robot-1)
(define my-robot
  '(if (and (or s2 s3) (not (or s4 s5))) 'east
       (if (and (or s4 s5) (not (or s6 s7))) 'south
           (if (and (or s6 s7) (not (or s8 s1))) 'west
               (if (and (or s8 s1) (not (or s2 s3))) 'north
                   'north)))))
;(change-robot w my-robot)