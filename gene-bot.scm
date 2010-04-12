;;; gene-bot.scm -- a genetic algorithm to build a wall-following robot
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-09-26 12:36:15 league>

(load "gene-tree.scm")
(load "grid-view.scm")

(define room-0
  '("        "
    "        "
    "        "
    "        "))

(define genetic-robot-algo%
  (class genetic-tree-algo%
    (override create-leaf create-branch evaluate)
    (init-field (template room-0))
    (define room (new room-model% (template template)))
    (define (choose-from v)
      (vector-ref v (random (vector-length v))))
    (define (create-leaf)
      (choose-from #(s1 s2 s3 s4 s5 s6 s7 s8 north south east west)))
    (define (create-branch h)
      (case (random 4)
        ('0 (list 'if 
                  (send this create-tree h) 
                  (send this create-tree h)
                  (send this create-tree h)))
        ('1 (list 'and 
                  (send this create-tree h)
                  (send this create-tree h)))
        ('2 (list 'or 
                  (send this create-tree h) 
                  (send this create-tree h)))
        ('3 (list 'not (send this create-tree h)))))
    (define (evaluate tr)
      (send room set-controller tr)
      (- (send room measure-fitness 8 60)
         (round (log2 (tree-size tr)))))
    (super-new)))

(define ga-robot-view%
  (class ga-text-view%
    (override update)
    (init-field (template room-0)
                (room (new room-model% (template template))))
    (define frame (new grid-world-frame% (room room)))
    (define (update . args)
      (super update . args)
      (when (and (pair? args) (eq? (car args) 'finish))
        (let ((bot (car (send (get-field ga this) best))))
          (send room set-controller bot)
          (send room clear)
          (printf "installing best bot:~n  ~v~n" bot))))
    (super-new (ga (new genetic-robot-algo% (template template))))
    (send frame show #t)))

(define (new-robot-ga room)
  (let ((gv (new ga-robot-view% (template room))))
    (get-field ga gv)))

(define ga (new-robot-ga room-0))

