(load "gene-prog.scm")
(load "grid-view.scm")

(define room-0
  '("     "
    "     "
    "     "
    "     "))

(define the-room room-1)

(define vis-room (open-world the-room))
(define bot-room (new room-model% (template the-room)))

(define bot-leaves
  '(s1 s2 s3 s4 s5 s6 s7 s8 north south east west))

(define bot-ops
  '((if . 3) (and . 2) (or . 2) (not . 1)))

(define (interpret-tree tr sensors ret cont)
  (cond
    ((pair? tr)
     (case (car tr)
       ('not (interpret-tree 
              (cadr tr) sensors ret 
              (lambda (x) (cont (not x)))))
       ('and (interpret-tree
              (cadr tr) sensors ret
              (lambda (x1)
                (if x1 
                    (interpret-tree (caddr tr) sensors ret cont)
                    (cont #f)))))
       ('or (interpret-tree
             (cadr tr) sensors ret
             (lambda (x1)
               (if x1
                   (cont #t)
                   (interpret-tree (caddr tr) sensors ret cont)))))
       ('if (interpret-tree
             (cadr tr) sensors ret
             (lambda (x1)
               (if x1
                   (interpret-tree (caddr tr) sensors ret cont)
                   (interpret-tree (cadddr tr) sensors ret cont)))))))
    (else
     (case tr
       ('s1 (cont (list-ref sensors 0)))
       ('s2 (cont (list-ref sensors 1)))
       ('s3 (cont (list-ref sensors 2)))
       ('s4 (cont (list-ref sensors 3)))
       ('s5 (cont (list-ref sensors 4)))
       ('s6 (cont (list-ref sensors 5)))
       ('s7 (cont (list-ref sensors 6)))
       ('s8 (cont (list-ref sensors 7)))
       ('north (ret 'north))
       ('south (ret 'south))
       ('east (ret 'east))
       ('west (ret 'west))))))

(define (bot-make-fn tree)
  (lambda (s1 s2 s3 s4 s5 s6 s7 s8)
    (interpret-tree tree (list s1 s2 s3 s4 s5 s6 s7 s8) I I)))

;(define (bot-fix-controller tr)
;  (cond
;    ((pair? tr) (map bot-fix-controller tr))
;    ((eqv? tr 'north) ''north)
;    ((eqv? tr 'south) ''south)
;    ((eqv? tr 'east) ''east)
;    ((eqv? tr 'west) ''west)
;    (else tr)))

(define (bot-fitness ctrl)
  (send bot-room set-fn (bot-make-fn ctrl))
  (send bot-room measure-fitness 10 60))

(define (install-best-bot)
  (let ((ctrl (car (best-of CURRENT_POP))))
    (send vis-room set-fn (bot-make-fn ctrl))
    ctrl))

(define bot-gp 
  (make-genetic-program bot-leaves bot-ops bot-fitness))

(define (start)
  (genesis bot-gp)
  (install-best-bot))

(define (next k)
  (run-gens bot-gp k)
  (install-best-bot))

;(start)
