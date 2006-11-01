(load "stdlib.scm")
(load "sets.scm")

(define world-0
  '((door tall-gate gate-room east ante-chamber)
    (door wooden-door ante-chamber east treasure-room)
    (path gate-room south broom-closet)
    (key brass-key gate-room wooden-door)
    (treasure bag-of-gold-coins treasure-room)
    (start gate-room)
    (end-with bag-of-gold-coins)
    (end-in gate-room)))
  
(define (opposite-direction d)
  (case d
    ('north 'south)
    ('south 'north)
    ('east 'west)
    ('west 'east)))

(define (opposite-doorway rule)  ; (door D1 R1 EAST R2)
  (list 'door (cadr rule) 
        (list-ref rule 4)
        (opposite-direction (cadddr rule))
        (list-ref rule 2)))

(define (opposite-path rule) ; (path R1 SOUTH R2)
  (list 'path 
        (cadddr rule)
        (opposite-direction (caddr rule))
        (cadr rule)))

(define (expand-world w)
  (if (null? w) null
      (let ((z (expand-world (cdr w))))
        (case (caar w)
          ('door (cons (car w) (cons (opposite-doorway (car w)) z)))
          ('path (cons (car w) (cons (opposite-path (car w)) z)))
          (else (cons (car w) z))))))

(define (rule->state r)
  (case (car r)
    ('start (list 'room (cadr r)))
    ('door (list 'door (cadr r) 'closed))
    ('key (list 'object (cadr r) (caddr r)))
    ('treasure (list 'object (cadr r) (caddr r)))
    ('torch (list 'object (cadr r) (caddr r)))
    (else #f)))

(define (world->state w)
  (if (null? w) null
      (let ((z (world->state (cdr w)))
            (r (rule->state (car w))))
        (if r (cons r z) z))))

(define *current-world* #f)
(define *current-state* #f)

(define (lookup-door di)
  (findf (lambda (x) (and (eq? (car x) 'door)
                          (eq? (cadr x) di)))
         *current-state*))

(define (lock-door di)
  (set-cdr! (lookup-door di) (list di 'locked)))

(define (lock-all-doors)
  (for-each
   (lambda (r) 
     (when (eq? (car r) 'key)
       (lock-door (cadddr r))))
   *current-world*))

(define (start w)
  (set! *current-world* (expand-world w))
  (set! *current-state* 
        (cons (list 'inventory) (world->state w)))
  (lock-all-doors)
  (look))

(define (look)
  (let ((r (cadr (assq 'room *current-state*))))
    (printf "You are in ~v.~n" r)
    (for-each
     (lambda (x)
       (when (and (eq? (car x) 'door) (eq? (caddr x) r))
         (printf "There is a ~v to the ~v.~n" (cadr x) (cadddr x)))
       (when (and (eq? (car x) 'path) (eq? (cadr x) r))
         (printf "There is a path to the ~v.~n" (caddr x))))
     *current-world*)
    (for-each
     (lambda (x)
       (when (and (eq? (car x) 'object) (eq? (caddr x) r))
         (printf "A ~v is here.~n" (cadr x))))
     *current-state*)
    (check)))

(define (inventory)
  (let ((i (cdr (assq 'inventory *current-state*))))
    (cond
      ((null? i)
       (printf "You are empty-handed.~n"))
      (else
       (printf "You are carrying:~n")
       (for-each (lambda (x) (printf "  ~v~n" x)) i)))))

(define (lookup-obj thing)
  (findf (lambda (x) (and (eq? (car x) 'object)
                          (eq? (cadr x) thing)))
         *current-state*))

(define (lookup-obj-in-room thing room)
  (findf (lambda (x) (and (eq? (car x) 'object)
                          (eq? (cadr x) thing)
                          (eq? (caddr x) room)))
         *current-state*))

(define (take thing)
  (let* ((rm (cadr (assq 'room *current-state*)))
         (ok (lookup-obj-in-room thing rm))
         (inv (assq 'inventory *current-state*)))
    (cond
      ((not ok)
       (printf "There is no ~v here.~n" thing))
      (else
       (set-cdr! ok (list thing #f))
       (set-cdr! inv (cons thing (cdr inv)))
       (printf "Taken.~n")))))

(define (drop thing)
  (let* ((inv (assq 'inventory *current-state*))
         (rm (cadr (assq 'room *current-state*)))
         (b (lookup-obj thing)))
    (cond
      ((element-of? thing (cdr inv))
       (set-cdr! inv (remq thing (cdr inv)))
       (set-cdr! b (list thing rm))
       (printf "Dropped.~n"))
      (else
       (printf "You aren't carrying that.~n")))))

(define (south) (go 'south))
(define (north) (go 'north))
(define (east) (go 'east))
(define (west) (go 'west))

(define (lookup-path-from room dir)
  (findf (lambda (x) 
           (and (eq? (car x) 'path)
                (eq? (cadr x) room)
                (eq? (caddr x) dir)))
         *current-world*))

(define (lookup-door-from room dir)
  (findf (lambda (x)
           (and (eq? (car x) 'door)
                (eq? (caddr x) room)
                (eq? (cadddr x) dir)))
         *current-world*))           

(define (go dir)
  (let* ((rm (assq 'room *current-state*))
         (p (lookup-path-from (cadr rm) dir))
         (d (lookup-door-from (cadr rm) dir)))
    (cond
      (p 
       (set-cdr! rm (list (cadddr p)))
       (look))
      (d (go-door rm d))
      (else (printf "There is no path to the ~v.~n" dir)))))

(define (lookup-door-state d)
  (findf (lambda (x) (and (eq? (car x) 'door)
                          (eq? (cadr x) d)))
         *current-state*))
                               
(define (go-door rm d)
  (let ((c (lookup-door-state (cadr d))))
    (cond
      ((eq? (caddr c) 'open)
       (set-cdr! rm (list (list-ref d 4)))
       (look))
      (else
       (printf "Sorry, the ~v is ~v.~n" (cadr d) (caddr c))))))

(define (lookup-door-in d room)
  (findf (lambda (x)
           (and (eq? (car x) 'door)
                (eq? (cadr x) d)
                (eq? (caddr x) room)))
         *current-world*))

(define (lookup-door-state-in d room)
  (let ((ok (lookup-door-in d room)))
    (if ok (lookup-door-state d) #f)))

(define (open d)
  (let* ((rm (cadr (assq 'room *current-state*)))
         (b (lookup-door-state-in d rm)))
    (cond
      ((not b)
       (printf "There is no ~v here.~n" d))
      ((eq? (caddr b) 'locked)
       (printf "Sorry, the ~v is locked.~n" d))
      ((eq? (caddr b) 'open)
       (printf "The ~v is already open.~n" d))
      (else
       (set-cdr! b (list d 'open))
       (printf "Ok.~n")))))

(define (lookup-key-for d)
  (findf (lambda (x) (and (eq? (car x) 'key)
                          (eq? (cadddr x) d)))
         *current-world*))

(define (unlock d)
  (let* ((rm (cadr (assq 'room *current-state*)))
         (inv (cdr (assq 'inventory *current-state*)))
         (b (lookup-door-state-in d rm))
         (k (cadr (lookup-key-for d))))
    (cond
      ((not b)
       (printf "There is no ~v here.~n" d))
      ((eq? (caddr b) 'closed)
       (printf "The ~v is not locked; did you try opening it?~n" d))
      ((eq? (caddr b) 'open)
       (printf "The ~v is already open.~n" d))
      ((element-of? k inv)
       (set-cdr! b (list d 'closed))
       (printf "Ok.~n"))
      (else
       (printf "Sorry, you don't have the key to unlock the ~v.~n" d)))))

(define (check)
  (let* ((cur-rm (cadr (assq 'room *current-state*)))
         (goal-rm (cadr (assq 'end-in *current-world*)))
         (cur-inv (cdr (assq 'inventory *current-state*)))
         (goal-inv (cdr (assq 'end-with *current-world*))))
    (when (and (eq? cur-rm goal-rm)
               (subset? goal-inv cur-inv))
      (printf "Congratulations, you have completed the task!~n"))))
