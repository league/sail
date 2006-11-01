(load "search-graph.scm")

(define search-implicit-graph%
  (class searchable%
    (override start goal? state-eq? successors-of)
    (init-field (from #f) (goal #t) (actions null))
    (define (start) from)
    (define (goal? s) (match-precondition s goal))
    (define (state-eq? s t) (equal? s t))
    (define (successors-of s)
      (foldr (consider-action s) null actions))
    (define (variable? sym)
      (eq? #\? (string-ref (symbol->string sym) 0)))
    (define (match-precondition state pre)
      (cond
        ((and (null? state) (null? pre)) null)
        ((and (pair? state) (pair? pre))
         (let ((rest (match-precondition (cdr state) (cdr pre))))
           (cond
             ((not rest) #f)
             ((eq? (car state) (car pre)) rest)
             ((variable? (car pre)) 
              (let ((b (assq (car pre) rest)))
                (if (or (not b) (eq? (cdr b) (car state)))
                    (cons (cons (car pre) (car state)) rest)
                    #f)))
             (else #f))))
        (else #f)))
    (define (substitute bindings post)
      (if (null? post) null
          (let ((rest (substitute bindings (cdr post))))
            (if (variable? (car post))
                (let ((b (assq (car post) bindings)))
                  (if b (cons (cdr b) rest)
                      (error "Unbound variable" (car post))))
                (cons (car post) rest)))))
    (define (consider-action state)
      (lambda (xform list)
        (let ((b (match-precondition state (cadr xform))))
          (if b (cons (cons (car xform) (substitute b (caddr xform))) list)
              list))))
    (super-new)))

;;;;;;;;;;;;;;;;;; START HERE

;; Here are the parts you may want to change: start, goal, and actions.
;; You can use these to represent many kinds of problems, not just the
;; dungeon/treasure sort of thing shown here.

;; In this example, we have an adventurer, a key, a treasure, and 4 rooms
;; with 3 doors connecting them.  The state is represented as 6 components:
;;    1. location of the adventurer, 
;;    2. location of key (or "A" if agent holds it),
;;    3. location of gold (or "A" if agent holds it),
;;    4. status of door 1 (C for closed, O for open, L for locked)
;;    5. status of door 2
;;    6. status of door 3

(define start '(R1 R1 R4 C L C))

;; Here we define what the goal state looks like.  Use a variable (begins 
;; with a question mark) if you don't care about that variable.  Here, we
;; want the adventurer to hold the treasure, but nothing else matters.

(define goal '(?x ?y A ?d1 ?d2 ?d3))

;; Here we define the actions as state transformers.  Each action provides
;; a pattern for the "before" state (precondition) and a pattern for after
;; (postcondition).  If a variable name appears more than once in a pre-
;; condition, it means that those values must be the same.

;; Let's look at an example:
;;    ("unlock D2" (R2 A ?z ?d1 L ?d3) (R2 A ?z ?d1 C ?d3))
;;    ("unlock D2" (R3 A ?z ?d1 L ?d3) (R3 A ?z ?d1 C ?d3))
;; There are two ways to unlock door D2.  The condition (R2 A ?z ?d1 L ?d3)
;; means that the adventurer is in room R2, he holds the key, and door 2 is 
;; locked.  The status of the gold (?z), and doors 1,3 don't matter.  In
;; this case, unlocking the door puts us in the state (R2 A ?z ?d1 C ?d3).
;; That is, all that changed is that the status of door changed from Locked 
;; to Closed.  The second rule says that the same thing can happen from 
;; room R3 (since door D2 connects rooms R2 and R3).

(define actions
  '(("go east" (R1 ?y ?z O ?d2 ?d3) (R2 ?y ?z O ?d2 ?d3))
    ("go west" (R2 ?y ?z O ?d2 ?d3) (R1 ?y ?z O ?d2 ?d3))
    ("go south" (R2 ?y ?z ?d1 O ?d3) (R3 ?y ?z ?d1 O ?d3))
    ("go south" (R3 ?y ?z ?d1 ?d2 O) (R4 ?y ?z ?d1 ?d2 O))
    ("go north" (R3 ?y ?z ?d1 O ?d3) (R2 ?y ?z ?d1 O ?d3))
    ("go north" (R4 ?y ?z ?d1 ?d2 O) (R3 ?y ?z ?d1 ?d2 O))    
    ("take key" (?x ?x ?z ?d1 ?d2 ?d3) (?x A ?z ?d1 ?d2 ?d3))
    ("drop key" (?x A ?z ?d1 ?d2 ?d3) (?x ?x ?z ?d1 ?d2 ?d3))
    ("take gold" (?x ?y ?x ?d1 ?d2 ?d3) (?x ?y A ?d1 ?d2 ?d3))
    ("drop gold" (?x ?y A ?d1 ?d2 ?d3) (?x ?y ?x ?d1 ?d2 ?d3))
    ("open D1" (R1 ?y ?z C ?d2 ?d3) (R1 ?y ?z O ?d2 ?d3))
    ("open D1" (R2 ?y ?z C ?d2 ?d3) (R2 ?y ?z O ?d2 ?d3))
    ("close D1" (R1 ?y ?z O ?d2 ?d3) (R1 ?y ?z C ?d2 ?d3))
    ("close D1" (R2 ?y ?z O ?d2 ?d3) (R2 ?y ?z C ?d2 ?d3))
    ("unlock D2" (R2 A ?z ?d1 L ?d3) (R2 A ?z ?d1 C ?d3))
    ("unlock D2" (R3 A ?z ?d1 L ?d3) (R3 A ?z ?d1 C ?d3))
    ("lock D2" (R2 A ?z ?d1 C ?d3) (R2 A ?z ?d1 L ?d3))
    ("lock D2" (R3 A ?z ?d1 C ?d3) (R3 A ?z ?d1 L ?d3))
    ("open D2" (R2 ?y ?z ?d1 C ?d3) (R2 ?y ?z ?d1 O ?d3))
    ("open D2" (R3 ?y ?z ?d1 C ?d3) (R3 ?y ?z ?d1 O ?d3))
    ("close D2" (R2 ?y ?z ?d1 O ?d3) (R2 ?y ?z ?d1 C ?d3))
    ("close D2" (R3 ?y ?z ?d1 O ?d3) (R3 ?y ?z ?d1 C ?d3))
    ("open D3" (R3 ?y ?z ?d1 ?d2 C) (R3 ?y ?z ?d1 ?d2 O))
    ("open D3" (R4 ?y ?z ?d1 ?d2 C) (R4 ?y ?z ?d1 ?d2 O))
    ("close D3" (R3 ?y ?z ?d1 ?d2 O) (R3 ?y ?z ?d1 ?d2 C))
    ("close D3" (R4 ?y ?z ?d1 ?d2 O) (R4 ?y ?z ?d1 ?d2 C))
    ))

;; Set it up as a search problem.
(define dungeon 
  (new search-implicit-graph% 
       (from start) (goal goal) (actions actions)))

;; This runs the search and outputs the resulting path through the graph.
(send dungeon search)


