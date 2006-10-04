(load "grid-view.scm")

(define room-0
  '("        "
    "        "
    "        "
    "        "
    "        "
    "        "))

(define w (open-world room-2))

(define (exists? goal list)
  (findf (lambda (x) (equal? x goal)) list))

(define (addq binding pair)
  (unless (exists? pair (cdr binding))
    (set-cdr! binding (cons pair (cdr binding)))))
         
(define (new-state)
  (list (cons 'current (cons 0 0))
        (cons 'visited (list (cons 0 0)))
        (cons 'walls '())))

(define (addq-rel binding pair dr dc)
  (addq binding (cons (+ (car pair) dr) (+ (cdr pair) dc))))

(define (remember-walls sensors state)
  (let ((w (assq 'walls state))
        (c (cdr (assq 'current state))))
    (when (list-ref sensors 0) (addq-rel w c -1 -1))
    (when (list-ref sensors 1) (addq-rel w c -1  0))
    (when (list-ref sensors 2) (addq-rel w c -1 +1))
    (when (list-ref sensors 3) (addq-rel w c  0 +1))
    (when (list-ref sensors 4) (addq-rel w c +1 +1))
    (when (list-ref sensors 5) (addq-rel w c +1  0))
    (when (list-ref sensors 6) (addq-rel w c +1 -1))
    (when (list-ref sensors 7) (addq-rel w c  0 -1))))

(define (lookup-visits state)
  (let* ((v (cdr (assq 'visited state)))
         (p (cdr (assq 'current state)))
         (r (car p))
         (c (cdr p)))
    (list (exists? (cons (- r 1) (- c 1)) v)  ; NW
          (exists? (cons (- r 1) c) v)        ; N
          (exists? (cons (- r 1) (+ c 1)) v)  ; NE
          (exists? (cons r (+ c 1)) v)        ; E
          (exists? (cons (+ r 1) (+ c 1)) v)  ; SE
          (exists? (cons (+ r 1) c) v)        ; S
          (exists? (cons (+ r 1) (- c 1)) v)  ; SW
          (exists? (cons r (- c 1)) v)        ; W
          )))

(define (record-movement dir state)
  (let* ((b (assq 'current state))
         (r (cadr b))
         (c (cddr b)))
    (addq (assq 'visited state) (cdr b))
    (case dir
      ('north (set-cdr! b (cons (- r 1) c)))
      ('south (set-cdr! b (cons (+ r 1) c)))
      ('east  (set-cdr! b (cons r (+ c 1))))
      ('west  (set-cdr! b (cons r (- c 1)))))))

(define (bot-control s1 s2 s3 s4 s5 s6 s7 s8
                     v1 v2 v3 v4 v5 v6 v7 v8)
  (let ((wn (or s2 s3))  ; wall to the north?
        (we (or s4 s5))  ; wall to the east?
        (ws (or s6 s7))
        (ww (or s8 s1))
        (vn (or v1 v2))  ; already visited to the north?
        (vw (or v7 v8))  ; already visited to the west?
        (vs (or v5 v6))
        (ve (or v3 v4)))
    (cond
      ((and vn (not vw) (not s8)) 'west)
      ((and vw (not vs) (not s6)) 'south)
      ((and vs (not ve) (not s4)) 'east)
      ((and ve (not vn) (not s2)) 'north)
      ((and wn (not we) 'east))
      ((and we (not ws) 'south))
      ((and ws (not ww) 'west))
      (else 'north))))
     
(define (bot-fn sensors state)
  (unless state (set! state (new-state)))
  (remember-walls sensors state)
  (let* ((vv (lookup-visits state))
         (dir (apply bot-control (append sensors vv))))
    (record-movement dir state)
    (cons dir state)))

(send w set-fn bot-fn)
