;;; id3tree.scm -- decision tree induction using Quinlan's ID3 algorithm
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-09-21 12:00:25 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "stdlib.scm")
(require (lib "pregexp.ss"))

(define (randomly-by p)
  (lambda (entity) (< (random) p)))

(define (csv->list fix text)
  (map fix (pregexp-split "," text)))

(define (quoted-sym string)
  (list 'quote (string->symbol string)))

(define (read-lines convert inport)
  (let ((line (read-line inport)))
    (if (eof-object? line) null
        (cons (convert line) (read-lines convert inport)))))

(define (read-csv filename)
  (call-with-input-file filename
    (partial-apply read-lines (partial-apply csv->list string->symbol))))

(define (histogram get data)
  (foldr (lambda (element accum)
           (let* ((value (get element))
                  (binding (assoc value accum)))
             (cond
               ((pair? binding)
                (set-cdr! binding (+ 1 (cdr binding)))
                accum)
               (else
                (cons (cons value 1) accum)))))
         null data))

(define (entropy-term p)
  (- (* p (log2 p))))

(define (entropy data)
  (let ((n (length data)))
    (foldr (lambda (binding accum)
             (+ accum (entropy-term (/ (cdr binding) n))))
           0 (histogram (ith 0) data))))
  
(define (split test data)
  (foldr (lambda (element accum)
           (if (test element)
               (cons (cons element (car accum)) (cdr accum))
               (cons (car accum) (cons element (cdr accum)))))
         (cons null null)
         data))

(define (entropy-of-split test data)
  (let* ((n (length data))
         (p (split test data)))
    (+ (* (/ (length (car p)) n) (entropy (car p)))
       (* (/ (length (cdr p)) n) (entropy (cdr p))))))

(define (tests-in-column i data)
  (let ((h (histogram (ith i) data)))
    (if (or (null? h) (null? (cdr h)))  
        ;; if there are fewer than 2 values, no test is possible
        null
        (map (lambda (binding) (list i (car binding))) h))))

(define (all-tests data)
  (let ((n (length (car data)))
        (ts null))
    (for-loop 1 n (lambda (i) 
                    (set! ts (append ts (tests-in-column i data)))))
    ts))

(define (choose-best-of data tests)
  (foldr (lambda (test best)
           (let ((e (entropy-of-split (apply ith-eq? test) data)))
             (if (< e (car best))
                 (cons e test)
                 best)))
         (cons 1 #f)
         tests))

(define (build-tree data names)
  (let ((h (histogram (ith 0) data))
        (ts (all-tests data)))
    (cond
      ((null? h) (error "data set is empty"))
      ((null? (cdr h)) (list 'quote (caar h))) ; all elements classified same way
      ((null? ts) (majority h))
      (else
       (let* ((t0 (cdr (choose-best-of data ts)))
              (p (split (apply ith-eq? t0) data)))
         (list 'if 
               (list 'eqv? (list-ref names (car t0)) 
                     (list 'quote (cadr t0)))
               (build-tree (car p) names) 
               (build-tree (cdr p) names)))))))

(define (make-fn tree names)
  (eval (list 'lambda names tree)))

(define (test-tree tree data names)
  (let ((fn (make-fn tree names)))
    (/ (foldr (lambda (element score)
                (if (eq? (apply fn element) (car element))
                    (+ 1 score)
                    score))
              0 data)
       (length data))))

(define (tree-size tree)
  (cond
    ((and (pair? tree) (eq? (car tree) 'if))
     (+ 1 (tree-size (caddr tree)) (tree-size (cadddr tree))))
    (else 0)))

(define (accuracy-with ratio data)
  (let* ((p (split (randomly-by ratio) data))
         (train (car p))
         (test (cdr p))
         (tree (build-tree train shroom-names)))
    (printf "~s examples, ~s nodes~n" (length train) (tree-size tree))
    (exact->inexact (test-tree tree test shroom-names))))

;(define data (read-csv "shrooms.data"))
;(define tr (build-tree data shroom-names))
;(define fn (make-fn tr shroom-names))

(define shroom-names
  '(classification           ; edible=e,poisonous=p
    cap-shape                ; bell=b,conical=c,convex=x,flat=f,
                             ; knobbed=k,sunken=s
    cap-surface              ; fibrous=f,grooves=g,scaly=y,smooth=s
    cap-color                ; brown=n,buff=b,cinnamon=c,gray=g,green=r,
                             ; pink=p,purple=u,red=e,white=w,yellow=y
    bruises?                 ; bruises=t,no=f
    odor                     ; almond=a,anise=l,creosote=c,fishy=y,foul=f,
                             ; musty=m,none=n,pungent=p,spicy=s
    gill-attachment          ; attached=a,descending=d,free=f,notched=n
    gill-spacing             ; close=c,crowded=w,distant=d
    gill-size                ; broad=b,narrow=n
    gill-color               ; black=k,brown=n,buff=b,chocolate=h,gray=g,
                             ; green=r,orange=o,pink=p,purple=u,red=e,
                             ; white=w,yellow=y
    stalk-shape              ; enlarging=e,tapering=t
    stalk-root               ; bulbous=b,club=c,cup=u,equal=e,
                             ; rhizomorphs=z,rooted=r,missing=?
    stalk-surface-above-ring ; ibrous=f,scaly=y,silky=k,smooth=s
    stalk-surface-below-ring ; ibrous=f,scaly=y,silky=k,smooth=s
    stalk-color-above-ring   ; brown=n,buff=b,cinnamon=c,gray=g,orange=o,
                             ; pink=p,red=e,white=w,yellow=y
    stalk-color-below-ring   ; brown=n,buff=b,cinnamon=c,gray=g,orange=o,
                             ; pink=p,red=e,white=w,yellow=y
    veil-type                ; partial=p,universal=u
    veil-color               ; brown=n,orange=o,white=w,yellow=y
    ring-number              ; none=n,one=o,two=t
    ring-type                ; cobwebby=c,evanescent=e,flaring=f,large=l,
                             ; none=n,pendant=p,sheathing=s,zone=z
    spore-print-color        ; black=k,brown=n,buff=b,chocolate=h,green=r,
                             ; orange=o,purple=u,white=w,yellow=y
    population               ; abundant=a,clustered=c,numerous=n,
                             ; scattered=s,several=v,solitary=y
    habitat                  ; grasses=g,leaves=l,meadows=m,paths=p,
                             ; urban=u,waste=w,woods=d
    ))
