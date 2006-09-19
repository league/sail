;;; id3tree.scm -- decision tree induction using Quinlan's ID3 algorithm
;;; Copyright 2006 by Christopher League <league@contrapunctus.net>
;;; Time-stamp: <2006-09-14 20:10:14 league>

;;; This is free software; you may copy, distribute and modify it under the
;;; terms of the GNU General Public License, but it comes with NO WARRANTY.

(load "stdlib.scm")
(require (lib "pregexp.ss"))

(define (csv->list fix text)
  (map fix (pregexp-split "," text)))

(define (read-lines convert inport)
  (let ((line (read-line inport)))
    (if (eof-object? line) null
        (cons (convert line) (read-lines convert inport)))))

(define (read-csv filename)
  (call-with-input-file filename
    (partial-apply read-lines (partial-apply csv->list string->symbol))))

(define (parti predicate list)
  (foldr (lambda (element accum)
           (if (predicate element)
               (cons (cons element (car accum)) (cdr accum))
               (cons (car accum) (cons element (cdr accum)))))
         (cons null null)
         list))

;;(define (entro data)
;
;;(define data 
;;  (call-with-input-file "../agaricus-lepiota.data" ((read-lines csv->list)))
;
;;; Here are sample data by Peter Ross, from a document on data mining.
;;; http://www.dcs.napier.ac.uk/~peter/vldb/dm/node11.html
;;;   "You are stranded on a desert island and have no way to determine 
;;;    which of the many types of fruit available are safe to eat. They 
;;;    are of various colours and sizes, some have hairy skins and others 
;;;    are smooth, some have hard flesh and others are soft. After a great 
;;;    deal of stomach ache, you compile the following table."
;;; The conclusion is the first (0th) element of each row.
;(define examples
;  '(('good   'hairy    'brown   'large   'hard)
;    ('good   'hairy    'green   'large   'hard)
;    ('bad    'smooth   'red     'large   'soft)
;    ('good   'hairy    'green   'large   'soft)
;    ('good   'hairy    'red     'small   'hard)
;    ('good   'smooth   'red     'small   'hard)
;    ('good   'smooth   'brown   'small   'hard)
;    ('bad    'hairy    'green   'small   'soft)
;    ('bad    'smooth   'green   'small   'hard)
;    ('good   'hairy    'red     'large   'hard)
;    ('good   'smooth   'brown   'large   'soft)
;    ('bad    'smooth   'green   'small   'soft)
;    ('good   'hairy    'red     'small   'soft)
;    ('bad    'smooth   'red     'large   'hard)
;    ('good   'smooth   'red     'small   'hard)
;    ('bad    'hairy    'green   'small   'hard)))
;
;;; The names of the other attributes (1-4) are provided separately.
;(define attributes
;  '((skin . 1) (color . 2) (size . 3)  (flesh . 4)))
;
;(define (accum-alist data get init update)
;  (let ((alist null))
;    (for-each
;     (lambda (entity)
;       (let* ((key (get entity))
;              (pair (assoc key alist)))
;         (if pair
;             (set-cdr! pair (update entity (cdr pair)))
;             (set! alist (cons (cons key (init entity)) alist)))))
;     data)
;    alist))
;
;;; Partition a list of examples based on the value returned by 'get'.
;(define (partition data get)
;  (accum-alist data get list cons))
;
;(define (histogram data get)
;  (accum-alist data get 
;               (lambda (entity) 1)
;               (lambda (entity n) (+ 1 n))))
;
;(define (a-priori-probs data get)
;  (map (apply-to-cdr (divide-by (length data)))
;       (histogram data get)))
;
;(define (sum-logs alist)
;  (foldr (lambda (pair sum)
;           (+ sum (* (cdr pair) (log2 (cdr pair)))))
;         0 alist))
;
;(define (entropy-part get)
;  (apply-to-cdr (lambda (data) (sum-logs (a-priori-probs data get)))))
;
;(define (entropy data get given)
;  (let ((ps (a-priori-probs data given)))
;    (foldr (lambda (binding sum) 
;             (+ sum (* (cdr binding) (cdr (assoc (car binding) ps)))))
;           0 (map (entropy-part get) (partition data given)))))           
;
;(define (each-attribute data attrs)
;  (if (null? attrs) null
;      (cons (cons (caar attrs) (entropy data (ith 0) (ith (cdar attrs))))
;            (each-attribute data (cdr attrs)))))
;
;(define (best-of list)
;  (best-of-helper (car list) (cdr list)))
;
;(define (best-of-helper best rest)
;  (cond 
;    ((null? rest) best)
;    ((< (cdr best) (cdar rest)) 
;     (best-of-helper (car rest) (cdr rest)))
;    (else
;     (best-of-helper best (cdr rest)))))
;
;(define (sym x) (list 'quote x))
;
;(define (assoc-rm key alist)
;  (cond
;    ((null? alist) null)
;    ((equal? (caar alist) key) (cdr alist))
;    (else (cons (car alist) (assoc-rm key (cdr alist))))))
;                   
;(define (make-branch data attrs)         
;  (let* ((k (best-of (each-attribute data attrs)))
;         (remaining (assoc-rm (car k) attrs))
;         (each (lambda (binding tree)
;                 (if (null? tree)
;                     (build-tree (cdr binding) remaining)
;                     (list 'if (cond
;                                 ((eq? (car binding) #t) (car k))
;                                 ((eq? (car binding) #f) (list 'not (car k)))
;                                 (else (list 'eq? (car k) (car binding))))
;                           (build-tree (cdr binding) remaining)
;                           tree)))))
;    (foldr each null (partition data (ith (cdr (assoc (car k) attrs)))))))
;
;(define (majority h)
;  (foldr (lambda (pair best)
;           (if (> (cdr pair) (cdr best))
;               pair best))
;         (car h)
;         (cdr h)))
;
;(define (build-tree data attrs)
;  (let ((h (histogram data (ith 0))))
;    (cond
;      ((= 1 (length h))
;       (caar h))
;      ((null? attrs)
;       (car (majority h)))
;      (else 
;       (make-branch data attrs)))))
;
;(define tr (build-tree examples attributes))
;(define fn (eval (list 'lambda '(skin color size flesh) tr)))
;
;(define not-ex '((#f 'a #t) (#f 'b #t) (#t 'b #f) (#t 'c #f)))
;(define not-at '((s1 . 1) (s2 . 2)))
