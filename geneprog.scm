(load "stdlib.scm")

(define terminals 
  '(s1 s2 s3 s4 s5 s6 s7 s8 north south east west))

(define operators
  '((if . 3) (and . 2) (or . 2) (not . 1)))

(define (tree-size-acc tr n)
  (if (pair? tr)
      (foldr tree-size-acc n tr)
      (+ n 1)))

(define (tree-size tr) (tree-size-acc tr 0))
  
(define (random-program max-height leaves ops)
  (letrec ((j (length leaves))
           (k (length ops))
           (n (+ j k))
           (branch (lambda (h r)
                     (let ((op (list-ref operators r)))
                       (cons (car op)
                             (repeat (cdr op) (partial-apply make h))))))
           (make (lambda (h) 
                   (if (< h 1) (list-ref leaves (random j))
                       (choose h (random n)))))
           (choose (lambda (h r)
                     (if (< r j)
                         (list-ref leaves r)
                         (branch (- h 1) (- r j))))))
    (make max-height)))

(define (nth-subtree-list trees n return continue)
  (if (null? trees) (continue n)
      (nth-subtree (car trees) n return
                   (lambda (k) 
                     (nth-subtree-list (cdr trees) k return continue)))))

(define (nth-subtree tree n return continue)
  (cond
    ((= n 0) (return tree))
    ((pair? tree) 
     (nth-subtree-list (cdr tree) (- n 1) return continue))
    (else (continue (- n 1)))))

(define (replace-nth-subtree-list trees n new return)
  (if (null? trees)
      (return n null)
      (replace-nth-subtree 
       (car trees) n new
       (lambda (k tr)
         (replace-nth-subtree-list 
          (cdr trees) k new
          (lambda (j ts) (return j (cons tr ts))))))))

(define (replace-nth-subtree tree n new return)
  (cond
    ((= n 0) (return -1 new))
    ((pair? tree)
     (replace-nth-subtree-list 
      (cdr tree) (- n 1) new
      (lambda (k ts) (return k (cons (car tree) ts)))))
    (else
     (return (- n 1) tree))))

(define (random-crossover tree1 tree2)
  (let* ((r1 (random (tree-size tree1)))
         (r2 (random (tree-size tree2))))
    (replace-nth-subtree tree1 r1 (nth-subtree tree2 r2 I E) snd)))

(define (I x) x)
(define (snd x y) y)
(define (E x) (error (number->string x)))
(define (ns t j) (nth-subtree t j I E))
(define pop (repeat 20 (partial-apply random-program 8 terminals operators)))
