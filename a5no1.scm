(load "ac3algo.scm")
(define silly%
  (class ac3algo%
     (override constraint-on constraint-between)
     (define (constraint-on var)
       (lambda (val)
         ;; YOUR CODE HERE
         #t))
     (define (constraint-between var1 var2)
       (lambda (val1 val2)
         ;; YOUR CODE HERE
         #t))
     (super-new
      (variables '(x y z))
      (values '(0 1 2 3 4 5 6)))))
(define s (new silly%))
(send s search)
