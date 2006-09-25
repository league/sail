(load "id3tree.scm")

(define goal-tree
  '(if (eqv? age '<25) 'high
       (if (eqv? accidents '3+) 'high
           (if (eqv? color 'red) 'medium
               (if (eqv? alarm 'yes) 'low 'medium)))))

(define auto-names
  '(risk age sex accidents color alarm))

(define goal-fn (make-fn goal-tree auto-names))

(define auto-data
  '((high    <25  m  0-2  blue   no)
    (high    <25  f  0-2  blue   yes)
    (high    25+  f  3+   red    no)
    (high    <25  f  3+   blue   yes)
    (high    <25  m  3+   red    no)
    (high    25+  m  3+   green  no)
    (high    <25  m  0-2  red    no)
    (low     25+  m  0-2  blue   yes)
    (medium  25+  m  0-2  red    no)
    (medium  25+  f  0-2  red    yes)
    (high    <25  f  3+   blue   no)
    (high    <25  m  3+   red    yes)
    (high    25+  f  3+   blue   yes)
    (high    25+  m  3+   green  no)
    (medium  25+  f  0-2  green  no)
    (low     25+  m  0-2  green  yes)
    (high    <25  f  0-2  green  no)
    (high    <25  m  0-2  green  yes)
    ))

(define test-data
  '(
    (high    <25  f  3+   green  yes)
    (high    25+  f  3+   red    yes)
    (low     25+  f  0-2  blue   yes)
    (high    <25  m  3+   blue   no)
    (medium  25+  m  0-2  blue   no)
    (high    <25  f  0-2  red    yes)
    (high    25+  m  3+   blue   yes)
    ))

(test-tree goal-tree auto-data auto-names)
;(choose-best-of auto-data (all-tests auto-data))
;(define d1 (cdr (split (ith-eq? 1 '<25) auto-data)))
;(choose-best-of d1 (all-tests d1))
;(define d2 (cdr (split (ith-eq? 1 '<25) d1)))
;(choose-best-of d2 (all-tests d2))
;(define d3 (cdr (split (ith-eq? 5 'no) d2)))
(define tr (build-tree auto-data auto-names))
(test-tree tr auto-data auto-names)
(test-tree tr test-data auto-names)
tr
