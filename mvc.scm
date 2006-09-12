(define model%
  (class object%
    (public subscribe notify)
    (define listeners null)
    (define (subscribe aView)
      (set! listeners (cons aView listeners))
      (send aView update this 'initial))
    (define (notify . args)
      (for-each (lambda (i) (send i update this . args)) listeners))
    (super-new)))

(define cell-model%
  (class model%
    (public wall?)
    (init-field (kind 1))
    (define wallp (= kind 1))
    (define (wall?) wallp)
    (super-new)))

(define *grid-template*
  #(#(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 1 1 1 1 1 1 0 0 0 0 0 0 1)
    #(1 0 0 1 1 0 0 1 1 0 0 0 0 0 0 1)
    #(1 0 0 1 1 0 0 1 1 0 0 0 0 0 0 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 1 1 1 0 0 0 1 1 1 1)
    #(1 0 0 0 0 0 1 1 1 0 0 0 1 1 1 1)
    #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

(define (for-loop f i n)
  (when (< i n) 
    (f i) 
    (for-loop f (+ 1 i) n)))

(define (vector-for-each f v)
  (for-loop (lambda (i) (f i (vector-ref v i)))
            0 (vector-length v)))

(define (vector-map f v0)
  (let ((v1 (make-vector (vector-length v0))))
    (vector-for-each (lambda (i x) (vector-set! v1 i (f x))) v0)
    v1))

(define (vector2d-map f m)
  (vector-map (lambda (v) (vector-map f v)) m))

(define grid-model%
  (class model%
    (public get-cell-at get-all-cells)
    (init-field (template *grid-template*))
    (define cells
      (vector2d-map 
       (λ (i) (new cell-model% (kind i)))
       template))
    (define (get-cell-at r c)
      (vector-ref (vector-ref cells r) c))
    (define (get-all-cells) cells)
    (super-new)))

(define *wall-color* (send the-color-database find-color "DarkRed"))

(define cell-view%
  (class canvas%
    (public update)
    (define (update cm . args)
      (when (send cm wall?)
        (send this set-canvas-background *wall-color*)))
    (super-new (min-width 32) (min-height 32) (style '(border)))))

(define grid-view%
  (class vertical-panel%
    (public update)
    (define (update gm . args)
      (vector-for-each 
       (λ (i v)
         (let ((row (new horizontal-panel% (parent this))))
           (vector-for-each
            (λ (j m)
              (send m subscribe (new cell-view% (parent row))))
            v)))
       (send gm get-all-cells)))
    (super-new)))

(define mm (new grid-model%))
(define fr (instantiate frame% ("Grid world")))
(define gv (new grid-view% (parent fr)))
(send mm subscribe gv)
(send fr show #t)

(define int-model%
  (class model%
    (public get change!)
    (define value 0)
    (define (get) value)
    (define (change! x) 
      (set! value x)
      (send this notify))
    (super-new)))

(define int-label-view%
  (class message%
    (public update)
    (init-field (render number->string))
    (define (update model . args)
      (send this set-label (render (send model get))))
    (super-new)))

(define (english-0-to-10 n)
  (case n
    ((0) "zero")
    ((1) "one")
    ((2) "two")
    ((3) "three")
    ((4) "four")
    ((5) "five")
    ((6) "six")
    ((7) "seven")
    ((8) "eight")
    ((9) "nine")))

(define (english-teens n)
  (case n
    ((10) "ten")
    ((11) "eleven")
    ((12) "twelve")
    ((13) "thirteen")
    ((14) "fourteen")
    ((15) "fifteen")
    ((16) "sixteen")
    ((17) "seventeen")
    ((18) "eighteen")
    ((19) "nineteen")))

(define (english-helper prefix ones)
  (if (= ones 0) prefix
      (string-append prefix "-" (english-0-to-10 ones))))

(define (number-in-english n)
  (let ((ones (modulo n 10)))
    (case (quotient n 10)
      ((0) (english-0-to-10 n))
      ((1) (english-teens n))
      ((2) (english-helper "twenty" ones))
      ((3) (english-helper "thirty" ones))
      ((4) (english-helper "forty" ones))
      ((5) (english-helper "fifty" ones))
      ((6) (english-helper "sixty" ones))
      ((7) (english-helper "seventy" ones))
      ((8) (english-helper "eighty" ones))
      ((9) (english-helper "ninety" ones))
      ((10) "one hundred"))))
     
;(define im (new int-model%))
;(define frame (instantiate frame% ("Example")))
;(define msg (new int-label-view% (label "test") (parent frame)))
;(send im subscribe msg)
;(define mdiv2 (new int-label-view% (label "foo") (parent frame) 
;                   (render (lambda (n) (number->string (quotient n 2))))))
;(send im subscribe mdiv2)
;(define meng (new int-label-view% (label "english             ") (parent frame)
;                  (render number-in-english)))
;(send im subscribe meng)
;(define (sldf s e)
;  (send im change! (send s get-value)))
;(define sld (new slider% (parent frame) (label "value") (callback sldf)
;                 (min-value 0) (max-value 100) (style '(plain horizontal))))
;(send frame show #t)

