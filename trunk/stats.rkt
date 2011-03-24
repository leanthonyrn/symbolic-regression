#lang racket
(require plot "genetic-programming.rkt" racket/sandbox (planet williams/science/random-distributions/gaussian))

(current-directory "/home/milan/fax/Diplomski/CHARLIE/surf/gen-surf-w-imgs/")
(define files (directory-list))

(define (not-inf? x) (not (eq? x +Inf.0)))

;(define (code->function code vars)
;  ((make-evaluator 'racket/base) `(lambda ,vars ,code)))

#|(with-output-to-file "stats.plot"
  (lambda _
    (map (lambda (file)
           ;(display file)(display '--)
           ;(let* ([pop (with-input-from-file file read)]
           ;       [pop-valid (filter not-inf? (map first pop))]
           ;       [mean (/ (apply + pop-valid) (length pop-valid))]
           ;       [var  (/ (apply + (map sqr (map (lambda (xi) (- xi mean)) pop-valid))) (length pop-valid))])
        
           ;  (display (format "~a\t~a\t~a\n" (first (first pop)) mean var)))) files)))
           (let ([pop (with-input-from-file file read)]
                 [histogram (make-vector 30)])
             (map (lambda (n) (vector-set! histogram n (add1 (vector-ref histogram n))))
              (map tree-depth (map second pop)))
             (display histogram) (newline)
             )) files)))|#

(map (lambda (file)
       (let* ([gens (with-input-from-file file read)]
              [Fs (take (map second gens) 30)]
              [Funs (map (curryr code->function '(x y)) Fs)])
         (begin (plot3d (surface
                         (lambda (x y)
                           (/ (apply + (map (lambda (f) (f x y)) Funs)) 5))
                          #:color 'red #:samples 50)
         #:x-min -30 #:x-max 30 #:y-min -30 #:y-max 30 #:z-min 0 #:z-max 1.5 #:width 800 #:height 800
         #:x-label "" #:y-label "" #:z-label "" #:out-file (string-append 
                                                            "/home/milan/fax/Diplomski/CHARLIE/slike/m30-big-" 
                                                            (path->string file) ".png")) 'ok)))
       files)


#;(plot3d (mix
         (surface (lambda (x y) (+ (random-gaussian 0 .075) (/ 1 (+ (* x x) (* y y) x (* x y) y 1))))
                    #:color 'green #:samples 50))
         ;(surface (lambda (x y) (/ 1 (+ 1 x y (* x y) (* x x) (* y y)))) #:color 'green))
         #:x-min -9.5 #:x-max 9.5 #:y-min -9.5 #:y-max 9.5 #:z-min 0 #:z-max 1.5 #:width 800 #:height 800
         #:x-label "" #:y-label "" #:z-label "")