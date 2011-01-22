#lang racket
(require plot racket/sandbox (planet williams/science/random-distributions/gaussian))

(current-directory "generations-mse-surf+noise")
(define files (directory-list))

(define (not-inf? x) (not (eq? x +Inf.0)))

(define (code->function code vars)
  ((make-evaluator 'racket/base) `(lambda ,vars ,code)))

#;(with-output-to-file "stats.plot"
  (lambda _
    (map (lambda (file)
           ;(display file)(display '--)
           (let* ([pop (with-input-from-file file read)]
                  [pop-valid (filter not-inf? (map first pop))]
                  [mean (/ (apply + pop-valid) (length pop-valid))]
                  [var  (/ (apply + (map sqr (map (lambda (xi) (- xi mean)) pop-valid))) (length pop-valid))])
        
             (display (format "~a\t~a\t~a\n" (first (first pop)) mean var)))) files)))


#;(map (lambda (file)
       (let* ([gens (with-input-from-file file read)]
              [F (second (first gens))])
         (plot3d (surface (code->function F '(x y))
                          #:color 'red #:samples 50)
         #:x-min -9.5 #:x-max 9.5 #:y-min -9.5 #:y-max 9.5 #:z-min 0 #:z-max 1.5 #:width 800 #:height 800
         #:x-label "" #:y-label "" #:z-label "" #:out-file (string-append 
                                                            "/home/milan/fax/Diplomski/CHARLIE/generations-mse-surf+noise/" 
                                                            (path->string file) ".png")))) 
       files)


(plot3d (mix
         (surface (lambda (x y) (+ (random-gaussian 0 .075) (/ 1 (+ (* x x) (* y y) x (* x y) y 1))))
                    #:color 'green #:samples 50))
         ;(surface (lambda (x y) (/ 1 (+ 1 x y (* x y) (* x x) (* y y)))) #:color 'green))
         #:x-min -9.5 #:x-max 9.5 #:y-min -9.5 #:y-max 9.5 #:z-min 0 #:z-max 1.5 #:width 800 #:height 800
         #:x-label "" #:y-label "" #:z-label "")