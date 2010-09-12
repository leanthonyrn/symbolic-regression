#lang racket
(require "genetic-programming.rkt")
(require plot)
;(require racket/trace)
(require (planet williams/science/random-distributions/gaussian))

;error function
(define (error norm X Y) (let ([res (/ (apply + (map norm (map - X Y))) (exact->inexact (length X)))]) (if (real? res) res +Inf.0)))
(define (calc-fitness tree Xlist Ylist)
  (let ([F (code->function tree)])
    (error square (map F Xlist)
           Ylist)))

(define (cos2x F) (let ([xes (sequence (- pi) .2 pi)])
                     (calc-fitness F xes 
                                   (map (lambda (x) (cos (* 2 x))) xes))))

(parameterize
    ([variables '(x)]
     [translation-table `((+ . ,add) (- . ,sub) (* . ,mul) (/ . ,div) (sin . ,sin) (^ . ,sexpt))]
     [symbol/constant 1/2]
     [random-constant (lambda () (random 2))])
  (random-seed 400000)
  (life 20 4000 cos2x 1e-7))