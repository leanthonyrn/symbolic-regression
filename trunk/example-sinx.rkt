#lang racket
(require "genetic-programming.rkt")
(require plot)
;(require racket/trace)
(require racket/unsafe/ops)
(require (planet williams/science/random-distributions/gaussian))

;error function
(define (error norm X Y) (let ([res (/ (apply + (map norm (map - X Y))) (exact->inexact (length X)))]) (if (real? res) res +Inf.0)))
(define (calc-fitness tree Xlist Ylist)
  (error square (map (code->function tree) Xlist) Ylist))

(define X-scx (sequence 1 .1 10.0))
(define Y-scx (map (lambda (x) (/ (sin x) x)) X-scx));test/w pop=300,1000,2000

(define X-cpl (sequence 1.0 10.0 100.0))
(define Y-cpl (map (lambda(x) (+ (* x x x) (* x x) (sin x) x 1)) X-cpl)); pop 600

(define (poly F) (calc-fitness F X-cpl Y-cpl))

(define (sinx/x F) (let ([xes (sequence 1.0 .1 50.0)])
                     (calc-fitness F xes 
                                   (map (lambda (x) (+ (/ (sin x) x) (random-gaussian 0 .3))) xes))))

(parameterize
    ([variables '(x)]
     [translation-table `((+ . ,add) (- . ,sub) (* . ,mul) (/ . ,div) (sin . ,sin) (cos . ,cos) (^ . ,sexpt) (rlog . ,rlog))]
     ;[translation-table `((+ . ,unsafe-fl+) (- . ,unsafe-fl-) (* . ,unsafe-fl*) (div . ,div) (sin . ,unsafe-flsin) (cos . ,unsafe-flcos))]
     [symbol/constant 1])
  (random-seed 400000)
  (time (life 50 1000 sinx/x .009)))