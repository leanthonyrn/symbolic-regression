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

(define X-scx (sequence 1 .1 50.0))
(define Y-scx (map (lambda (x) (+ (/ (sin x) x) (random-gaussian 0 .3))) X-scx))

(define (sinx/x F) (calc-fitness F X-scx Y-scx))
                                   ;(map (lambda (x) (+ (/ (sin x) x) (random-gaussian 0 .3))) xes))))

(parameterize
    ([variables '(x)]
     [translation-table `((+ . ,add) (- . ,sub) (* . ,mul) (/ . ,div) (sin . ,sin) (cos . ,cos) (^ . ,sexpt) (rlog . ,rlog))]
     ;[translation-table `((+ . ,unsafe-fl+) (- . ,unsafe-fl-) (* . ,unsafe-fl*) (div . ,div) (sin . ,unsafe-flsin) (cos . ,unsafe-flcos))]
     [symbol/constant 1])
  (random-seed 400000)
  (time (life 500 100 sinx/x 1e-6)))