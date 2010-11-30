#lang racket
(require "genetic-programming.rkt")
(require plot)
;(require racket/trace)
(require racket/unsafe/ops)
(require (planet williams/science/random-distributions/gaussian))

;error function
;(define (error norm X Y) (let ([res (/ (apply + (map norm (map - X Y))) (exact->inexact (length X)))]) (if (real? res) res +Inf.0)))
;(define (calc-fitness tree Xlist Ylist)
;  (error square (map (code->function tree) Xlist) Ylist))

;(define (sinx/x F) (calc-fitness F X-scx Y-scx))
;(map (lambda (x) (+ (/ (sin x) x) (random-gaussian 0 .3))) xes))))

(define Xs (sequence 1 .1 50.0))
(define Ys (map (lambda (x) (+ (* x x) x)) Xs))

(define (mean lst)
  (/ (apply + lst) (length lst)))
(define (variance lst meanval)
  (/ (apply + (map (λ (x) (square (- x meanval))) lst)) (sub1 (length lst))))
(define (covariance lstX lstY meanX meanY)
  (/ (apply + (map (λ (x y) (* (- x meanX) (- y meanY))) lstX lstY)) (sub1 (length lstX))))



(define fitness
  (λ (F)
    (let* ([xmean (mean Ys)]
           [sigmax2 (variance Ys xmean)]
           [Y (map real-part (map (code->function F) Xs))]
           [ymean (mean Y)]
           [sigmay2 (variance Y ymean)]
           [sigmaxy (covariance Ys Y xmean ymean)])
      (add1 (* -1 (/ (* 4 sigmaxy xmean ymean) (* (+ sigmax2 sigmay2) (+ (square xmean) (square ymean)))))))))

#;(parameterize
    ([variables '(x)]
     [translation-table `((+ . ,add) (- . ,sub) (* . ,mul) (/ . ,div) (sin . ,sin) (cos . ,cos) (^ . ,sexpt) (rlog . ,rlog))]
     [symbol/constant 1])
  ;(random-seed 400000)
  (time (life 500 700 fitness 1e-6)))