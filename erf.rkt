#lang racket
(require "genetic-programming.rkt"
         (prefix-in f: "safe-defines.rkt"))

(define (nan? x) (not (= x x)))

(define dat (with-input-from-file "input/erf.list" read))

(define Xs (map first dat))
(define Ys (map second dat))

(define (mean lst)
  (/ (apply + lst) (length lst)))
(define (variance lst meanval)
  (/ (apply + (map (λ (x) (sqr (- x meanval))) lst)) (sub1 (length lst))))
(define (covariance lstX lstY meanX meanY)
  (/ (apply + (map (λ (x y) (* (- x meanX) (- y meanY))) lstX lstY)) (sub1 (length lstX))))



(define fitness
  (λ (F)
    (let* ([xmean (mean Ys)]
           [sigmax2 (variance Ys xmean)]
           [Y (map real-part (map (code->function F '(h)) Xs))]
           [ymean (mean Y)]
           [sigmay2 (variance Y ymean)]
           [sigmaxy (covariance Ys Y xmean ymean)]
           [fit (add1 (* -1 (/ (* 4 sigmaxy xmean ymean) (* (+ sigmax2 sigmay2) (+ (sqr xmean) (sqr ymean))))))])
           (if (nan? fit) +Inf.0 fit))))

(define fitness-mse
  (λ (F)
    (let ([fit (/ (apply + (map sqr (map - (map (code->function F '(h)) Xs) Ys))) (length Ys))])
      (cond [[complex? fit] +Inf.0]
            [[< 0 fit] (* -1 fit)]
            [(real? fit) fit]
            [else +Inf.0]))))

(parameterize
    ([variables '(h)]
     [translation-table `((+ . ,f:+) (- . ,f:-) (* . ,f:*) (/ . ,f:/) (exp . ,f:exp) (sqrt . ,sqrt))]
     [symbol/constant .75])
  (random-seed 664)
  (life 1000 5000 fitness-mse 1e-6))