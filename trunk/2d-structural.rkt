#lang racket
(require "genetic-programming.rkt")
(require plot)
;(require racket/unsafe/ops)
;(require (planet williams/science/random-distributions/gaussian))

(define (nan? x) (not (= x x)))

(define dat (with-input-from-file "2d.list" read))

(define Xs (map first dat))
(define Ys (map second dat))
(define Zs (map third dat))

(define (mean lst)
  (/ (apply + lst) (length lst)))
(define (variance lst meanval)
  (/ (apply + (map (λ (x) (square (- x meanval))) lst)) (sub1 (length lst))))
(define (covariance lstX lstY meanX meanY)
  (/ (apply + (map (λ (x y) (* (- x meanX) (- y meanY))) lstX lstY)) (sub1 (length lstX))))



(define fitness
  (λ (F)
    (define (Q x y)
      (let* ([xmean (mean x)]
             [sigmax2 (variance x xmean)]
             [ymean (mean y)]
             [sigmay2 (variance y ymean)]
             [sigmaxy (covariance x y xmean ymean)])
        (/ (* 4 sigmaxy xmean ymean) (* (+ sigmax2 sigmay2) (+ (square xmean) (square ymean))))))

    (let* ([Z (map real-part (map (code->function F) Xs Ys))]
           [fit (add1 (* -1 (Q Zs Z)))])
      (if (nan? fit) +Inf.0 fit))))


(parameterize
    ([variables '(D h)]
     [translation-table `((+ . ,add) (- . ,sub) (* . ,mul) (/ . ,div) (exp . ,exp) (rlog . ,rlog))]
     [symbol/constant .75])
  (random-seed 666)
  (time (life 1000 1000 fitness 1e-6 500)))
