#lang racket
(require "genetic-programming.rkt"
         "gauss-newton.rkt"
         (prefix-in f: "safe-defines.rkt"))

(define (nan? x) (not (= x x)))

(define (mean lst)
  (/ (apply + lst) (length lst)))
(define (variance lst meanval)
  (/ (apply + (map (λ (x) (sqr (- x meanval))) lst)) (sub1 (length lst))))
(define (covariance lstX lstY meanX meanY)
  (/ (apply + (map (λ (x y) (* (- x meanX) (- y meanY))) lstX lstY)) (sub1 (length lstX))))

(define vars '(x y))

;\frac{10}{ .314x^2 + 3.4y^2 + x + 1.6xy + 3y + 4}
(define data (with-input-from-file "input/surf-nocoeff.list" read))

(define datX (map first  data))
(define datY (map second data))
(define datZ (map third  data))

(define fitness
  (λ (F)
    (let* ([xmean (mean datZ)]
           [sigmax2 (variance datZ xmean)]
           [Y (map (code->function F vars) datX datY)]
           [ymean (mean Y)]
           [sigmay2 (variance Y ymean)]
           [sigmaxy (covariance datZ Y xmean ymean)]
           [fit (add1 (* -1 (/ (* 4 sigmaxy xmean ymean)
                               (* (+ sigmax2 sigmay2) (+ (sqr xmean) (sqr ymean))))))])
           (if (nan? fit) +Inf.0 fit))))


(parameterize
    ([variables vars]
     [translation-table `((+ . ,f:+) (- . ,f:-) (* . ,f:*) (/ . ,f:/))]
     [symbol/constant 1]
     [inter-opt (lambda (x) x)])
;  (random-seed 664)
  (life 2000 3000 fitness 1e-6))