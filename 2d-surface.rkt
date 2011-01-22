#lang racket
(require "genetic-programming.rkt"
         "gauss-newton.rkt"
         "simplifier.rkt"
         (planet williams/science/random-distributions/gaussian)
         (prefix-in f: "safe-defines.rkt"))

(define (nan? x) (not (= x x)))

(define (mean lst)
  (/ (apply + lst) (length lst)))
(define (variance lst meanval)
  (/ (apply + (map (λ (x) (sqr (- x meanval))) lst)) (exact->inexact (length lst))))
(define (covariance lstX lstY meanX meanY)
  (/ (apply + (map (λ (x y) (* (- x meanX) (- y meanY))) lstX lstY)) (exact->inexact (length lstX))))

(define vars '(x y))

(define data (with-input-from-file "input/surf-nocoeff+noise.list" read))

(define datX (map first  data))
(define datY (map second data))
(define datZ (map third  data))

(define fit 
  (lambda (F)
    (let ([out (map (code->function F vars) datX datY)])
      (let ([fit (/ (apply + (map sqr (map - out datZ))) (length datZ))])
        (if (nan? fit) +Inf.0 (exact->inexact fit))))))

(define fitness
  (lambda (F)
    (let* ([xmean (mean datZ)]
           [sigmax2 (variance datZ xmean)]
           [Y (map (code->function F vars) datX datY)]
           [ymean (mean Y)]
           [sigmay2 (variance Y ymean)]
           [sigmaxy (covariance datZ Y xmean ymean)]
           [fit (add1 (* -1 (/ (* 4 sigmaxy xmean ymean)
                               (* (+ sigmax2 sigmay2) (+ (sqr xmean) (sqr ymean))))))])
      
      (if (nan? fit) +Inf.0 fit))))

(define (every n #:do thunk #:otherwise thunko)
  (let ([counter 0])
    (lambda (x)
      (set! counter (add1 counter))
      (display counter) (newline)
      (if (zero? (remainder counter n)) (thunk x) (thunko x)))))


(parameterize
    ([variables vars]
     [translation-table `((+ . ,f:+) (- . ,f:-) (* . ,f:*) (/ . ,f:/))]
     [symbol/constant 1]
     [mutation-percent 5]
     [stop-percent 10]
     [subexp-percent 10]
     [crossover-percent 10]
     [initial-complexity 7])
    ; [inter-opt (every 10 #:do (lambda (F) (gauss-newton (simplify F) '(x y) (map list datX datY) 4)) 
    ;                      #:otherwise (lambda (F) F))])
     ;(random-seed 664)
  (life 500 5000 fit 1e-6))