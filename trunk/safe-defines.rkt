#lang racket
(provide (rename-out [safe-log  log]
                     [safe-expt expt]
                     [safe-sin  sin]
                     [safe-cos  cos]
                     [safe-add  +]
                     [safe-sub  -]
                     [safe-div  /]
                     [safe-mul  *]
                     [safe-exp  exp]))

(define (infinity? x) (or (eq? x +Inf.0) (eq? x -Inf.0)))

(define (safe-log x) (if (zero? x) +Inf.0 (log x)))
(define (safe-expt x y) (if (zero? x) 0 (expt x y)))
(define (safe-add x y) (+ x y))
(define (safe-sub x y) (- x y))
(define (safe-mul x y) (* x y))
(define (safe-div x y) (if (zero? y) +Inf.0 (exact->inexact (/ x y))))
(define (safe-sin x) (if (infinity? x) (* (sgn x) +Inf.0) (sin x)))
(define (safe-cos x) (if (infinity? x) (* (sgn x) +Inf.0) (cos x)))
(define (safe-exp x) (exp x))