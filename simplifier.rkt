#lang racket
#|
EULER: Rule based expression simplifier and differentiator
Copyright (C) 2010 Milan Markovic (zivotinja@gmail.com | milan@elfak.rs)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Revision history:
0.0.0 Wed Sep  8 2010 - initial implementation
0.0.2 Thu Nov 25 2010 - added differentiation operator -- needs testing, TODO: display simplificator
|#

(require racket/sandbox)
(require "pattern-matcher.rkt")
(provide simplify D drules srules pre-eval-inspector)

(define rlog log)
(define ^ expt)

;simplification rules
(define srules (make-parameter
               '{[(- ?a ?a)    0]
                 [(* 0 ?a)     0]
                 [(* 1 ?a)    ?a]
                 [(+ 0 ?a)    ?a]
                 [(expt ?a 1) ?a]
                 [(expt ?a 0) 1]
                 [(/ 0 ?a)    0]                 
                 [(- ?a 0)    ?a]
                 [(/ 1 ?a)    (expt ?a -1)]
                 [(+ 0 ?a)    ?a]
                 [(+ ?a ?a)   (* 2 ?a)]
                 [(+ (* ?m ?a)(* ?n ?a)) (* (+ ?m ?n) ?a)]
                 [(* ?a ?n)   (* ?n ?a)]
                 [(+ ?a ?n)   (+ ?n ?a)]
                 [(/ ?a ?a)   1]
                 [(* ?a ?a)   (expt ?a 2)]
                 [(* ?n (* ?m ?a))   (* (* ?n ?m) ?a)]
                 [(* ?x (* ?n ?y))   (* ?n (* ?x ?y))]
                 [(* (* ?n ?x) ?y)   (* ?n (* ?x ?y))]
                 [(* ?b (/ ?a ?b))   ?a]
                 [(* (/ ?a ?b) ?b)   ?a]
                 [(/ (* ?n ?a) (* ?m ?a)) (*(/ ?n ?m) ?a)]
                 [(expt ?a 1)   ?a]
                 [(expt ?a 0)    1]
                 [(* ?x (expt ?x n))         (expt ?x (+ n 1))]
                 [(* (expt ?a ?n) (expt ?a ?m)) (expt ?a (+ ?m ?n))]
                 [(* (* ?n ?x) (* ?m ?x)) (* (* ?m ?n) ?x)]
                 [(expt (expt ?x ?m) ?n)        (expt ?x (* ?n ?m))]
                 [(* (expt ?x ?a) (* (expt ?x ?b) ?c)) (* (expt ?x (+ ?a ?b)) ?c)]
                 }))

;differentiation rules
(define drules (make-parameter 
                '{[(D ?x ?x) 1]
                 [(D ?x ?y)  0]
                 [(D ?x ?n)  0]
                 [(D ?x (sin ?a))   (* (cos ?a) (D ?x ?a))]
                 [(D ?x (cos ?a))   (* (* -1 (sin ?a)) (D ?x ?a))]
                 [(D ?x (log ?a))   (* (/ 1 ?a) (D ?x ?a))]
                 [(D ?x (exp ?a))   (* (exp ?a) (D ?x ?a))]
                 [(D ?x (+ ?a ?b))  (+ (D ?x ?a) (D ?x ?b))]
                 [(D ?x (- ?a ?b))  (- (D ?x ?a) (D ?x ?b))]
                 [(D ?x (* ?a ?b))  (+ (* (D ?x ?a) ?b) (* (D ?x ?b) ?a))]
                 [(D ?x (/ ?a ?b))  (/ (- (* (D ?x ?a) ?b) (* (D ?x ?b) ?a)) (* ?b ?b))]
                 [(D ?x (expt ?a ?n))  (* (* ?n (expt ?a (- ?n 1))) (D ?x ?a))]
                 [(D ?x (expt ?n ?a))  (* (D ?x ?a) (* (expt ?n ?a) (log ?n)))]
                 [(D ?x (expt ?a ?b))  (* (expt ?a ?b) (D ?x (* (log ?a) ?b)))]                  
                 }))

(define (pattern rule) (car rule))
(define (action  rule) (cadr rule))

(define evaluate (make-evaluator 'racket/base))
(define pre-eval-inspector (make-parameter (λ (x) x)))

(define (simplify exp)
  (if [atom? exp]
      exp
      (simplify-exp (map simplify exp))))

(define (simplify-exp exp)
  (if (and (andmap atom? (cdr exp)) (andmap number? (cdr exp)))
      (evaluate ((pre-eval-inspector) exp))
      (let [(translation (translate-once exp (srules)))]
        (if translation
            (simplify translation) ;translate until no rules apply
            exp))))

(define (D var exp)
  (simplify (diff (list 'D var (simplify exp)))))

(define (diff exp)
  (if [atom? exp]
      exp
      (diff-exp (map diff exp))))

(define (diff-exp exp)
  (let [(translation (translate-once exp (drules)))]
    (if translation
        (diff translation) ;translate until no rules apply
        exp)))

;heart of the system
(define (translate-once exp rules)
    (cond [(null? rules) #f] ;reached end of rules - fail translation
          [(match-pattern (pattern (first rules)) exp) => (lambda (env) (bind (action (first rules)) env))]
          [else (translate-once exp (rest rules))]))