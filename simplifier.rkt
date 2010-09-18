#lang racket
#|
EULER: Rule based expression simplifier
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
|#

(require racket/sandbox)
(require mzlib/defmacro)
(require "pattern-matcher.rkt")
(provide simplify pre-eval-inspector)

(define rules (make-parameter
               '{[(- ?a ?a)                         0]
                 [(* 0 ?a)                          0]
                 [(* 1 ?a)                         ?a]
                ;[(* ?a ?a)                  (^ ?a 2)]
                 [(/ ?a 0)                     +Inf.0]
                 [(/ 0 ?a)                          0]
                 [(/ ?a ?a)                         1]
                 [(- ?a 0)                         ?a]
                 [(+ 0 ?a)                         ?a]
                 [(+ ?a ?a)                  (* 2 ?a)]
                 [(* ?a ?n)                 (* ?n ?a)]
                 [(* ?n (* ?m ?a))   (* (* ?n ?m) ?a)]
                 [(* ?x (* ?n ?y))   (* ?n (* ?x ?y))]
                 [(* (* ?n ?x) ?y)   (* ?n (* ?x ?y))]
                 [(* ?x (/ ?y ?x))                 ?y]
                 [(/ (* ?n ?a) (* ?m ?a))   (/ ?n ?m)]
                 [(^ ?a 1)                         ?a]
                 [(^ ?a 0)                          1]
                 ; D - differentiation operator - d/dx only
                 [(D ?x)    1]
                 [(D ?n)    0]
                 [(D (/ 1 ?x))   (log ?x)]
                 [(D (+ ?a ?b))  (+ (D ?a) (D ?b))]
                 [(D (- ?a ?b))  (- (D ?a) (D ?b))]
                 [(D (* ?a ?b))  (+ (* (D ?a) ?b) (* (D ?b) ?a))]
                 [(D (/ ?a ?b))  (/ (- (* (D ?a) ?b) (* (D ?b) ?a)) (* ?b ?b))]
                 [(D (^ ?x ?n))  (* ?n (^ ?x (- ?n 1)))]
                 }))



(define (pattern rule) (car rule))
(define (action  rule) (cadr rule))

;if you need some additional constraints on evaluated code (for example guard against division by zero parameterize this function)
(define pre-eval-inspector (make-parameter (lambda (exp) exp)))

(define evaluate (make-evaluator 'racket/base))

(define (simplify exp)
  (if [atom? exp]
      exp
      (simplify-exp (map simplify exp))))


(define (simplify-exp exp)
  (if (and (andmap atom? (cdr exp)) (andmap number? (cdr exp)) (not (equal? (car exp) 'D)))
      (evaluate ((pre-eval-inspector) exp))
      (let [(translation (translate-once exp (rules)))]
        (if translation
            (simplify translation) ;translate until no rules apply
            exp))))

(define (translate-once exp rules)
    (cond [(null? rules) #f] ;reached end of rules - fail translation
          [(match-pattern (pattern (first rules)) exp) => (lambda (env) (bind (action (first rules)) env))]
          [else (translate-once exp (rest rules))]))