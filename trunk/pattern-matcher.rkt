#lang racket
#|
Pattern matcher
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
0.0.0 Wed Sep  10 2010 - initial implementation
|#
(provide (except-out (all-defined-out) patvar? patvar-symbol? patvar-any?))
(define (atom? x) (not (pair? x)))

;rules have following semantics
;?m ?n ?o ?p ?q matches numbers
(define numbers (make-parameter '("m" "n" "i" "j" "k")))
;?x ?y ?z ?i ?j matches variables (symbols)
(define symbols (make-parameter '("x" "y" "z")))
;?a ?b ?c ?d ?e matches anything
(define anything (make-parameter '("a" "b" "c" "d" "e")))
;?* matches segment
(define segments (make-parameter '("*" "#")))

(define (patvar? x)
  (and (symbol? x) (equal? #\? (string-ref (symbol->string x) 0))))
(define (patvar-number? x)
  (if (and (patvar? x) (member (substring (symbol->string x) 1 2) (numbers))) #t #f))
(define (patvar-symbol? x)
  (if (and (patvar? x) (member (substring (symbol->string x) 1 2) (symbols))) #t #f))
(define (patvar-any? x)
  (if (and (patvar? x) (member (substring (symbol->string x) 1 2) (anything))) #t #f))
(define (rematchable? x) 
  (if (and (patvar? x) (> (string-length (symbol->string x)) 2)
           (equal? #\. (string-ref (symbol->string x) 2)))
      #f
      #t))

;binds env assoc into expression act
;(define (bind-fancy act env)
;  (evaluate-emit `(let ,env ,act)))

(define (bind expr env)
  (cond [(null? env) expr]
        [(null? expr) null]
        [(atom? expr)
         (cond [(assoc expr env) => cdr]
               [else expr])]
        [(pair? expr)
         (cons (bind (car expr) env)
               (bind (cdr expr) env))]))

(define (match-pattern pattern exp [env null])
  (cond [(not env) #f]
        [(pair? pattern)
         (and (pair? exp)
              (match-pattern (bind (cdr pattern) env) (cdr exp)
                             (match-pattern (bind (car pattern) env) (car exp)
                              env)))]
        [(patvar? pattern) (match-variable pattern exp env)]
        [(equal? exp pattern) env]
        [else #f]))
        
(define (match-variable variable exp env)
  (if [or (and (patvar-number? variable) (number? exp))
           (and (patvar-symbol? variable) (symbol? exp))
           (patvar-any? variable)]
      (if (rematchable? variable)
          [cons (cons variable exp) env]
          (if (member exp (map cdr env)) #f ;check if it is already bound
              [cons (cons variable exp) env]))
      #f))