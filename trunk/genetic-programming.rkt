#lang racket
#|
CHARLIE: A symbolic regression system using genetic programming
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
Author: Milan Markovic (11533) milan@elfak.rs

Revision history:
0.0.0 Sun Aug 15 2010 - initial implementation
0.1.0 Tue Aug 24 2010 - removed globals, added parameters instead, tried natural-selection-proportional (discarded but included in this file)
0.1.1 Sun Sep  5 2010 - added simplify (expression simplifier
0.1.2 Wed Sep  8 2010 - moved simplify to a separate module, added sandboxed evaluation
|#
(require mzlib/defmacro)
(require plot)
(require racket/unsafe/ops)
(require racket/sandbox)
(require "simplifier.rkt")
(provide 
 (all-defined-out))
;-------------------------------------------------------------------------------------------------------------
;                  ;      
;    ;;;;   ;;;;  ;;;; ;    ;  ; ;;;
;   ;   ;  ;;  ;;  ;   ;    ;  ;;  ;;
;   ;      ;    ;  ;   ;    ;  ;    ; 
;   ;;;    ;;;;;;; ;   ;    ;  ;    ;;
;      ;;  ;       ;   ;    ;  ;    ;;
;   ;   ;; ;;  ;;  ;;  ;;  ;;  ;;  ;; 
;    ;;;;   ;;;;    ;;  ;;; ;  ; ;;;  
;                              ;      
;                              ;
;-------------------------------------------------------------------------------------------------------------c

;protected functions - removed singularities and with constant arities
(define (rlog x)  (if (zero? x) 1 (log x)))
(define (div x y) (if (zero? y) 0 (/ x y)))
(define (add x y) (+ x y))
(define (sub x y) (- x y))
(define (mul x y) (* x y))
(define (scdr lst) (if (and (list? lst) (not (null? lst))) (cdr lst) null))
(define (sexpt a b) (if (zero? a) 0 (expt a b)))

(define translation-table  (make-parameter `((+ . ,add) (- . ,sub) (* . ,mul) (/ . ,div) (sin . ,sin) (cos . ,cos) (expt . ,sexpt) (rlog . ,rlog))))
(define (arity-table)      (map cons (map car (translation-table)) (map procedure-arity (map cdr (translation-table)))))
(define (function-set)     (map car (translation-table)))
(define (proc? symb)       (member symb (function-set)))

(define mutation-percent   (make-parameter 5.0))     ;what percent of chromosomes should be mutated
(define stop-percent       (make-parameter 5.0))     ;stop probab. at creating function
(define subexp-percent     (make-parameter 10.0))    ;stop probab. at fetching subexpression
(define crossover-percent  (make-parameter 10.0))    ;stop probab. at crossover
(define initial-complexity (make-parameter 5.0))     ;max initial depth of exp
(define allowed-complexity (make-parameter 17.0))    ;max overall depth of exp (Koza. 17)
(define symbol/constant    (make-parameter 0.5))     ;ratio between symbols and constants
(define variables          (make-parameter '(x)))    ;list of variables (for multivariate add more symbols)
(define random-constant    (make-parameter random))
                                               
;we use a sandboxed evaluation to discard memory and time consuming functions. If using scheme other than racket plain eval should do
(define evaluate           (parameterize ([sandbox-eval-limits '(10 5)]);(s MiB) ;TODO: handle these exceptions
                             (make-evaluator 'racket/base)))
;-------------------------------------------------------------------------------------------------------------
;Misc. function
(define (I something) something)   ;identity
(define file with-input-from-file)
(define (date->string date) (format "~a.~a.~a ~a:~a GMT+~a" 
                                   (date-year date) (date-month date) (date-day date) (date-hour date) (date-minute date) 
                                   (/ (date-time-zone-offset date) 3600)))
(define (n->str number nchars)
  (let ([n (number->string number)])
    (string-append (make-string (- nchars (string-length n)) #\0) n)))
(define (square x) (* x x))
(define (atom? x) (not (pair? x)))
(define (leaf? node) (or (number? node) (symbol? node)))
(define (true percent) 
  (>= (* 100 percent) (random 10000)))
(define (tree-depth tree)
  (cond
    [(null? tree) 0]
    [(atom? tree) 1]
    [else (let ([recur (lambda (x) (add1 (tree-depth x)))]
                [zerof (lambda (x) 0)])
            (traverse tree max zerof recur))]))

(define (func-assoc lst)
  (if (null? lst) null
      (let ([f (car lst)])
        (cons (cons (car lst) (evaluate (car lst))) (func-assoc (cdr lst))))))

(define (random-symbol) (list-ref (variables) (random (length (variables)))))
(define (random-function function-set) (list-ref function-set (random (length function-set))))

;removes function symbols from code (sin x) -> (#<procedure sin> x) -between machine and human representation
(define (translate code)
  (let ([unsymb (lambda (symb) (let ([response (assoc symb (translation-table))]) (if response (cdr response) symb)))])
  (if (null? code) null
      (if (atom? code) (unsymb code)
      (traverse code list unsymb translate)))))

(define (proc-arity proc)
  (let ([response (assoc proc (arity-table))]) (if response (cdr response) 0)))

(define (make-leaf)
  (if (true (* 100 (symbol/constant))) (random-symbol) ((random-constant))))

(define (make-function tree-depth)
  ;generate random expression tree
    (letrec ([loop
              (lambda (to-go)
                (cond [(zero? to-go) (make-leaf)]
                      [else
                       (if (true (stop-percent)) (make-leaf)
                           (let* ([proc (random-function (function-set))]
                                  [arity (proc-arity proc)])
                             (cond [(= arity 2) (list proc (loop (sub1 to-go)) (loop (sub1 to-go)))]
                                   [(= arity 1) (list proc (loop (sub1 to-go)))])))]))])
      (loop tree-depth)))

(define (code->function code [vars (variables)] [ev evaluate])
  (ev `(lambda ,vars ,(translate code))))

;                                       ;                     ;             
;                                   ;   ;                     ;             
;                                   ;                         ;             
;    ;;; ;   ;;;;   ; ;;;    ;;;;  ;;;; ;   ;;;;       ;;;;   ;   ;;; ;     
;   ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;;  ;   ;  ;;  ;;     ;;  ;;  ;  ;;  ;;     
;   ;    ;  ;    ;  ;    ;  ;    ;  ;   ;  ;    ;          ;  ;  ;    ;     
;  ;;    ;  ;;;;;;; ;    ;  ;;;;;;; ;   ; ;;            ;;;;  ; ;;    ;     
;   ;    ;  ;    ;  ;    ;  ;    ;  ;   ;  ;    ;     ;    ;  ;  ;    ;     
;   ;;  ;;  ;;  ;;  ;    ;  ;;  ;;  ;;  ;  ;;  ;;     ;   ;;  ;  ;;  ;;     
;    ;;; ;   ;;;;   ;    ;   ;;;;    ;; ;   ;;;;      ;;;; ;; ;   ;;; ;  ;; 
;        ;                                                            ;     
;   ;   ;;                                                       ;   ;;     
;    ;;;;                                                         ;;;;      
;                                                                           

(define (random-subexpression expr depth)
  (define (random-operand expr)
    (cond [(null? expr) null]
          [(list? expr) 
           (let ([proc (car expr)])
             (if (proc? proc)
                 (list-ref (cdr expr) (random (proc-arity proc)))
                 expr))]
          [else expr]))
  (if [or (true (subexp-percent)) (zero? depth)] (random-operand expr)
      (random-operand (random-subexpression expr (sub1 depth)))))

;crossover genes
(define (crossover A B)
  (let ([inserted #f]) ;to ensure single insertion
    (define (insert node ins)
      (cond 
        [(null? ins)  node]
        [(null? node) null]
        [(atom? node) node]
        [(and (true (crossover-percent)) (not inserted)) (set! inserted #t) ins]
        [else (let ([recur (lambda (x) (insert x ins))])
                (traverse node list I recur))]))
    (insert A (random-subexpression B (tree-depth B)))))

(define (restricted-crossover A B)
  (let ([res (crossover A B)])
    (if (<= (tree-depth res) (allowed-complexity)) res
       A)))

(define (mutate tree)
  (define (random-proc arity)
    (let ([proc (random-function (function-set))])
      (if (= arity (proc-arity proc)) proc
          (random-proc arity))))
  (define (change atom)
    (cond [(proc? atom) (random-proc (proc-arity atom))]
          [(symbol? atom) (random-symbol)]
          [else (* atom ((random-constant)))]))
  (define (random-change atom)
    (if (true (mutation-percent)) (change atom) atom))
  (cond 
    [(null? tree) null]
    [(atom? tree) (random-change tree)]
    [else (traverse tree list mutate mutate)]))
            
(define (traverse tree combine root-proc leaf-proc)
  (define (1st-operand exp) (list-ref exp 1))
  (define (2nd-operand exp) (list-ref exp 2))
  (let* ([proc (car tree)]
         [arity (proc-arity proc)])
    (cond
      [(= arity 2) (combine (root-proc proc) (leaf-proc (1st-operand tree)) (leaf-proc (2nd-operand tree)))]
      [(= arity 1) (combine (root-proc proc) (leaf-proc (1st-operand tree)))])))

;end of core functions

;sorting function
(define (insert-before condition? element lst)
  (if (null? lst) (list element)
      (if (condition? element (car lst)) (cons element lst)
          (cons (car lst) (insert-before condition? element (cdr lst))))))

(define (sort-by-fitness apopulus)
  (letrec ([<fitness  (lambda (A B) (< (car A) (car B)))]
           [insert    (lambda (elem lst) (insert-before <fitness elem lst))])
    (foldl insert '() apopulus)))

(define (init-gen population)
  (if (zero? population) null
      (cons (make-function (initial-complexity)) (init-gen (sub1 population)))))

; creates assoc with (fitness . tree) pairs
;(assign-fitness (lambda (F) (calc-fitness F x x)) populus)
(define (assign-fitness fitness populus)
;  (let ([simplified-populus (map simplify populus)])
    (map list (map fitness populus) populus))

(define (normalize-fitness fitness.populus)
  (define (adjust-fitness fitness)
    (/ 1 (+ 1 fitness)))
  (let* ([adjusted-fitness (map adjust-fitness (map car fitness.populus))]
         [sum-adjusted (apply + adjusted-fitness)])
    (map cons 
         (map (lambda (x) (/ x sum-adjusted)) adjusted-fitness)
         (map cdr fitness.populus))))
           
(define (save-generation nr populus)
  (define (pretty-print lst)
    (cond [(null? lst) (newline)]
          [else (write (car lst)) (newline) (pretty-print (cdr lst))]))
  (with-output-to-file (string-append "generations/" (n->str nr 4) ".txt")
    (lambda () 
      (display (format "; CHARLIE: Generation ~a's listing~n; Date: ~a ~n; Formatted as pairs (fitness . expression)~n("
                       nr
                       (date->string (seconds->date (current-seconds)))))
      (pretty-print populus) 
      (display ")"))
     #:mode 'text #:exists 'replace))

(define (load-generation nr)
  (with-input-from-file (string-append "generations/" (n->str nr 4) ".txt") read))

;non proportional selection
(define (natural-selection-old fitness.populus)
  (define (fittest-of n) (inexact->exact (floor (* n (expt (random) 1.3)))))
  (list-ref (list-ref fitness.populus (fittest-of (length fitness.populus))) 1))

;proportional
(define (natural-selection normfit.populus)
  (let loop ([pop normfit.populus] [last '(0 null)] [fit (random)])
    (if (null? pop) (cadr last)
        (if (>= (+ (caar pop) (car last)) fit) (cadar pop)
            (loop (cdr pop) (car pop) fit)))))
          
(define (create-offspring population fitness.populus)
  (if (zero? population) '()
      (cons (mutate (crossover (natural-selection fitness.populus) 
                               (natural-selection fitness.populus)))
            (create-offspring (sub1 population) fitness.populus))))

(define (life max-generations population fitness-function threshold [load -1])
  (let loop ([populus (if (= -1 load) (init-gen population) (map cadr (load-generation load)))]
             [generation (if (= -1 load) 0 load)])
    (let ([fitness.populus (sort-by-fitness (assign-fitness fitness-function populus))])
      (save-generation generation fitness.populus)
      (display "___________________________________________")(newline)
      (display "Generation: ")(display generation)(newline)
      (display "Best score: ")(display (caar fitness.populus))(newline)
      (let ([best-fit (list-ref (car fitness.populus) 1)])
        (if (or (<= (caar fitness.populus) threshold)
                (= generation max-generations))
            (begin (parameterize ([pre-eval-inspector translate]) 
                     (simplify best-fit)))
            (loop (cons best-fit (create-offspring (sub1 population) fitness.populus)) (add1 generation)))))))


;mics functions

(define (t-plot2 tree [x-range '(0 . 10)] [y-range '(0 . 10)])
  (define (prepare-to-plot F) (lambda (x) (let ([res (F x)]) (if (real? res) res (magnitude res)))))
  (plot (line (prepare-to-plot (code->function tree))) #:x-min (car x-range) #:x-max (cdr x-range) #:y-min (car y-range) #:y-max (cdr y-range)))

;plain sequence generator (to generate x points)
(define (sequence start increment end)
  (if (>= start end) null
      (cons start (sequence (+ start increment) increment end))))
         
;          ;                      ;  ;   ;;       
;          ;                      ;  ;  ;;        
;                                 ;     ;;        
;    ;;;;  ;  ; ;;; ;;;   ; ;;;   ;  ; ;;;;;;   ;;
;   ;   ;  ;  ;;  ;;; ;;  ;;  ;;  ;  ;  ;;  ;   ; 
;   ;      ;  ;   ;;   ;  ;    ;  ;  ;  ;;  ;   ; 
;   ;;;;   ;  ;   ;;   ;  ;    ;; ;  ;  ;;  ;; ;; 
;       ;; ;  ;   ;;   ;  ;    ;; ;  ;  ;;   ;;;  
;   ;   ;; ;  ;   ;;   ;  ;;  ;;  ;  ;  ;;   ;;;  
;    ;;;;  ;  ;   ;;   ;  ; ;;;   ;  ;  ;;    ;   
;                         ;                  ;;   
;                         ;                 ;;    
;
;(define (Zero? x)
;  (and (number? x) (zero? x)))
;
;(define (One? x)
;  (and (number? x) (= 1 x)))
;
;(define (not-number? x)
;  (not (number? x)))
;
;(define (simplify tree)
;  (cond
;    ([atom? tree] tree)
;    (else (simplify-exp (map simplify tree)))))
;
;simplifies already simplified expr
; raw version (TODO: rewrite using pattern matcher and rules)
;(define (simplify-exp tree)
;  (define (addition? oper) (or (eq? '+ oper) (eq? '- oper)))
;  (define (multiplication? oper) (or (eq? '* oper) (eq? '/ oper)))
;  (let ([procedure (car tree)]
;        [operands  (cdr tree)])
;    (cond ([andmap number? operands] ((code->function tree '()))) ;eval if no bound variables found
;          ([and (eq? '+ procedure) (apply equal? operands)] (simplify `(* 2 ,(car operands)))) ;could be slow if complex expression
;          ([ormap atom? operands]
;           (if (= 2 (proc-arity (car tree)))
;               (cond
;                 ([and (eq? '/ procedure) (apply eq? operands)]      1)
;                 ([and (eq? '/ procedure) (ormap Zero? operands)]    0)
;                 ([and (eq? '- procedure) (apply eq? operands)]      0)
;                 ([and (eq? '- procedure) (Zero? (list-ref tree 2))] (list-ref tree 1))
;                 ([and (eq? '+ procedure) (ormap number? operands) (ormap Zero? operands)] (car (filter (lambda (x) (not (Zero? x))) operands)))
;                 ([and (eq? '* procedure) (ormap number? operands) (ormap Zero? operands)] 0)
;                 ([and (eq? '^ procedure) (ormap Zero? operands)]    0)
;                 ([and (eq? '^ procedure) (One? (car operands))]     1)
;                 ([and (eq? '^ procedure) (One? (cadr operands))] (car operands))
;                 
;                 ([and (multiplication? procedure) (ormap number? operands) (ormap pair? operands) ; (* 2 (* exp 3)) -> (* 6 exp)
;                       (let* ([subexp (car (filter pair? operands))]
;                              [subproc (car subexp)]
;                              [subops  (cdr subexp)])
;                         (and (multiplication? subproc) (ormap number? subops) (ormap not-number? subops)))]
;                  (simplify (let ([subexp (car (filter pair? operands))])
;                              `(* ,(* ((translate procedure) 1 (car (filter number? operands))) 
;                                      ((translate (car subexp)) 1 (car (filter number? subexp))))
;                                  ,(car (filter not-number? (cdr subexp)))))))
;                 
;                 ([and (addition? procedure) (ormap number? operands) (ormap pair? operands) ; (+ 2 (+ exp 3)) -> (+ 5 exp)
;                       (let* ([subexp (car (filter pair? operands))]
;                              [subproc (car subexp)]
;                              [subops  (cdr subexp)])
;                         (and (addition? subproc) (ormap number? subops) (ormap not-number? subops)))]
;                  (simplify (let ([subexp (car (filter pair? operands))])
;                              `(+ ,(+ ((translate procedure) 0 (car (filter number? operands))) 
;                                      ((translate (car subexp)) 0 (car (filter number? subexp))))
;                                  ,(car (filter not-number? (cdr subexp)))))))
;                 
;                 ([and (eq? '* procedure) (ormap number? operands) (ormap One? operands)]  (car (filter (lambda (x) (not (One? x))) operands)))
;                 (else tree))
;               tree))
;          (else tree))))
