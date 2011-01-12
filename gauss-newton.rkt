#lang racket
(require "pattern-matcher.rkt" ;we use bind utility from this module
         "simplifier.rkt"
         "list-matrix.rkt"
         "genetic-programming.rkt")
(provide (all-defined-out))

;Gauss-Newton algorithm

; expression -> (list expression environment)
; selects all coefficients in expr and assigns them symbolic names
; generates environment (assoc of symbols and values)
(define (name-coeffs expr)
    ;flattens response from coeff
    (define (env-flatten env)
      (define (env-pair? x)
        (and (pair? x) (atom? (car x)) (atom? (cdr x))))
      (cond [(null? env) null]
            [(env-pair? (car env)) (cons (car env) (env-flatten (cdr env)))]
            [else (append (env-flatten (car env)) (env-flatten (cdr env)))]))
  
    (define (coeff expr env)
      (cond [(null? expr) (list expr null)]
            [(number? expr) (let ([symbol (gensym 'c)]) (list symbol (cons symbol expr)))]
            [(atom? expr) (list expr null)]          
            [(list? expr) (let ([ret (map (λ(e) (coeff e env)) expr)]) (list (map car ret)                               
                                                                                   (map cadr ret)))]))
  (let ([resp (coeff expr null)])
    (list (car resp)
          (env-flatten (cdr resp)))))

(define (partial-diff expr symbols)
  (map (λ(symbol) (D symbol expr)) symbols))

(define (generate-functions expr symbols data)
  (define (make-env symbols datum) (map cons symbols datum))
  (map (λ(datum) (bind expr (make-env symbols datum)))
       data))
;expr vars data -> (env expr J)
(define (generate-jacobian expr vars-in-data data)
  (let* ([res (name-coeffs expr)]
         [expression (first res)]
         [env (second res)]
         [dat (let ([n (sub1 (length (first data)))]) (map (λ(x) (take x n)) data))]
         [exprs (generate-functions expression vars-in-data dat)])
    (list env expression
          (map (λ(expr) (partial-diff expr (map car env))) exprs))))

(define (gauss-newton expr vars-in-data data num-iter)
  ;calculates residuals
  (define (residuals expr vars-in-data data)
    (transpose 
     (list 
      (map - (map 
              (λ(x) (apply (code->function expr vars-in-data) x))
              (map (λ(x) (take x (sub1 (length x)))) data))
           (map last data)))))
  
  (if (zero? num-iter) expr
      (let* ([Je/e       (generate-jacobian expr vars-in-data data)]
             [J          (third Je/e)]
             [expression (second Je/e)]
             [env        (first Je/e)]
             [beta       (map car env)]
             [beta0      (map cdr env)]
             [r          (residuals expr vars-in-data data)]
             [Jf         (matrix-map J (λ (expr) (simplify (bind expr env))))]
             [Jf\'*Jf    (multiply (transpose Jf) Jf)]
             [delta      (multiply (multiply (inverse Jf\'*Jf) (transpose Jf)) r)]
             [beta1      (matrix-map (sub-elems (map list beta0) delta) exact->inexact)])
        (display "MSE:") (display (exact->inexact (/ (apply + (map sqr (map car r))) (length (map car r)))))
        (newline) (display "---") (newline)
        (let ([e (bind expression (map cons beta (map car beta1)))])
          (display e)(newline)
          (gauss-newton 
           e
           vars-in-data
           data
           (sub1 num-iter))))))


;TEST CASES
(define ex '(+ (+ (* 20 (* x x)) (* 6 x)) (/ 2 (+ x 2))))

(define X (build-list 20 values))
(define Y (build-list 20 (λ(x) (+ (+ (* 5 (* x x)) (* 3 x)) (/ 1 (+ x 1))))))

;(gauss-newton ex '(x) (map list X Y) 20)


;1d-structural 150gen score 0.009004204992428044 (stuck)
;(define ex '(/ (/ (- h (- (+ (+ h (+ (+ (- (* h h) h) (+ h (/ h (/ 0.0007284950162342976 h)))) (- h (* (* h h) h)))) (- h h)) (* (* (+ h h) h) h))) (* (- (- (log 0.003131963321612487) h) h) h)) (+ (+ (log h) (/ h h)) (+ (log h) (/ h (+ h (- h (/ 0.0007264220029854186 h))))))))


;(define data (with-input-from-file "1d.list" read))

;(gauss-newton ex '(x) (map list X Y) 20)
