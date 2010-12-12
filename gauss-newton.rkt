#lang racket
(require "pattern-matcher.rkt") ;we use bind utility from this module
(require "simplifier.rkt")

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
;expr vars data -> J
(define (generate-jacobian expr vars data)
  (let* ([res (name-coeffs expr)]
         [expression (first res)]
         [env (second res)]
         [exprs (generate-functions expression vars data)])
    (map (λ(expr) (partial-diff expr (map car env))) exprs)))



(define (print-matrix M)
  (define (println lst)
    (map (λ(x) (display x) (display "\t\t")) lst)
    (newline))
  (map println M) #t)
