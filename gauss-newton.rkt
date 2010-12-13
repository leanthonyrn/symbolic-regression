#lang racket
(require "pattern-matcher.rkt") ;we use bind utility from this module
(require "simplifier.rkt")
(require "list-matrix.rkt")
(require "genetic-programming.rkt")
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
         [exprs (generate-functions expression vars-in-data data)])
    (list env expression
          (map (λ(expr) (partial-diff expr (map car env))) exprs))))

(define (gauss-newton expr vars-in-data data num-iter)
  ;calculates residuals
  (define (residuals expr vars-in-data data)
    (map (code->function expr vars-in-data)))
  (if (zero? num-iter) expr
      (let* ([Je/e       (generate-jacobian expr vars-in-data data)]
             [J          (first Je/e)]
             [expression (second Je/e)]
             [env        (third Je/e)]
             [beta       (map car env)]
             [beta0      (map cdr env)]
             [r          (residuals expr vars-in-data data)]
             [Jf         (matrix-map (λ (expr) (simplify (bind expr env))))]
             [Jf\'*Jf    (multiply (transpose Jf) Jf)]
             [delta      (multiply (multiply (inverse Jf\'*Jf) Jf) r)]
             [beta1      (map + beta0 delta)])
        (gauss-newton 
         (bind expression (map cons beta beta1))
         vars-in-data
         data
         (sub1 num-iter)))))

;test
(define g1000 (with-input-from-file "generations/1009.txt" read))
(define ex '(/ (/ h 0.00016132276460659387) (* (- (log 1.5598753370524297e-24) h) h)))

(define data (with-input-from-file "1d.list" read))

;(define Je/e (parameterize ([pre-eval-inspector translate])
;    (generate-jacobian '(* (- (rlog x) (* 3 x))) '(h) (map list (map second data)))))
;test
(define Je (parameterize ([pre-eval-inspector translate])
             (generate-jacobian ex '(h) (map list (map second data)))))

(define JJ (matrix-map (third Je)
                       (λ (expr)
                         (parameterize ([pre-eval-inspector translate])
                           (simplify (bind expr (first Je)))))))

(define M (multiply (inverse (multiply (transpose JJ) JJ)) (transpose JJ)))