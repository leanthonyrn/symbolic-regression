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
  (display "GNA got:")(display expr)(newline)
  (define (residuals expr vars-in-data data)
    (transpose 
     (list 
      (map - (map 
              (λ(x) (apply (code->function expr vars-in-data) x))
              (map (λ(x) (take x (sub1 (length x)))) data))
           (map last data)))))
  
  (if (or (zero? num-iter) (not (list? expr))) expr
      (let* ([Je/e       (generate-jacobian expr vars-in-data data)]
             [J          (third Je/e)]
             [expression (second Je/e)]
             [env        (first Je/e)]
             [beta       (map car env)]
             [beta0      (map cdr env)]
             [r          (residuals expr vars-in-data data)]
             [Jf         (matrix-map J (λ (expr) (simplify (bind exp
;TEST CASES
(define ex '(+ (+ (* 20 (* x x)) (* 6 x)) (/ 2 (+ x 2))))

;(define X (build-list 20 values))
;(define Y (build-list 20 (λ(x) (+ (+ (* 5 (* x x)) (* 3 x)) (/ 1 (+ x 1))))))

;(gauss-newton ex '(x) (map list X Y) 20)

(define data (with-input-from-file "input/erfg.list" read))

(define f10 '(-
  (+
   x
   (/
    (- (- (- x 0.5957344872673912) 1.369766971410876) (+ (+ 0.9481347187450206 (* x x)) (+ 0.3687179649000375 x)))
    0.31383183651534424))
  1.369766971410876))

(define f20 '(/
  (/
   (-
    (+
     0.16139188977998395
     (+ x (/ (/ (- (- x 0.577975238693573) x) (+ (+ 0.9481347187450206 (* x x)) x)) 0.07770404452021121)))
    (+ (* x x) (+ 0.1720107710907883 x)))
   0.31383183651534424)
  -0.8083944613216025))

(define f30 '(+
              (-
               (+
                10.224317978760634
                (* 4 (+ x (* x x)))) x)
              (/ x (*
                    (+ (- x (/ x (- (* x x) x))) (* (- (/ x 0.528219042548342) (+ -0.2711076618198814 x)) x))
                    (/ x (- (- (* x x) (* 2 x)) x))))))

(define f40 '(+
  (- (+ 7.406900966677131 (* 4 (+ x (* x x)))) x)
  (/
   x
   (*
    (+ (- x (/ x (- (* x x) x))) (* (- (/ x 0.528219042548342) (+ -0.26874194005559054 x)) x))
    (/ x (- (- (- 0 (* 2 x)) (* 2 x)) x))))))

(define f60 '(+
  (- 26.4829448053532 x)
  (+ x (* 5.9725230258214035 (/ x (- (+ -3.8361041820397768 (/ (* 2 x) (- 1.9577546704590723 x))) x))))))

(define f80 '(+
  (+ 22.47263310937463 (- (- 0.039960084014423136 x) x))
  (+ x (* 5.765021553062702 (/ x (- (+ -3.8240310836872817 (/ (* 2 x) 6.847779709367281)) x))))))

(define f100 '(+
  (-
   23.806442773922186
   (- x (+ x (* 5.9725230258214035 (/ x (- (+ -3.8240310836872817 (/ (* 2 x) 6.847779709367281)) x))))))
  (+ x (* 5.9725230258214035 (/ x (- (+ -2.608853523962392 (/ (* 2 x) 1.3897018151814922)) x))))))


;---------------------------new attempt --------------------------

(define F10 '(- (* (- (* x x) x) (- (+ (/ (* 2 x) 0.732425485816363) (+ (* 2 x) (* x x))) x)) (+ -0.5840011426415849 x)))
(define F20 '(* x (+ (* (* (+ (* 0.2734836053167204 x) (+ 0.36759436125717176 (* 0.7089686411119712 x))) x) (+ (/ x (/ (* x x) (* 2 x))) x)) x)))

(define F30 '(* x (+ (* (* (+ (* 0.2734836053167204 x) (+ 0.36759436125717176 (* 0.7089686411119712 x))) x) (+ (/ x (/ (* x x) (* 2 x))) x)) x)))

(define F40 '(* x (+ (* (* (+ (* 0.2734836053167204 x) (+ 0.36759436125717176 (* 0.7089686411119712 x))) x) (+ (/ x (/ (* x x) (* 2 x))) x)) x)))

(define F50a '(+ (* 0.9824522464286916 (* x (* x (* x x)))) (+ (* 2.332498854114555 (* x (* x x))) (* 1.7351887225143434 (* x x)))))

(define erf25 '(exp
  (*
   0.48032129157959225
   (-
    (* 0.12103046832964914 (* (/ 1.0302623641202422 (exp (exp (* x x)))) (* (/ 0.37008855033458093 x) (+ (sqrt x) x))))
    (/ 2.7247304161140207 (* (sqrt (exp (/ (+ (* x x) (exp x)) 1))) (sqrt x)))))))

(define erf26 '(exp
  (*
   0.48032129157959225
   (-
    (* 0.12103046832964914 (* (/ 1.0302623641202422 (exp (exp (* x x)))) (* (/ 0.37008855033458093 x) (+ (sqrt x) x))))
    (/ 2.7247304161140207 (* (sqrt (exp (/ (+ (* x x) (exp x)) 1))) (sqrt x)))))))

(define erf37simple '(exp (/ -1 (sqrt (* x (exp (+ (* x x) (exp x))))))))



(gauss-newton erf37simple '(x) data 20)

r env))))]
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

