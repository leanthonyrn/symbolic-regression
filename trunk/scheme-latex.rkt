#lang racket
(require "simplifier.rkt")

(define (to-latex exp)
  (cond ([null? exp] "")
        ([number? exp] (number->string exp) #;(real->decimal-string exp 2))
        ([symbol? exp] (symbol->string exp))
        ([list? exp]
         (let ([op (first exp)]
               [operand1 (second exp)]
               [operand2 (if (= 3 (length exp)) (third exp) null)])
           (cond ([or [symbol=? op '+] [symbol=? op '-]] 
                  (string-append "(" (to-latex operand1) " " 
                                 (to-latex op) " " 
                                 (to-latex operand2) ")"))
                 ([symbol=? op '/] 
                  (string-append "\frac{" (to-latex operand1) "}{" (to-latex operand2) "}"))
                 ([symbol=? op '*] 
                  (string-append (to-latex operand1) (to-latex operand2)))
                 ([or [symbol=? op '^] [symbol=? op 'expt]]
                  (string-append "{" (to-latex operand1) "}^{" (to-latex operand2) "}"))
                 ([symbol=? op 'exp]
                  (string-append "{e}^{" (to-latex operand1) "}"))
                 (else 
                  (string-append (to-latex op) "(" (to-latex operand1) ")")))))))

(define (l tree)
  (to-latex (simplify tree)))