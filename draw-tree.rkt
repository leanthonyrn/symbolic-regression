#lang racket
(require 2htdp/image)

(define size (make-parameter 10))
(define variables (make-parameter '(x y z)))

(define (draw-node node)
  (cond ((null? node) (rectangle (* 3 (size)) (* 3 (size)) "outline" "white"))
        ((symbol? node)
         (if (member node (variables))
             (overlay (rectangle (* 3 (size)) (* 2 (size)) "outline" "black") (text (symbol->string node) (size) "black"))
             (overlay (circle (* 1.5 (size)) "outline" "black") (text (symbol->string node) (size) "black"))))
        ((number? node) (overlay (rectangle (* 4 (size)) (* 2  (size)) "outline" "black") (text (real->decimal-string node 2) (size) "black")))
        (else (error "unsupported node"))))

(define (draw-tree tree depth)
  (let ([s [* 2 (size)]])
    (if (list? tree)
        (above (draw-node (list-ref tree 0))
               (beside (line (* (expt 2 depth) (- s)) s "black") (rectangle s s "solid" "white") (line (* s (expt 2 depth)) s "black"))
               (beside (draw-tree (list-ref tree 1) (sub1 depth)) (rectangle s s "solid" "white") (draw-tree (list-ref tree 2) (sub1 depth))))
        (draw-node tree))))
  


