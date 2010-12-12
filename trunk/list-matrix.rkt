#lang racket
(define (matrix . columns) 
  (if (apply = (map length columns))
      columns
      (error "all cols must be of same size")))

(define (build-matrix m n [proc (λ(x y) 0)])
  (build-list m (λ(x) (build-list n (λ(y) (proc x y))))))

(define (matrix-dim matrix)
  (list (length (first matrix)) (length matrix)))

(define (matrix-map matrix proc)
  (map (λ(x) (map proc x)) matrix))

(define (matrix-ref matrix i j)
  (if (or (list? i) (list? j))
      (map (λ(lst) (list-slice lst j)) (list-slice matrix i))
      (list-ref (list-ref matrix i) j)))

(define (list-slice lst slice)
  (define (remove-at-index lst index)
    (define (rem lst i drop)
      (cond [(null? lst) '()]
            [(= i drop) (cdr lst)]
            [else (cons (car lst) (rem (cdr lst) (add1 i) drop))]))
    (rem lst 0 index))
    (let ([sl (if (number? slice) (list slice slice) slice)])
        (let ([len (length lst)]
              [start (first sl)]
              [end   (second sl)])
          (if (eq? 'not start)             
              (remove-at-index lst end)
              (let ([head (if (eq? start 'start)  0  start)]
                    [tail (if (eq? end   'end)   (sub1 len) end)])
                (take (drop lst head) (add1 (- tail head))))))))
  
(define (transpose matrix)
  (let ([dim (matrix-dim matrix)])
    (build-matrix (first dim) (second dim)
                  (λ(x y) (matrix-ref matrix y x)))))

(define (scale scalar matrix)
  (matrix-map matrix (λ (x) (* scalar x))))

(define (multiply A B)
  (map [λ(Col) (map [λ(Row) (apply + (map * Col Row))] (transpose B))] A))

(define (det A)
  (define (singleton? x) (and (list? x) (= 1 (length x)) (= 1 (length (car x)))))
  (if (singleton? A) (caar A)
      (let ([dim (matrix-dim A)]
            [coeffs (map first A)])
        (if (not (apply = dim)) 
            (error "must be a square matrix, given matrix of size" dim)
            (let loop ([i 0] [accum 0])
              (if (= i (second dim)) accum
                  (loop (add1 i) 
                        (+ accum 
                           (* (if (even? i) 1 -1) 
                              (list-ref coeffs i)
                              (det (matrix-ref A (list 'not i) '(1 end))))))))))))

(define (inverse A)
  (let ([dim (matrix-dim A)]
        [determinant (det A)])
    (if (zero? determinant) #f
        (transpose (scale (/ 1 determinant)
                          (build-matrix (first dim) (second dim)
                                        (λ (i j) (*
                                                  (if (even? (+ i j)) 1 -1)
                                                  (det (matrix-ref A (list 'not i) (list 'not j)))))))))))

(define (matrix-print matrix)
  (map (λ(Col) (map (λ(x) (display (real->decimal-string x 2)) (display "\t")) Col) (newline)) matrix)
  (void))