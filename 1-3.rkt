#lang sicp

(define (sumOfSquares a b ) 
    (+ (* a a)(* b b)))

(define (sumOfSquares a b c) 
    (cond 
        ((and (>= a c) (>= b c) (sumOfSquares a b)))
        ((and (>= a b) (>= c b) (sumOfSquares a c)))
        ((and (>= b a) (>= c b) (sumOfSquares b c)))
    )    
)    
