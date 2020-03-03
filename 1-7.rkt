#lang sicp

(define (abs x)
    (if (< x 0) 
        (- x) 
            x))

(define (square x)
    (* x x))

(define (good-enough? guess x)
    (< (abs (- 1 (/ (improve guess x) guess))) 0.001))

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                    x)))

(sqrt-iter 1.0 10000000000000)