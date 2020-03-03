#lang sicp

(define (abs x)
    (if (< x 0) 
        (- x) 
            x))

(define (square x)
    (* x x))

(define (good-enough? guess x)
    (< (abs (-(square guess) x)) 0.001))

(define (average x y)
    (/ (+ x y) 2))

(define (improve guess x)
    (average guess (/ x guess)))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                    x)))


; http://community.schemewiki.org/?sicp-ex-1.6
#| 
    New if is a procedure. Procedures use applicative order evaluation.
    That way the predicate, then and else-clause are evaluated first.
    When this is called from the sqrt-iter2 and we pass the else-clause we never stop.
    It seems that if statement is a special form that only evaluate the else part if the predicate is false

    About the if
    To evaluate an if expression, the interpreter starts by evaluating the
    〈predicate〉 part of the expression. If the 〈predicate〉 evaluates to a true
    value, the interpreter then evaluates the 〈consequent〉 and returns its
    value. Otherwise it evaluates the 〈alternative〉 and returns its value.
|#
(define (new-if predicate then else-clause)
    (cond (predicate then)
        (else else-clause)))

(define (sqrt-iter2 guess x)
    (new-if (good-enough? guess x)
        guess
        (sqrt-iter2 (improve guess x)
                    x)))        


; This works acording with page  52
#|
    Conditional expressions are evaluated as follows. The predicate 〈p1 〉 is
    evaluated first. If its value is false, then 〈p2 〉 is evaluated. If 〈p2 〉’s value is
    also false, then 〈p3 〉 is evaluated. This process continues until a predicate
    is found whose value is true, in which case the interpreter returns the value
    of the corresponding consequent expression 〈e 〉 of the clause as the value
    of the conditional expression. 
|#
(define (sqrt-iter3 guess x)
    (cond((good-enough? guess x) guess)
         ((= 1 1) (sqrt-iter3 (improve guess x) x))))
  

; This not work. Fails exactly as newif. Apparentyly event if we are outside of a Procedure
; the applicative order evaluation is applied to the whole expression. In this case the else part belongs
; to the cond expression since it works for the same predicate.
(define (sqrt-iter4 guess x)
    (cond ((good-enough? guess x) guess)
        (else (sqrt-iter4 (improve guess x) x))))
  
                            