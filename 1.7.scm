(include "core.scm")

(define (sqrt-iter guess x)
   (if (good-enogh? guess x)
       guess
       (sqrt-iter (improve guess x)
                   x)))

(define (improve guess x)
        (average guess (/ x guess)))

(define (average x y)
        (/ (+ x y) 2))

; (define (good-enogh? guess x)
;         (< (abs (- (square guess) x)) 0.001))

;alternarive good-enough
(define (good-enogh? guess x)
        (< (delta (improve guess x) guess) 0.001))

(define (sqrt x)
        (sqrt-iter 1.0 x))