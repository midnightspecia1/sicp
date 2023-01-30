(include "core.scm")

;lexical scoping
(define (qdrt x)
    (define (qdrt-iter guess)
        (if (good-enogh? guess)
        guess
        (qdrt-iter (improve guess))))
    (define (improve guess)
        (third (+ (/ x
                    (* guess guess))
                  (* 2 guess))))
    (define (average x y)
        (/ (+ x y) 2))
    (define (good-enogh? guess)
        (< (delta (improve guess) guess) 0.001))
    (qdrt-iter 1.0))











