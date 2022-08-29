(define (p) (p))

(define (test x y)
    (if (= x 0)
        0
        y))

(test 0 (p))

;applicative order - procedure can be evaluated (stuck trying to eval (p))
;normal order - procedure should return 0 
;so scheme interpreter using applicative order        