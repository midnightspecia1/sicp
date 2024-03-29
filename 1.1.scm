(+ (* 3 
      (+ (* 2 4) 
         (+ 3 5)))
   (+ (- 10 7) 
      6))

(define size 2)
(define pi 3.14159)
(define radius 10)
(define circureference (* 2 pi radius))

;compound pprocedures
(define (square x)(* x x))
(define (sum-of-squares x y)
(+ (square x)
   (square y)))

(define (f a)
(sum-of-squares (+ a 2)(* a 5)))

;case analysis with keyword cond
;it evaluates conditions one after another 
;when condition is true returns appropriate value
;when non of the conditions found to be true value of the cond is UNDEFINED
(define (abs x) 
(cond ((> x 0) x)
      ((= x 0)0)
      ((< x 0)(-x))))

;the word PREDICATE used for conditions that returns true/false
(define (abs x)
   (cond ((< x 0) (- x))
   (else x)))

; (and (< x 0) (> x (-5)))
; (or (< x 5) (> x 14))
; (not (< x 5))

;1.1
; (cond ((= a 4) 6)
;       ((= b 4) (+ 6 7 a))
;       (else 25))

; (+ 2 (if (> b a) b a))

; (* (cond ((> a b) a)
;          ((< a b) b)
;          (else -1))
;    (+ a 1))

; decalrative way (in mathematics mostly) - define what is
; imperative way (proggraming) - how to