(include "~/Projects/sicp/core.scm")

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;(+ 4 5)
;first case
;(inc (+ 3 5))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9
;recursive process 


;second case
;(+ (dec 4) (inc 5))
;(+ (dec 3) (inc 6))
;(+ (dec 2) (inc 7))
;(+ (dec 1) (inc 8))
;(+ 0 9)
;9
;iterative process

;1.10
(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(define (f n) (A 0 n)) ;2n
(define (g n) (A 1 n)) ;2^n
(define (h n) (A 2 n)) ;2^h(n-1)
(define (k n) (* 5 n n)) ;5n^2
	 
;;Counting change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

;;tail recursion variant - above should be optimized something like this
(define (cc-tail amount kinds-of-coins acc)
  (cond ((= amount 0) (+ acc 1))
	((< amount 0) acc)
	(else (if (= kinds-of-coins 0)
		  acc
		  (cc-tail (- amount (first-denomination kinds-of-coins))
			   kinds-of-coins
			   (cc-tail amount
				    (- kinds-of-coins 1)
				    acc))))))

(define (first-denomination kind-of-coins)
  (cond ((= kind-of-coins 1) 1)
	((= kind-of-coins 2) 5)
	((= kind-of-coins 3) 10)
	((= kind-of-coins 4) 25)
	((= kind-of-coins 5) 50)))

(define (logged-cc amount kinds-of-coins)
  (display amount) (newline)
  (cc amount kinds-of-coins))

;;(count-change 100)
