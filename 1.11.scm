;; (define (fib n)
;;   (cond ((= n 0) 0)
;; 	((= n 1) 1)
;; 	(else (+ (fib (- n 1))
;; 		 (fib (- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;;1.11
;; recursive process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (+ (* (f (- n 2))
	       2)
	    (* (f (- n 3))
	       3)))))

;;f(n-1) + 2f(n-2) + 3f(n-3) n=5
;;  f(4) + 2f(3) + 3f(2) =

;;= f(3) + 2f(2) + 3f(1) +
;;+ 2(f(2) + 2f(1) + 3f(0)) +
;;+ 3(f(2)) =

;;= f(2) + 2f(1) + 3f(0) + 4 + 3 +
;;+ 2(2 + 2 + 0) +
;;+ 6 =

;;= 2 + 2 + 0 + 4 + 3 +
;;+ 4 + 4 + 0 +
;;+ 6 =

;;= 11 + 8 + 6 =
;;= 25

;; iterative process
(define (fi n count)
  (if (< n 3)
      n
      (if ())))
      
