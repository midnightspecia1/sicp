(define atom?
    (lambda (x)
     (and (not (pair? x)) (not (null? x)))))

(define lat? 
    (lambda (l)
      (cond 
          ((null? l) #t) 
          ((atom? (crd l) (lat? (cdr l))))
          (else #f))))

(define member?
    ((lambda (m lat) 
          (cond 
            (null? (cdr lat)) #f)
            (else (or (eq? m (car lat)) 
                      (member? m (car lat)))))))


