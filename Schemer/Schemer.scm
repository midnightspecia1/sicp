(define atom?
    (lambda (x)
     (and (not (pair? x)) (not (null? x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; The First Comandment
;;;;; Always ask null? as the first question in expressing any function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define member?
    (lambda (a lat)
        (cond
            ((null? lat) #f)
            (else (or (eq? (car lat) a)
                    (member? a (cdr lat)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; The Second Comandment
;;;;; use cons to build lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rember
    (lambda (a lat)
        (cond ((null? lat) '())
              ((eq? (car lat) a) (cdr lat))
              (else (rember a (cdr lat))))))

(define remberC
    (lambda (a lat)
        (cond ((null? lat) '())
              ((eq? (car lat) a) (cdr lat))
               (else (cons (car lat)
                        (rember a (cdr lat)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; The Thierd Comandment
;;;;; When building a list decribe the first typical element,
;;;;; and then cons it onto the natural recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define multirember
    (lambda (a lat)
        (cond ((null? lat) '())
              ((eq? (car lat) a) (multirember a (cdr lat)))
               (else (cons (car lat)
                        (multirember a (cdr lat)))))))

(define multiinsertR
    (lambda (new old lat)
        (cond ((null? lat) '())
              ((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
              (else (cons (car lat) (multiinsertR new old (cdr lat)))))))


(define multiinsertL
    (lambda (new old lat)
        (cond ((null? lat) '())
              ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
              (else (cons (car lat) (multiinsertL new old (cdr lat)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; The Fourth Comandment
;;;;; always change at least one argument while reccurring.
;;;;; It must be changed to be closer to termination.
;;;;; The changing argument must be tested in the terminal condition
;;;;; and then cons it onto the natural recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define insertR
    (lambda (new old lat)
        (cond ((null? lat) '())
              ((eq? (car lat) old) (cons old (cons new (cdr lat))))
              (else (cons (car lat)
                          (insertR new old (cdr lat)))))))

(define insertL
    (lambda (new old lat)
        (cond ((null? lat) '())
              ((eq? (car lat) old) (cons new lat))
              (else (cons (car lat)
                          (insertL new old (cdr lat)))))))

(define subst
    (lambda (new old lat)
        (cond ((null? lat) '())
              ((eq? (car lat) old) (cons new (cdr lat)))
              (else (cons (car lat)
                          (insertR new old (cdr lat)))))))


(define subst2
    (lambda (new o1 o2 lat)
        (cond ((null? lat) '())
              ((or (eq? (car lat) o1) (cons new (cdr lat))
                   ((eq? (car lat) o2) (cons new (cdr lat)))))
              (else (cons (car lat)
                          (subst2 new o1 o2 (cdr lat)))))))

(define multisubst
    (lambda (new old lat)
        (cond ((null? lat) '())
              ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
              (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define first
    (lambda (l)
        (cond 
            ((null? l) '())
            (else (cons (car (car l)) (first (cdr l)))))))

(define lat? 
    ())

;;;;;;;;;;;;;;;;;;;;;
;;;;; The law of car
;;;;; the primitive car is defined only for non empty lists
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;;;;; The law of cdr
;;;;; The primitive cdr is defined only for non-empty lists.
;;;;; The cdr of any non-empty lists is always another list 
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;;;;; The law of cons
;;;;; The primitive cons takes two arguments.
;;;;; The second argument must be a list. the result is a list
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;;;;; The law of null?
;;;;; The primitive null? is defined only for lists
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;;;;; The law of eq?
;;;;; The primitive eq? takes two arguments. Each must be a non-numeric atom
;;;;;;;;;;;;;;;;;;;;;

;; CHAPTER 4 NUMBER GAMES

(define add1
    (lambda (n)
        (+ n 1)))

(define sub1
    (lambda (n)
        (- n 1)))

(define o+
    (lambda (a b)
        (cond ((zero? b) a)
              (else (add1 (o+ a (sub1 b)))))))

(define o-
    (lambda (a b)
        (cond ((zero? b) a)
              (else (sub1 (o- a (sub1 b)))))))

(define addtup
    (lambda (tup)
        (cond ((null? tup) 0)
              (o+ (car tup) (addtup (cdr tup))))))

(define o*
    (lambda (a b)
        (cond ((zero? b) 0)
              (else (o+ a (o* a (sub1 b)))))))              

(define o^
    (lambda (a b)
        (cond ((zero? b) 1)
              (else (o* a (o^ a (sub1 b)))))))

(define tup+
    (lambda (tup1 tup2)
        (cond ((null? tup1) tup2)
              ((null? tup2) (tup1))
              (else (cons (o+ (car tup1) (car tup2))
                          (tup+ (cdr tup1) (cdr tup2)))))))

(define greater
    (lambda (a b)
        (cond ((zero? a) #f)
              ((zero? b) #t)
              (else (greater (sub1 a) (sub1 b))))))

(define lesser
    (lambda (a b)
        (cond ((zero? b) #f)
              ((zero? a) #t)
              (else (lesser (sub1 a) (sub1 b))))))              

(define equal
    (lambda (a b)
        (cond ((and (zero? a)(zero? b) #t))
              ((zero? a) #f)
              (else (equal (sub1 a) (sub1 b))))))

(define o/
    (lambda (a b)
        (cond ((lesser a b) 0)
              (else (add1 (o/ (o- a b) b))))))

(define len
    (lambda (lat)
        (cond ((null? lat) 0)
              (else (add1 (len (cdr lat)))))))

(define pick
    (lambda (n lat)
        (cond (zero? (sub1 n) (car lat))
              (else (pick (sub1 n) (cdr lat))))))

(define rempick
    (lambda (n lat)
        (cond ((one? n) (cdr lat))
              (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
    (lambda (lat)
        (cond ((null? lat) '())
              ((number? (car lat)) (no-nums (cdr lat)))
              (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
    (lambda (lat)
        (cond ((null? lat) '())
              ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
              (else (all-nums (cdr lat))))))

(define eqan?
    (lambda (x y)
        (cond ((and (number? x)(number? y) (equal x y)))
              ((or (number? x) (number? y)) #f)
              (else (eq? x y)))))

(define occur
    (lambda (x lat)
        (cond ((null? lat) 0)
              ((eqan? x (car lat)) (add1 (occur x (cdr lat))))
              (else (occur x (cdr lat))))))

; (define one?
;     (lambda (x)
;         (cond ((zero? x) #f)
;               (else (zero? (sub1 n))))))

(define one?
    (lambda (x)
        (equal x 1)))

;;;;; CHAPTER 5
(define rember* 
    (lambda (a lat)
        (cond ((null? lat) '())
              ((atom? (car lat))
               (cond ((eq? (car lat) a)
                      (rember* a (cdr lat)))
                     (else (cons (car lat)
                                 (rember* a (cdr lat))))))
              (else (cons (rember* a (car lat))
                          (rember* a (cdr lat)))))))

                          
(define insertR*
    (lambda (new old l)
        (cond ((null? l) '())
              ((atom? (car l)) 
                (cond ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
                      (else (cons (car l) (insertR* new old (cdr l))))))
              (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))                          

(define occur*
    (lambda (a l)
        (cond ((null? l) 0)
              ((atom? (car l))
                (cond ((eq? a (car l)) (add1 (occur* a (cdr l))))
                      (else (occur* a (cdr l)))))
              (else (o+ (occur* a (car l))
                         (occur* a (cdr l)))))))

(define subst*
    (lambda (new old l)
        (cond ((null? l) '())
              ((atom? (car l))
                (cond ((eq? old (car l)) 
                            (cons new (subst* new old (cdr l))))
                      (else (cons (car l) (subst* new old (cdr l))))))
              (else (cons (subst* new old (car l))
                          (subst* new old (cdr l)))))))

(define insertL*
    (lambda (new old l)
        (cond ((null? l) '())
              ((atom? (car l))
                (cond ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
                      (else (cons (car l) (insertL* new old (cdr l))))))
              (else (cons (insertL* new old (car l))
                          (insertL* new old (cdr l)))))))

(define member*
    (lambda (a l)
        (cond ((null? l) #f)
              ((atom? (car l))
                (or (eq? a (car l)) 
                    (member* a (cdr l))))
              (else (or (member* a (car l))
                        (member* a (cdr l)))))))

(define leftmost
    (lambda (l)
        (cond ((atom? (car l) (car l)))
              (else (leftmost (car l))))))

(define and-
    (lambda (a b)
        (cond (a b)
              (else #f))))

(define or-
    (lambda (a b)
        (cond (a #t)
              (else b))))

;; first version of the eqlist?
#| (define eqlist?
    (lambda (l1 l2)
        (cond 
            ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            ((and (atom? (car l1)) (atom? (car l2)))
             (and (eqan? (car l1) (car l2))
                  (eqlist? (cdr l1) (cdr l2))))
            ((atom? (car l1)) #f)
            ((null? l2) #f)
            ((atom? (car l2)) #f)
            (else (and (eqlist? (car l1) (car l2))
                       (eqlist? (cdr l1) (cdr l2)))))))
 |#

(define eqlist?
    (lambda (l1 l2)
        (cond 
            ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            (else (and (equal? (car l1) (car l2))
                       (equal? (cdr l1) (cdr l2)))))))

(define equal?
    (lambda (s1 s2)
        (cond 
            ((and (atom? s1) (atom? s2))(eqan? s1 s2))
            ((or (atom? s1) (atom? s2)) #f)
            (else (eqlist? s1 s2)))))                       

(define remberS
    (lambda (s l)
        (cond ((null? l) '())
              ((equal? (car l) s) (cdr l))
              (else (cons (car l)
                          (remberS s (cdr l)))))))

#| (define numbered?
    (lambda (aexp)
        (cond ((atom? aexp) (number? aexp)) 
              ((eq? (car (cdr aexp)) (quote +)) 
                    (and (numbered? (car aexp)) 
                         (numbered? (car (cdr (cdr aexp)))))) 
              ((eq? (car (cdr aexp)) (quote x)) 
                    (and (numbered? (car aexp)) 
                         (numbered? (car (cdr (cdr aexp)))))) 
              ((eq? (car (cdr aexp)) (quote ↑))
                    (and (numbered? (car aexp)) 
                         (numbered? (car (cdr (cdr aexp)))))))))
 |#                        
;we can write it simplier

(define numbered?
    (lambda (aexp)
        (cond ((atom? aexp) (number? aexp)) 
              (else (and (numbered? (car aexp)) 
                         (numbered? (car (cdr (cdr aexp)))))))))

#| (define value
    (lambda (nexp)
        (cond ((atom? nexp) nexp)
              ((eq? (car (cdr nexp)) (quote +))
                    (o+ (value (car nexp)) 
                        (value (car (cdr (cdr nexp))))))
              ((eq? (car (cdr nexp)) (quote x)) 
                    (o* (value (car nexp)) 
                        (value (car (cdr (cdr nexp))))))
              (else (o^ (value (car nexp)) 
                        (value (car (cdr (cdr nexp))))))))) |#

(define fst-sub-expr
    (lambda (aexp) (car (cdr aexp))))

(define snd-sub-expr
    (lambda (aexp) (car (cdr (cdr aexp)))))

(define operator
    (lambda (aexp)(car aexp)))

(define value
    (lambda (nexpr)
        (cond ((atom? nexpr) nexpr)
              ((eq? (operator nexpr) (quote +))
                        (o+ (value (fst-sub-expr nexpr))
                            (value (snd-sub-expr nexpr))))
              ((eq? (operator nexpr) (quote x))
                        (o* (value (fst-sub-expr nexpr))
                            (value (snd-sub-expr nexpr))))
              (else     (o^ (value (fst-sub-expr nexpr))
                            (value (snd-sub-expr nexpr)))))))

; lets assume different representations of the numbers
; zero would be (), one (()), two (() ()) etc.

(define sero?
    (lambda (n)(null? n)))

(define edd1
    (lambda (n)(cons n '())))

(define zub1
    (lambda (n)(cdr n)))
    
(define a+
    (lambda (a b)
        (cond ((sero? b) a)
              (else (edd1 (a+ a (zub1 b)))))))

;;7 FRIENDS AND RELATIONS

(define set?
    (lambda (lat)
        (cond ((null? lat) #t)
              ((member? (car lat) (cdr lat)) #f)
              (else (set? (cdr lat))))))

#| (define makeset
    (lambda (lat)
        (cond ((null? lat) lat)
              ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
              (else (cons (car lat) (makeset (cdr lat)))))))
 |#

 (define makeset
    (lambda (lat)
        (cond ((null? lat) '())
              (else (cons (car lat) 
                          (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
    (lambda (set1 set2)
        (cond ((null? set1) #t)
              ((member? (car set1) set2) (subset? (cdr set1) set2))
              (else #f))))

#| (define eqset? 
    (lambda (set1 set2)
        (cond ((subset? set1 set2) (subset? set2 set1))
              (else #f)))) |#

(define eqset?
    (lambda (set1 set2)
        (and (subset? set1 set2) (subset? set2 set1))))

(define intersect? 
    (lambda (set1 set2)
        (cond ((null? set1) #f)
              ((member? (car set1) set2) #t)
              (else (intersect? (cdr set1) set2)))))

(define intersect
    (lambda (set1 set2)
        (cond ((null? set1) '())
              ((member? (car set1) set2) 
                    (cons (car set1) (intersect (cdr set1) set2)))
              (else (intersect (cdr set1) set2)))))

(define union
    (lambda (set1 set2)
        (cond ((null? set1) set2)
              ((member? (car set1) set2)
                    (union (cdr set1) set2))
              (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
    (lambda (l-set)
        (cond ((null? (cdr l-set)) (car l-set))
              (else (intersect (car l-set)
                               (intersectall (cdr l-set)))))))

(define a-pair?
    (lambda (x)
        (cond ((atom? x) #f)
              ((null? x) #f)
              ((null? (cdr x)) #f)
              ((null? (cdr (cdr x))) #t)
              (else #f))))

;; (define first
;;     (lambda (pair)
;;         (car pair)))

(define firsts
    (lambda (pairs)
        (cond ((null? pairs) '()) 
              (else (cons (first (car pairs)) (firsts (cdr pairs)))))))

;; (define second
;;     (lambda (pair)
;;         (car (cdr pair))))
        
(define seconds
    (lambda (pairs)
        (cond ((null? pairs) '()) 
              (else (cons (second (car pairs)) (seconds (cdr pairs)))))))        

;; (define build
;;     (lambda (f s)
;;         (cons f (cons s '()))))

;; rel stands for relation
;; finite function is represented by the list of pairs with all diferent first elements
(define fun?
    (lambda (rel)
        (set? (firsts rel))))

#| (define revrel
    (lambda (rel)
        (cond ((null? rel) '())
              (else (cons (build (second (car rel)) 
                                 (first (car rel))) 
                          (revrel (cdr rel)))))))
 |#
(define revpair
    (lambda (pair)
        (build (second pair) (first pair))))

(define revrel
    (lambda (rel)
        (cond ((null? rel) '())
              (else (cons (revpair (car rel)) 
                          (revrel (cdr rel)))))))

(define fulfun?
    (lambda (fun)
        (set? (seconds fun))))

(define one-to-one
    (lambda (fun)
        (set? (revrel fun))))

;;CHAPTER 8 LAMBDA THE ULTIMATE
#| (define rember-f
    (lambda (test? a l)
        (cond ((null? l) '())
              ((test? (car l) a) (cdr l))
              (else (cons (car l) (rember-f test? a (cdr l)))))))
 |#

 (define rember-f
    (lambda (test?)
        (lambda (a l)
            (cond ((null? l) '())
                ((test? (car l) a) (cdr l))
                (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))

;; currying
(define eq?-c
    (lambda (a)
        (lambda (x)
            (eq? a x))))

(define eq?-salad
    (eq?-c 'salad))

(define insertL-f
    (lambda (test?)
        (lambda (new old lat)
              (cond ((null? lat) '())
                    ((test? (car lat) old) (cons new lat))
                    (else (cons (car lat)
                                ((insertL-f test?) new old (cdr lat))))))))

(define insertR-f
    (lambda (test?)
        (lambda (new old lat)
              (cond ((null? lat) '())
                    ((test? (car lat) old) (cons old (cons new (cdr lat))))
                    (else (cons (car lat)
                                ((insertR-f test?) new old (cdr lat))))))))

(define SeqR
    (lambda (new old lat)
            (cons old (cons new lat)))) 

(define SeqL
    (lambda (new old lat)
            (cons new (cons old lat))))

(define SeqS
    (lambda (new old lat)
            (cons new lat)))

(define SeqRem
    (lambda (new old lat)
            l))

(define insertG
    (lambda (test? seq)
        (lambda (new old lat)
            (cond ((null? lat) '())
                  ((test? (car lat) old) (seq new old (cdr lat)))
                  (else (cons (car lat)
                                ((insertG test?) new old (cdr lat) seq)))))))

(define insertL (insertG equal? seqL))

(define insertL (insertG equal? 
                         (lambda (new old lat) (cons new (cons old lat)))))

(define substS (insertG equal? SeqS))

(define remberS (insertG equal? SeqRem))

;; THE NINTH COMMANDMENT
;; abstract the common patterns with a new function

#| (define atom-to-function
    (lambda (a)
        (cond ((eq? a (quote +)) o+))))
              ((eq? a (quote x)) o*)
              (else o^)

(define valueN
    (lambda (nexp)
        (cond ((atom? nexp) nexp)
              (else ((atom-to-function (operator nexp))
                     (value (1st-sub-exp nexp))
                     (value (2nd-sub-exp nexp)))))))

(define multirember-f
    (lambda (test?)
        (lambda (a lat)
            (cond ((null? lat) '())
                  ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
                  (else (cons (car lat)
                              ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna (eq?-c (quote tuna)))

(define multiremberT
    (lambda (test?)
        (lambda (a lat)
            (cond ((null? lat) '())
                ((test? (car lat) a) (multiremberT test? (cdr lat)))
                (else (cons (car lat)
                            (multiremberT test? (cdr lat))))))))

;;;; collector
;;;;

(define multiremberCO
    (lambda (a lat col) ;; 'tuna '(and tuna)
        (cond ((null? lat) (col '() '()))
              ((eq? (car lat) a) (multiremberCO a (cdr lat) 
                                    (lambda (newlat seen)
                                        (col newlat (cons (car lat) seen)))))
              (else (multiremberCO a (cdr lat)
                                        (lambda (newlat seen)
                                            (col (cons (car lat) newlat) seen)))))))
 |#

(define a-friend
    (lambda (x y) (null? y)))

(define new-friend
    (lambda (newlat seen)
        (a-friend newlat (cons (quote tuna) seen))))

(define latest-friend
    (lambda (newlat seen)
        (a-friend (cons (quote and) newlat) seen)))

;; THE 10 COMMANDMENT 
;; Build functions to collect more that one value at a time
#| 
(define multiinsertLR
    (lambda (new oldL oldR lat)
        (cond ((null? lat) '())
              ((eq? (car lat) oldL) (cons new 
                                          (cons oldL 
                                                (multiinsertLR new oldL oldR (cdr lat)))))
              ((eq? (car lat) oldR)(cons oldR 
                                         (cons new 
                                               (multiinsertLR new oldL oldR (cdr lat))))))))
              (else (cons (car lat) 
                          (multiinsertLR new oldL oldR (cdr lat)))) |#


(define multiinsertR
    (lambda (new old lat)
        (cond ((null? lat) '())
              ((eq? (car lat) old) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
              (else (cons (car lat) (multiinsertR new old (cdr lat)))))))


(define multiinsertL
    (lambda (new old lat)
        (cond ((null? lat) '())
              ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
              (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

;;;;;;;;;;;;;;; initial col would be (lambda (lat L R) lat)
(define multiinsertLRC
    (lambda (new oldL oldR lat col)
        (cond ((null? lat) (col '() 0 0))
              ((eq? (car lat) oldL) (multiinsertLRC new oldL oldR (cdr lat) 
                                                    (lambda (newlat L R)
                                                        (col (cons new (cons oldL newlat)) (add1 L) R))))
              ((eq? (car lat) oldR) (multiinsertLRC new oldL oldR (cdr lat)
                                                    (lambda (newlat L R)
                                                        (col (cons oldR (cons new newlat)) L (add1 R)))))
              (else (multiinsertLRC new oldL oldR (cdr lat) (lambda (newlat L R) 
                                                                (col (cons (car lat) newlat) L R)))))))

(define evens-only*
    (lambda (l)
        (cond ((null? l) '())
              ((atom? (car l))
                  (cond ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
                        (else (evens-only* (cdr l)))))
              (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(define evens-only*C
    (lambda (l col)
        (cond ((null? l) (col '() 1 0))
              ((atom? (car l))
                 (cond ((even? (car l)) (evens-only*C (cdr l) 
                                                      (lambda (newl p s)
                                                             (col (cons (car l) newl) (* (car l) p) s))))
                       (else (evens-only*C (cdr l) 
                                           (lambda (newl p s)
                                                 (col newl p (+ (car l) s)))))))
              (else (evens-only*C (car l) 
                                  (lambda (al ap as)
                                     (evens-only*C (cdr l)
                                                   (lambda (dl dp ds)
                                                          (col (cons al dl) 
                                                               (* ap dp)
                                                               (+ as ds))))))))))

;;(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend) 
                                                               

(define the-last-friend
    (lambda (newl product sum)
        (cons sum (cons product newl))))

; recursion that can ignore part of the list called - unnatural recursion
; keep-looking - it's a partial function (e.g. it can not reach the goal and stuck with infinite recursion)
; the opposite to partial is total functions 

(define eternity ;; this is the most unnatural recursion possible - it's never meet the goal
  (lambda (x) (eternity x)))


(define first
    (lambda (pair)
        (car pair)))

(define second
    (lambda (pair)
        (car (cdr pair))))

;; This build implementation is wrong becaus it just consing tow values
;; (define build
;;   (lambda (x y) (cons x y)))

;; (cons  'a '(b c)) => (a b c)       wrong
;; (build 'a '(b c)) => (a (b c))     correct

(define build
    (lambda (f s)
        (cons f (cons s '()))))
    (lambda (x) (eternity x))


(define shift
    (lambda (pair)
        (build (first (first pair))
               (build (second (first pair)) (second (shift '((a b) '(d e)))

(define align
  (lambda (pora)
    (cond ((atom? pora) pora)
	  ((a-pair? (first pora)) (align (shift pora)))
	  (else (build (first pora)
		       (align (second pora)))))))

(define weight*
  (lambda (pora)
    (cond ((atom? pora) 1)
	  (else (o+ (o* (weight* (first pora)) 2)
		    (weight* (second pora)))))))

(define shuffle
  (lambda (pora)
    (cond ((atom? pora) pora)
	  ((a-pair? (first pora)) (shuffle (revpair pora)))
	  (else (build (first pora)
		       (shuffle (second pora)))))))

;; Collatz conjenture funtion
(define C
  (lambda (n)
    (cond ((one? n) 1)
	  (else (cond ((even? n) (C (o/ n 2)))
		      (else (C (add1 (o* 3 n)))))))))



(define A
  (lambda (n m)
    (cond ((zero? n) (add1 m))
	  ((zero? m) (A (sub1 n) 1))
	  (else (A (sub1 n)
		   (A n (sub1 m)))))))

;; last-try - it's a total function 
(define last-try
  (lambda (x)
    (and (will-stop? last-try)
	 (eternity x))))

;; if we say that (will-stop? last-try) is #f
;; (last-try ()) return #f - but then (will-stop? last-try) = #f is wrong

;; if we say that (will-stop? last-try) is #t
;; (last-try ()) can't return anything because (eternity x) is not returning anything so
;; (will-stop? last-try) = #t is wrong either

;;(last-try ())
;; (define will-stop?
;;   (lambda (f)
;;     (cond (

;;length0
;; (lambda (l)
;;   (cond ((null? l) 0)
;; 	(else (add1 (eternity (cdr l))))))

;; ;;length1
;; (lambda (l)
;;   (cond ((null? l) 0)
;; 	(else (lambda (l)
;; 		(cond ((null? l) 0)
;; 		      (else (add1 (eternity (cdr l))))))
;; 	      (cdr l))))

;; ;;length2
;; (lambda (l)
;;   (cond ((null? l) 0)
;; 	(else (lambda (l)
;; 		(cond ((null? 1) 0)
;; 		      (else (lambda (l)
;; 			      (cond ((null? l) 0)
;; 				    (else (add1 (eternity (cdr l))))))
;; 			    (cdr l))))
;; 	      (cdr l))))


;; ((lambda (length)
;;    (lambda (l)
;;      (cond ((null? l) 0)
;; 	   (else (add1 (length (cdr l)))))))
;;  eternity)

;; ((lambda (f)
;;    (lambda (l)
;;      (cond ((null? l) 0)
;; 	   (else (add1 (f (cdr l)))))))
;;  ((lambda (g)
;;     (lambda (l)
;;       (cond ((null? l) 0)
;; 	    (else (add1 (g (cdr l)))))))
;;   eternity))

;; ;;length0
;; ((lambda (mk-length)
;;    (mk-length eternity))
;;  (lambda (length)
;;    (lambda (l)
;;      (cond ((null? l) 0)
;; 	   (else (add1 (length (cdr l))))))))

;; ;;length1
;; ((lambda (mk-length)
;;    (mk-length
;;     (mk-length eternity)))
;;  (lambda (length)
;;    (lambda (l)
;;      (cond ((null? l) 0)
;; 	   (else (add1 (length (cdr l))))))))

;; ;;length4
;; ((lambda (mk-length)
;;    (mk-length
;;     (mk-length
;;      (mk-length
;;       (mk-length eternity)))))
;;  (lambda (length)
;;    (lambda (l)
;;      (cond ((null? l) 0)
;; 	   (else (add1 (length (cdr l))))))))


;; ((lambda (mk-length)
;;    (mk-length mk-length))
;;  (lambda (mk-length)
;;    (lambda (l)
;;      (cond ((null? l) 0)
;; 	   (else (add1 ((mk-length eternity)
;; 			(cdr l))))))))

(((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 ((mk-length mk-length)
			(cdr l)))))))) '(a b c 3 s)) ;; => 5

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 ((lambda (x)
			  ((mk-length mk-length) x))
			(cdr l)))))))) ;; => 5

((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
	    ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond ((null? l) 0)
	   (else (add1 (length (cdr l))))))))

;;This is applicative order Y combinator - recursion only through the lambda functions
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;;Throwing the length lambda function and the argument list and it's work - amazing!
((Y  (lambda (length)
      (lambda (l)
	(cond ((null? l) 0)
	      (else (add1 (length (cdr l)))))))) '(a v c))

;;CHAPTER 10
;;entry is a pair whose first element is a set and second element has length same as the first
(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
			  (first entry)
			  (second entry)
			  entry-f)))
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names) (entry-f name))
	  ((eq? name (car names)) (car values))
	  (else (lookup-in-entry-help name
				      (cdr names)
				      (cdr values)
				      entry-f)))))

;;table (also called enviroment) - is a list of entries
(define extend-tables cons)


(define lookup-in-table
  (lambda (name table table-f)
    (cond ((null? table) (table-f name))
	  (else (lookup-in-entry name
				 (car table)
				 (lambda (name)
				   (lookup-in-table name
						    (cdr table)
						    table-f)))))))

;;(value '(car (quote (a b c))))

(define atom-to-expression
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e (quote cons)) *const)
     ((eq? e (quote car)) *const)
     ((eq? e (quote cdr)) *const)
     ((eq? e (quote null?)) *const)
     ((eq? e (quote eq?)) *const)
     ((eq? e (quote atom?)) *const)
     ((eq? e (quote zero?)) *const)
     ((eq? e (quote add1)) *const)
     ((eq? e (quote sub1)) *const)
     ((eq? e (quote number?)) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond ((eq? (car e) (quote quote)) *quote)
	    ((eq? (car e) (quote lambda)) *lambda)
	    ((eq? (car e) (quote cond)) *cond)
	    (else *application)))
     (else *application))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
	    
;;defining actions
;;CONST
(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build (quote primitive) e)))))

;;QUOTE
(define *quote
  (lambda (e table) (text-of e)))

(define text-of second)

;;IDENTIFIER
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car (quote ()))))

;;LAMBDA
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
	   (cons table (cdr e)))))

(define table-of first)
(define formals-of second)
(define body-of third)

;;COND
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)
 
(define evcond
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines)) table))
     ((meaning (question-of (car lines)) table)
      (meaning (answer-of (car lines)) table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond ((atom? x) (eq? x (quote else)))
	  (else #f))))

(define question-of first)
(define asnwer-of second)

;;APPLICATION
(define evlis
  (lambda (args table)
    (cond ((null? args) (quote ()))
	  (else (cons (meaning (car args) table)
		      (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
     (meaning (function-of e) table)
     (elvis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (qoute non-primitive))))

(define apply
  (lambda (fun vals)
    (cond
     ((primitive? fun) (apply-primitive (second fun) vals))
     ((non-primitive? fun) (apply-closure (second fun) vals)))))

;;TODO this function can we crd on the empty list or sub1 on 0 etc.
(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name (quote cons)) (cons (first vals) (second vals)))
     ((eq? name (quote car)) (car (first vals)))
     ((eq? name (quote cdr)) (cdr (first vals)))
     ((eq? name (quote null?)) (null? (first vals)))
     ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
     ((eq? name (quote atom?)) (.atom? (first vals)))
     ((eq? name (quote zero?)) (zero? (first vals)))
     ((eq? name (quote add1)) (add1 (first vals)))
     ((eq? name (quote sub1)) (sub1 (first vals)))
     ((eq? name (quote number?)) (number? (first vals))))))

(define .atom?
  (lambda (x)
    (cond
     ((atom? x) #t)
     ((null? x) #f)
     ((eq? (car x) (quote primitive)) #t)
     ((eq? (car x) (quote non-primitive)) #t)
     (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
	     (extend-table
	      (new-entry
	       (formals-of closure)
	       vals)
	      (table-of closure)))))

(define first
    (lambda (p) (car p)))

(define second
    (lambda (p) (car (cdr p))))

(define build
    (lambda (x y) (cons x y)))
