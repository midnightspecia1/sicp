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

(define first
    (lambda (pair)
        (car pair)))

(define firsts
    (lambda (pairs)
        (cond ((null? pairs) '()) 
              (else (cons (first (car pairs)) (firsts (cdr pairs)))))))

(define second
    (lambda (pair)
        (car (cdr pair))))
        
(define seconds
    (lambda (pairs)
        (cond ((null? pairs) '()) 
              (else (cons (second (car pairs)) (seconds (cdr pairs)))))))        

(define build
    (lambda (f s)
        (cons f (cons s '()))))

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
(define rember-f
    (lambda (test? a l)
        (cond ((null? l) '())
              ((test? (car l) a) (cdr l))
              (else (cons (car l) (rember-f test? a (cdr l)))))))
