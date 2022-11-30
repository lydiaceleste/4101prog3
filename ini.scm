(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))


(define (not b) (if b #f #t))
(define (and x y) (if x y #f))
(define (or x y) (if x #t y))

(define (list . l) l)

(define (length l)
  (if (null? l) 0
      (Addhelper 1 (length (cdr l)))))

; n-ary map not yet implemented
(define (map f l)
  (if (null? l) '()
      (cons (f (car l)) (map f (cdr l)))))

; n-ary for-each not yet implemented
(define (for-each f l)
  (if (null? l) '()
      (begin (f (car l)) (for-each f (cdr l)))))

(define (eof-object? x)
  (eq? x 'end-of-file))


;COMPARISON OPERATORS

(define (eqv? x y)
  (if (and (number? x) (number? y)) (eqhelper x y)
      (eq? x y)))

(define (b> x y) (b< y x))
(define (b<= x y) (or (eqhelper x y) (b< x y)))
(define (b>= x y) (or (eqhelper x y) (b> x y)))

(define (< x y . l)
  (if (null? l) (b< x y)
      (and (b< x y) (apply < (cons y l)))))

(define (> x y . l)
  (if (null? l) (b> x y)
      (and (b> x y) (apply > (cons y l)))))

(define (<= x y . l)
  (if (null? l) (b<= x y)
      (and (b<= x y) (apply <= (cons y l)))))

(define (>= x y . l)
  (if (null? l) (b>= x y)
      (and (b>= x y) (apply >= (cons y l)))))

(define (= x y . l)
  (if (null? l) (eqhelper x y)
      (and (eqhelper x y) (apply = (cons y l)))))

(define (eqhelper a b)
  (let ((a (helper a)) (b (helper  b)))
  (if (and (integer? a) (integer? b)) (s48-= a b) 
  (if (or (integer? a) (integer? b)) #f
  (if (and (rational? a) (rational? b)) (and (eqhelper (cadr a) (cadr b)) 
    (eqhelper (caddr a) (caddr b)))
    (type-error "error: cannot check equality" #f))))))


(define (positive? x)
    (> (numerator x) 0))

(define (negative? x)
    (< (numerator x) 0))

(define (zero? x)
    (= (numerator x) 0))

(define (equal? a b)
    (cond ((eqv? a b) #t)
     ((and (pair? a) (pair? b))
     (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))) (else #f)))


(define (zero? x) (eqhelper x 0))
(define (positive? x) (b< 0 x))
(define (negative? x) (b< x 0))




;ARITHMETIC OPERATORS

(define (+ . l)
  (if (null? l) 0
      (addhelper (car l) (apply + (cdr l)))))

(define (addhelper a b)
 (if (and (integer? a) (integer? b)) (s48-+ a b)
 (if (or (rational? a) (rational? b))
 (let ((lcm2 (lcm (numerator a) (numerator b) (denominator a) (denominator b))))
   (helper (rational (s48-+ (cadr (car lcm2)) (cadr (car (cdr lcm2)))) (caddr (car lcm2))))))))


(define (* . l)
  (if (null? l) 1
      (multhelper (car l) (apply * (cdr l)))))

(define (multhelper a b)
  (if (and (integer? a) (integer? b)) (s48-* a b)
  (if (and (rational? a) (rational? b))
    (helper (rational (s48-* (numerator a) (numerator b)) (s48-* (denominator a) (denominator b))))
     (type-error "error: cannot multiply" 1))))


(define (/ . l)
  (if (= (length l) 0) (type-error "error: cannot divide" 1)
  (if (= (length l) 1) (type-error "error: cannot divide" 1)
  (if (= (length l) 2) (divhelper (car l) (car (cdr l))) 
                       (divhelper (car l) (apply * (cdr l)))))))


(define (divhelper a b)
  (if (and (integer? a) (integer? b))
  (if (= b 0) (type-error "error: cannot divide" 1)
  (if (= a 0) 0
  (let ((c (gcd a b)))
  (if (= a b) 1
  (if (= a 0) 0
  (if (= c b) (s48-/ a b) (rational a b)))))))
  (if (or (rational? a) (rational? b))
      (helper (rational (* (numerator a) (denominator b)) (* (denominator a) (numerator b))))
      (type-error "error: cannot divide" 1))))



(define (- . l)
   (if (null? l) 0
   (if (= (length l) 1) (car l)
       (addhelper (car l) (apply subhelper (cdr l))))))

(define (subhelper . l)
   (if (null? l) 0
   (let ((x (multhelper (car l) -1)))
   (if (= (length l) 1) x
   	(addhelper x (apply subhelper (cdr l)))))))

(define (subhelper2 a b)
   (if (and (integer? a) (integer? b)) (s48-- a b)
   (if (or (rational? a) (rational? b))
   (let ((lcm1 (lcm (numerator a) (numerator b) (denominator a) (denominator b))))
       (helper (rational (s48-- (cadr (car lcm1)) (cadr (car (cdr lcm1)))) (caddr (car lcm1))))))))


(define (lcm n1 n2 d1 d2)
  (let ((p1 (s48-* n1 d2))
        (p2 (s48-* n2 d1))
        (p3 (s48-* d1 d2))
        (p4 (s48-* d1 d2))) (cons (rational p1 p3) (cons (rational p2 p4) '()))))

	  

;TEST PREDICATES


(define (number? x)
 (s48-rational? x))

(define (integer? x)
  (if (number? x) #t #f))

(define (rational? x)
  (if (number? x) #t 
  (if (pair? x) (if (eq? 'rational (car x))#t #f) #f)))

(define (rational x y) 
  (list 'rational x y))




;GCD FUNCTIONS

(define (denominator x)
  (let ((x (helper x)))
  (if (integer? x) 1 (caddr x))))

(define (numerator x)
  (let ((x (helper x)))
  (if (integer? x) x (cadr x))))

(define (helper a)
   (if (integer? a) a
   (if (rational? a)
     (let ((n (/ (cadr a) (gcd (cadr a) (caddr a))))
           (d (/ (caddr a) (gcd (cadr a) (caddr a))))) (divhelper n d))
                (type-error "cannot simplify" 1))))       


(define (gcd . l)
   (if (= (length l) 0) 0
   (if (= (length l) 1) (car l)
   (if (= (length l) 2) (gcdhelper (car l) (car (cdr l))) 
                        (gcdhelper (car l) (apply gcd (cdr l)))))))

(define (gcdhelper a b)
  (if (negative? a) (set! a (* a -1)))
  (if (negative? b) (set! b (* b -1)))
  (if (zero? a) b
  (if (zero? b) a
  (if (= a b) a
  (if (> a b)
     (gcdhelper (- a b) b)
     (gcdhelper a (- b a)))))))



 

;WRITE FUNCTIONS

(define (whelper a)
    (if (null? a) (display ")")
    (if (list? a) 
    (if (rational? a)
      (begin 
	(write (numerator a)) (display "/") (write (denominator a)))
      (begin
         (if (and (pair? (car a)) (not (rational? (car a)))) (display "(")) (whelper (car a))
         (if (not (null? (cdr a))) (display " ")) (whelper (cdr a))))
         (begin
           (write a)))))


(define (w a)
   (begin
      (if (and (list? a) (not (rational? a))) (display "("))
          (whelper a)))
