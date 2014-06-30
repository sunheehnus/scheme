(define (make-rat n d);make rational numer   n/d
  (let ((div (gcd n d)))
	(cons (/ n div) (/ d div))))

(define (make-rat n d)
  ;GCD always return non-neg value
  (define (GCD a b)
	(if (= b 0)
	  (cond ((> a 0) a)
			(#t (- a)))
	  (GCD b (remainder a b))))
  (let ((div (GCD n d)))
	(cond ((= d 0) (cons 0 0))
		  ((< d 0) (cons (/ n (- div)) (/ d (- div))))
		  (#t (cons (/ n div) (/ d div))))))

(define (numer x) (car x));get x=n/d's n
(define (denom x) (cdr x));get x=n/d's d

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))

(define (equal-ret? x y)
  (= (* (numer x) (denom y))
	 (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(print-rat one-half)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))
(print-rat (make-rat 0 0))
(print-rat (make-rat -1 0))
(print-rat (make-rat 1 -3))
