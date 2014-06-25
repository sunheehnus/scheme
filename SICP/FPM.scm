;(a^b) mod m
(define (FPM a b m)
  (define (mul-and-mod x y)
	(remainder (* x y) m))
  (define (square-and-mod x) (mul-and-mod x x))
  (cond ((= b 0) (remainder 1 m))
		((even? b) (square-and-mod (FPM a (/ b 2) m)))
		(else (mul-and-mod a (FPM a (- b 1) m)))))
