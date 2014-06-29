(define (fixed-point f first-guess)
  (define tolerance 0.0001)
  (define (close-enough? x y)
	(< (abs (- x y)) tolerance))
  (define (iter guess)
	((lambda (next)
	  (if (close-enough? guess next)
		next
		(iter next)))
	 (f guess)))
  (iter first-guess))

(display (fixed-point cos 1.0))
(newline)
