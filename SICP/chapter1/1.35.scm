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

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1
							  (/ 1 x)))
			   1.0))
(display (golden-ratio))
