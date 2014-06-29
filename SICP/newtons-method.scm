(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2)) tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? next guess)
		next
		(try next))))
  (try first-guess))

(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
	(/ (- (g (+ x dx)) (g x))
	   dx)))

(define (newton-transform g)
  (lambda (x)
	(- x
	   (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (average-damp (newton-transform g)) guess))
  ;(fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (* y y) x))
				  1.0))

(display (sqrt 2))
(newline)
