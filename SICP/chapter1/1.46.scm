(define (iterative-improve good-enough? next)
  (lambda (x)
	(if (good-enough? x)
	  x
	  ((iterative-improve good-enough? next) (next x)))))

(define (sqrt x)
  (define (good-enough? guess)
	(< (abs (- x (* guess guess))) 0.000001))
  (define (next guess)
	(/ (+ guess (/ x guess)) 2))
  ((iterative-improve good-enough? next) 1.0))

(display (sqrt 2))
(newline)

(define (fixed-point f)
  (define (good-enough? guess)
	(< (abs (- (f guess) guess)) 0.000001))
  (define (next guess)
	(f guess))
  ((iterative-improve good-enough? next) 1.0))

(define (average-damp f)
  (lambda (x)
  (/ (+ (f x) x) 2)))
(display (fixed-point (average-damp (lambda (y) (/ 2 y)))))
(newline)
