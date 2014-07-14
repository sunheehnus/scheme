(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
	(/ (+ (f (- x dx)) (f x) (f (+ x dx)))
	   3)))

(define (repeated f n)
  (if (= n 1)
	(lambda (x)
	  (f x))
	(lambda (x)
	  (f ((repeated f (- n 1)) x)))))

(define (n-smooth f n)
  (repeated (smooth f) n))

(define (f x) x)
(display ((n-smooth f 128) 3))
(newline)
