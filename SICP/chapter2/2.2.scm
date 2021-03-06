(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (let ((start-point (start-segment segment))
		(end-point (end-segment segment)))
	(make-point (/ (+ (x-point start-point) (x-point end-point)) 2)
				(/ (+ (y-point start-point) (y-point end-point)) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define seg (make-segment (make-point 1 1)
						  (make-point 6 11)))
(print-point (start-segment seg))
(print-point (end-segment seg))
(print-point (midpoint-segment seg))
