(define (abs x)
  (cond
	((< x 0) (- x))
	(else x)))
(display (abs -1))
(newline)
(display (abs 1))
(newline)
