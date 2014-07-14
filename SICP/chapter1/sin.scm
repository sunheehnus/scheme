(define (sin angle)
  (define (cube x)
	(* x x x))
  (define (extend x)
	(- (* 3 x) (* 4 (cube x))))
  (cond ((< (abs angle) 0.1) angle)
		(else (extend (sin (/ angle 3))))))

(display (sin (* 3.1415926 4)))
(newline)
