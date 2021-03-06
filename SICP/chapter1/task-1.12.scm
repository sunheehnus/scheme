(define (pascal-triangle N)
  (define (find-value-by-pos x y)
	(cond ((< x y) (- 1))
		  ((or (= y 1) (= x y)) 1)
		  (else (+ (find-value-by-pos (- x 1)
									  (- y 1))
				   (find-value-by-pos (- x 1)
									  y)))))
  (define (iter-col c cnt)
	(if (< cnt c)
	  (begin
		(display (find-value-by-pos c (+ cnt 1)))
		(display " ")
		(iter-col c (+ cnt 1)))))
  (define (iter-row r cnt)
	(if (< cnt r)
	  (begin
		(iter-col (+ cnt 1) 0)
		(newline)
		(iter-row r (+ cnt 1)))))
  (iter-row N 0)
  )

(begin
  (pascal-triangle 10)
  )
