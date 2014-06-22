(define (expt-recursive b n)
  (if (= n 0)
	b
	(* b (expt-recursive b (- n 1)))))

(define (expt-iterative b n)
  (define (iter res cur)
	(if (< cur n)
	  (iter (* res b) (+ cur 1))
	  res))
  (iter 1 0)
  )

(define (fast-expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
		((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))
