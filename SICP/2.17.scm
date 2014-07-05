(define (last-pair l)
  (cond ((null? l) #f)
	((null? (cdr l)) (car l))
	(#t (last-pair (cdr l)))))
