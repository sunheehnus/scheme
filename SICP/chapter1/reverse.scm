(define (reverse l)
  (define (iter res li)
    (if (null? li)
      res
      (iter (cons (car li) res)
	    (cdr li))))
  (iter (list) l))

(display (reverse (list 1 2 3 4)))
