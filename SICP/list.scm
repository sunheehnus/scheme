(define (list-ref items n)
  (if (= n 0)
	(car items)
	(list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
	0
	(+ 1 (length (cdr items)))))

(define (length items)
  (define (iter items res)
	(if (null? items)
	  res
	  (iter (cdr items) (+ res 1))))
  (iter items 0))

(define (append list1 list2)
  (if (null? list1)
	list2
	(cons (car list1) (append (cdr list1) list2))))

(define (last-pair l)
  (cond ((null? l) #f)
	((null? (cdr l)) (car l))
	(#t (last-pair (cdr l)))))

(define (reverse l)
  (define (iter res li)
    (if (null? li)
      res
      (iter (cons (car li) res)
	    (cdr li))))
  (iter (list) l))
