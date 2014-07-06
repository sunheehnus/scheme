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

(define (same-parity . lst)
  (define (same-with-1st element) (eq? (odd? (car lst))
									   (odd? element)))
  (define (insert-with-judge element result)
	(if (same-with-1st element)
	  (cons element result)
	  result))
  (define (iter lst result)
	(if (null? lst)
	  result
	  (iter (cdr lst) (insert-with-judge (car lst) result))))
  (reverse (iter lst (list))))

(define (same-parity . lst)
  (define (same-with-1st element) (eq? (odd? (car lst))
									   (odd? element)))
  (define (append-with-judge element result)
	(if (same-with-1st element)
	  (append result (list element))
	  result))
  (define (iter lst result)
	(if (null? lst)
	  result
	  (iter (cdr lst) (append-with-judge (car lst) result))))
  (iter lst (list)))

(define (scale-list items factor)
  (if (null? items)
	(list)
	(cons (* (car items) factor)
		  (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
	(list)
	(cons (proc (car items))
		  (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

