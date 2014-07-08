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

(define (square-list items)
  (define (square x) (* x x))
  (if (null? items)
	(list)
	(cons (square (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map (lambda (x) (* x x)) items))

(define (for-each proc items)
  (if (null? items)
	#t
	(begin
	  (proc (car items))
	  (for-each proc (cdr items)))))

(define (count-leave x)
  (cond ((null? x) 0)
		((not (pair? x)) 1)
		(else (+ (count-leave (car x))
				 (count-leave (cdr x))))))

(define (deep-reverse x)
  (define (parse element)
	(if (pair? element)
	  (deep-reverse element)
	  element))
  (define (deep-iter res lst)
	(if (null? lst)
	  res
	  (deep-iter (cons (parse (car lst)) res)
				 (cdr lst))))
  (deep-iter (list) x))

(define (fringe x)
  (cond ((pair? x) (append (fringe (car x))
						   (fringe (cdr x))))
		((null? x) x)
		(else (list x))))

(define (fringe x)
  (define (iter result lst)
	(cond ((pair? lst) (iter (iter result (car lst)) (cdr lst)))
		  ((null? lst) result)
		  (else (cons lst result))))
  (reverse (iter (list) x)))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (define (isLeaf? branch)
	(not (pair? (branch-structure branch))))
  (if (isLeaf? mobile)
	(cadr mobile)
	(+ (total-weight (car mobile))
	   (total-weight (cdr mobile)))))

(define (total-weight*length mobile)
  (define (isLeaf? branch)
	(not (pair? (branch-structure branch))))
  (if (isLeaf? mobile)
	(* (cadr mobile) (car mobile))
	(+ (total-weight*length (car mobile))
	   (total-weight*length (cdr mobile)))))

(define (balance? mobile)
  (eq? (total-weight*length (car mobile))
	   (total-weight*length (cdr mobile))))

(define (square-tree tree)
  (cond ((null? tree) (list))
		((pair? tree) (cons (square-tree (car tree))
							(square-tree (cdr tree))))
		(else (* tree tree))))

(define (square-tree tree)
  (map (lambda (x) (if (pair? x)
					 (square-tree x)
					 (else (* x x))))
	   tree))

(define (tree-map proc tree)
  (cond ((null? tree) (list))
		((pair? tree) (cons (tree-map proc (car tree))
							(tree-map proc (cdr tree))))
		(else (proc tree))))

(define (square-tree tree)
  (define (square x)
	(* x x))
  (tree-map square tree))
