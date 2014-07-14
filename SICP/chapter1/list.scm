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
					 (* x x)))
	   tree))

(define (tree-map proc tree)
  (cond ((null? tree) (list))
		((pair? tree) (cons (tree-map proc (car tree))
							(tree-map proc (cdr tree))))
		(else (proc tree))))

(define (square x)
  (* x x))

(define (square-tree tree)
  (tree-map square tree))

(define (subsets set)
  (if (null? set)
	(list (list))
	(let ((rest (subsets (cdr set))))
	  (append rest (map (lambda (x) (append (list (car set)) x))
						rest)))))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
		((not (pair? tree))
		 (if (odd? tree) (square tree) 0))
		(else (+ (sum-odd-squares (car tree))
				 (sum-odd-squares (cdr tree))))))
(define (fib x)
  (if (< x 2)
	x
	(+ (fib (- x 1)) (fib (- x 2)))))

(define (even-fibs n)
  (define (next k)
	(if (> k n)
	  (list)
	  (let ((f (fib k)))
		(if (even? f)
		  (cons f (next (+ k 1)))
		  (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) (list))
		((predicate (car sequence))
		 (cons (car sequence)
			   (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
	(list)
	(cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((pair? tree) (append (enumerate-tree (car tree))
							  (enumerate-tree (cdr tree))))
		((null? tree) (list))
		(else (list tree))))

(define (sum-odd-squares tree)
  (accumulate +
			  0
			  (map square
				   (filter odd?
						   (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
			  (list)
			  (filter even?
					  (map fib
						   (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
			  (list)
			  (map square
				   (map fib
						(enumerate-interval 0 n)))))

(define (product-of-square-of-odd-elements sequence)
  (accumulate *
			  1
			  (map square
				   (filter odd? sequence))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
				(+ this-coeff
				   (* higher-terms x)))
			  0
			  coefficient-sequence))

(define (count-leaves t)
  (accumulate (lambda (x y)
				(+ (length x) y))
			  0
			  (map enumerate-tree
				   t)))

(define (gen-pairs n)
  (accumulate append (list)
			  (map (lambda (x)
					 (map (lambda (y) (list x y))
						  (enumerate-interval 1 (- x 1))))
				   (enumerate-interval 1 n))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (gen-pairs n)
  (flatmap (lambda (x) (map (lambda (y) (list x y))
							(enumerate-interval 1 (- x 1))))
		   (enumerate-interval 1 n)))

(define (isprime? n)
  (define (next x)
	(if (= x 2)
	  3
	  (+ x 2)))
  (define (find-divisor x div) ;find div that can be divided by x
	(define (square x)
	  (* x x))
	(cond ((= (remainder x div) 0) div)
		  ((> (square div) x) x)
		  (else (find-divisor x (next div)))))
  (define (smallest-divisor x)
	(find-divisor x 2))
  (and (> n 1) (= (smallest-divisor n) n)))

(define (prime-sum? pair)
  (isprime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (define (triple-sum x y)
	(list x y (+ x y)))
  (triple-sum (car pair) (cadr pair)))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (gen-pairs n)))
  )

(define (permutations s)
  (if (null? s)
	(list (list))
	(flatmap (lambda (x)
			   (map (lambda (p) (cons x p))
					(permutations (remove x s))))
			 s)))

(define (remove item sequence)
  (filter (lambda (x) (not (eq? x item)))
		  sequence))

(define (unique-pairs n)
  (accumulate append (list)
			  (map (lambda (x)
					 (map (lambda (y) (list x y))
						  (enumerate-interval 1 (- x 1))))
				   (enumerate-interval 1 n))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n)))
  )

(define (gen-tripple-group n)
  (accumulate append (list)
			  (accumulate append (list) (map
										  (lambda (x)
											(map
											  (lambda (y)
												(map
												  (lambda (z)
													(list x y z))
												  (enumerate-interval 1 (- y 1))))
											  (enumerate-interval 1 (- x 1))))
										  (enumerate-interval 1 n))
						  )))

(define (gen-tripple-group n)
  (flatmap (lambda (x) x)
		   (flatmap
			 (lambda (x)
			   (map
				 (lambda (y)
				   (map
					 (lambda (z)
					   (list x y z))
					 (enumerate-interval 1 (- y 1))))
				 (enumerate-interval 1 (- x 1))))
			 (enumerate-interval 1 n))
		   ))
(define (gen-tripple-group n)
  (flatmap (lambda (x)
			 (flatmap (lambda (y)
						(map (lambda (z)
							   (list x y z))
							 (enumerate-interval 1 (- y 1))))
					  (enumerate-interval 1 (- x 1))))
		   (enumerate-interval 1 n)))

(display (gen-tripple-group 10))
(newline)
