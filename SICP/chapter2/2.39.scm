(define (fold-right op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(fold-right op initial (cdr sequence)))))
(define (fold-left op initial sequence)
  (define (iter result rest)
	(if (null? rest)
	  result
	  (iter (op result (car rest))
			(cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x)))
			  (list)
			  sequence))

(display (reverse (list 1 2 3 4 5)))
(newline)

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x))
			 (list)
			 sequence))

(display (reverse (list 1 2 3 4 5)))
(newline)
