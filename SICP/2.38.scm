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

(begin
  (display (fold-right / 1 (list 1 2 3)))
  (newline)
  (display (fold-left / 1 (list 1 2 3)))
  (newline)
  (display (fold-right list (list) (list 1 2 3)))
  (newline)
  (display (fold-left list (list) (list 1 2 3)))
  (newline)
  )
;op is an option with the same result of (op A B) and (op B A)
