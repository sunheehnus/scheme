(define (pair x y)
  (define (atom? x)
	(not (pair? x)))
  (cond ((and (null? x) (null? y))
		 '())
		((and (not (atom? x)) (not (atom? y)))
		 (cons (list (car x) (car y))
			   (pair (cdr x) (cdr y))))))

(display (pair '(a b c) '(d e f)))
