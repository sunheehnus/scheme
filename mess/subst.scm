(define (subst sour tar array)
  (define (atom? x)
	(not (pair? x)))
  (cond ((atom? array) (cond ((eq? array sour) tar) (#t array)))
		(#t (cons (subst sour tar (car array))
				  (subst sour tar (cdr array))))))

(display (subst 'a 'b '(a b c d)))
(newline)
