(define (append x y)
  (cond ((null? x) y)
		(#t (cons (car x) (append (cdr x) y)))))
(display (append '(a b c) '(d e f)))
(newline)
