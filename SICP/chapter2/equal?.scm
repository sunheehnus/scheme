(define (equal? list1 list2)
  (cond ((and (null? list1) (null? list2)) #t)
		((not (eq? (car list1) (car list2))) #f)
		(else (equal? (cdr list1) (cdr list2)))))

(display
  (equal? (list 'a 'b 'c) '(a  b  c)))
(newline)
