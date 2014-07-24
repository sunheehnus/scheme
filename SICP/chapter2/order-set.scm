(define equal? =)
(define less? <)
(define more? >)
(define (element-of-set? x set)
  (cond ((null? set) #f)
		((equal? x (car set)) #t)
		((less? x (car set)) #f)
		(else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (null? set)
	(list x)
	(let ((head (car set)))
	  (cond ((equal? x head) set)
			((less? x head) (cons x set))
			((more? x head) (cons head (adjoin-set x (cdr set))))))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
	'()
	(let ((x1 (car set1))
		  (x2 (car set2)))
	  (cond ((equal? x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
			((less? x1 x2) (intersection-set (cdr set1) set2))
			((more? x1 x2) (intersection-set set1 (cdr set2)))))))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
		((null? set2) set1)
		(else (let ((x1 (car set1))
					(x2 (car set2)))
				(cond ((equal? x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
					  ((less? x1 x2) (cons x1 (union-set (cdr set1) set2)))
					  ((more? x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))
(define set1 (list 1 2 3 4 5))
(define set2 (list 3 4 5 6 7))

(display
  (element-of-set? 3 set1)
  )
(newline)
(display
  (element-of-set? 4 set2)
  )
(newline)
(display
  (adjoin-set 10 set1)
  )
(newline)
(display
  (intersection-set set1 set2)
  )
(newline)
(display
  (union-set set1 set2)
  )
(newline)