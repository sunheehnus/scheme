(define equal? =)
(define less? <)
(define more? >)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
		((equal? x (entry set)) #t)
		((less? x (entry set)) (element-of-set? x (left-branch set)))
		((more? x (entry set)) (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
		((equal? x (entry set)) set)
		((less? x (entry set)) (make-tree (entry set)
										  (adjoin-set x (left-branch set))
										  (right-branch set)))
		((more? x (entry set)) (make-tree (entry set)
										  (left-branch set)
										  (adjoin-set x (right-branch set))))))
(define (tree->list tree)
  (if (null? tree)
	'()
	(append (tree->list (left-branch tree))
			(cons (entry tree)
				  (tree->list (right-branch tree))))))
(define (tree->list tree)
  (define (copy-to-list tree result-list)
	(if (null? tree)
	  result-list
	  (copy-to-list (left-branch tree)
					(cons (entry tree)
						  (copy-to-list (right-branch tree)
										result-list)))))
  (copy-to-list tree '()))
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
	(cons '() elts)
	(let ((left-size (quotient (- n 1) 2)))
	  (let ((left-result (partial-tree elts left-size)))
		(let ((left-tree (car left-result))
			  (non-left-elts (cdr left-result))
			  (right-size (- n (+ left-size 1))))
		  (let ((this-entry (car non-left-elts))
				(right-result (partial-tree (cdr non-left-elts)
											right-size)))
			(let ((right-tree (car right-result))
				  (remaining-elts (cdr right-result)))
			  (cons (make-tree this-entry left-tree right-tree)
					remaining-elts))))))))

(define (union-set tset1 tset2)
  (define (union-set-list lset1 lset2)
	(cond ((null? lset1) lset2)
		  ((null? lset2) lset1)
		  (else (let ((x1 (car lset1))
					  (x2 (car lset2)))
				  (cond ((equal? x1 x2) (cons x1 (union-set-list (cdr lset1) (cdr lset2))))
						((less? x1 x2) (cons x1 (union-set-list (cdr lset1) lset2)))
						((more? x1 x2) (cons x2 (union-set-list lset1 (cdr lset2)))))))))
  (let ((lset1 (tree->list tset1))
		(lset2 (tree->list tset2)))
	(list->tree (union-set-list lset1 lset2))))

(define (intersection-set tset1 tset2)
  (define (intersection-set-list lset1 lset2)
	(if (or (null? lset1) (null? lset2))
	  '()
	  (let ((x1 (car lset1))
			(x2 (car lset2)))
		(cond ((equal? x1 x2) (cons x1 (intersection-set-list (cdr lset1) (cdr lset2))))
			  ((less? x1 x2) (intersection-set-list (cdr lset1) lset2))
			  ((more? x1 x2) (intersection-set-list lset1 (cdr lset2)))))))
  (let ((lset1 (tree->list tset1))
		(lset2 (tree->list tset2)))
	(list->tree (intersection-set-list lset1 lset2))))

(define list1 (list 1 2 3 4 5 6 7 8))
(define list2 (list 2 3 4 5 6 7 8 9))
(display (tree->list (union-set (list->tree list1) (list->tree list2))))
(newline)
(display (tree->list (intersection-set (list->tree list1) (list->tree list2))))
(newline)

;storage (list key left-tree right-tree value)
(define (lookup given-key set-of-records)
  (define (key tree) (caar tree))
  (define (value tree) (cadar tree))
  (define (left tree) (cadr tree))
  (define (right tree) (caddr tree))
  (if (null? set-of-records)
	#f
	(let ((this-key (key set-of-records)))
	  (cond ((equal? given-key this-key) (value set-of-records))
			((less? given-key this-key) (lookup given-key (left set-of-records)))
			((more? given-key this-key) (lookup given-key (right set-of-records)))))))

(define tree1 (list->tree (list (list 1 "sunxingzhe") (list 2 "zhexingsun") (list 3 "xingzhesun"))))
(display (lookup 9 tree1))
(newline)
(display (lookup 1 tree1))
(newline)
(display (lookup 2 tree1))
(newline)
(display (lookup 3 tree1))
(newline)
