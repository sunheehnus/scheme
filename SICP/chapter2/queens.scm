;;;;;;;;;;;;;;;;;;;;;;;;basic methods;;;;;;;;;;;;;;;;;;;;;;;;
(define (enumerate-interval low high)
  (if (> low high)
	(list)
	(cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
	initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;methods of queens;;;;;;;;;;;;;;;;;;;;;
(define (build-empty-board board-size)
  (define (iter size empty-element result)
	(if (< size board-size)
	  (iter (+ size 1) empty-element (cons empty-element result))
	  result))
  (iter 0
		(iter 0 0 (list))
		(list)))

(define (get-position row col board)
  (define (get-col k line)
	(if (not (= k 1))
	  (get-col (- k 1) (cdr line))
	  (car line)))
  (if (not (= row 1))
	(get-position (- row 1) col (cdr board))
	(get-col col (car board))))

(define (safe? k positions)
  (define (col-ok?)
	(define (col-sum col board)
	  (if (null? board)
		0
		(+ (get-position 1 col board)
		   (col-sum col (cdr board)))))
	(= (col-sum k positions) 1))
  (define (get-row board)
	(if (= (get-position 1 k board) 1)
	  1
	  (+ (get-row (cdr board))
		 1)))
  (define (row-ok?)
	(define (row-sum row board)
	  (define (sum line)
		(if (null? line)
		  0
		  (+ (car line) (sum (cdr line)))))
	  (if (not (= row 1))
		(row-sum (- row 1) (cdr board))
		(sum (car board))))
	(= (row-sum (get-row positions) positions) 1))
  (define (left-up-ok?)
	(define (check row col)
	  (if (or (= row 0) (= col 0))
		#t
		(if (= (get-position row col positions) 1)
		  #f
		  (check (- row 1) (- col 1)))))
	(let ((row (get-row positions))
		  (col k))
	  (check (- row 1) (- col 1))))
  (define (left-down-ok?)
	(define (check row col)
	  (if (or (> row board-size) (= col 0))
		#t
		(if (= (get-position row col positions) 1)
		  #f
		  (check (+ row 1) (- col 1)))))
	(let ((row (get-row positions))
		  (col k))
	  (check (+ row 1) (- col 1))))
  (and (col-ok?) (row-ok?) (left-up-ok?) (left-down-ok?)))

(define (adjoin-position new-row new-col union)
  (define (set-col k line)
	(if (not (= k 1))
	  (cons (car line)
			(set-col (- k 1) (cdr line)))
	  (cons 1 (cdr line))))
  (if (not (= new-row 1))
	(cons (car union)
		  (adjoin-position (- new-row 1) new-col (cdr union)))
	(cons (set-col new-col (car union))
		  (cdr union))))

(define (queens board-size)
  (define (queen-cols k)
	(if (= k 0)
	  (list empty-board)
	  (filter
		(lambda (positions) (safe? k positions))
		(flatmap
		  (lambda (rest-of-queens)
			(map (lambda (new-row)
				   (adjoin-position new-row k rest-of-queens))
				 (enumerate-interval 1 board-size)))
		  (queen-cols (- k 1))))))
  (queen-cols board-size))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;output methods;;;;;;;;;;;;;;;;;;;;;;;;;
(define (output-board board)
  (if (not (null? board))
	(begin
	 (display (car board))
	 (newline)
	 (output-board (cdr board)))
	))

(define (output results)
  (if (not (null? results))
	(begin
	  (output-board (car results))
	  (newline)
	  (output (cdr results)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;test for 2.43;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (queens board-size)
  ;(define (queen-cols k)
	;(if (= k 0)
	  ;(list empty-board)
	  ;(filter
		;(lambda (positions) (safe? k positions))
		;(flatmap
		  ;(lambda (new-row)
			;(map (lambda (rest-of-queens)
				   ;(adjoin-position new-row k rest-of-queens))
				 ;(queen-cols (- k 1))))
		  ;(enumerate-interval 1 board-size)))))
  ;(queen-cols board-size))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;final result;;;;;;;;;;;;;;;;;;;;;;;;;
(define board-size 7)
(define empty-board (build-empty-board board-size))
(define results (queens board-size))
(output results)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
