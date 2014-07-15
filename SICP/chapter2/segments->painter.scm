(define (segment->painter segment-list)
  (lambda (frame)
	(for-each
	  (lambda (segment)
		(draw-line
		  ((frame-coord-map frame) (start-segment segment))
		  ((frame-coord-map frame) (end-segment segment))))
	  segment-list)))

(define (make-vect x y)
  (cons x y))

(define (make-segment xs ys xe ye)
  (cons (make-vect xs ys)
		(make-vect xe ye)))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define frame-self
  (make-frame (make-vect 0.0 0.0)
			  (make-vect 1.0 0.0)
			  (make-vect 0.0 1.0)))

(define left-bottom2top (make-segment 0.0 0.0 0.0 1.0))
(define right-bottom2top (make-segment 1.0 0.0 1.0 1.0))
(define down-left2right (make-segment 0.0 0.0 1.0 0.0))
(define up-left2right (make-segment 0.0 1.0 1.0 1.0))
(define ((draw-boarder (list left-bottom2top right-bottom2top down-left2right up-left2right)) frame-self))

; and so on
