(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
	(let ((m (frame-coord-map frame)))
	  (let ((new-origin (m origin)))
		(painter
		  (make-frame new-origin
					  (sub-vect (m corner1) new-origin)
					  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
					 (make-vect 0.0 1.0)
					 (make-vect 1.0 1.0)
					 (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
					 (make-vect 0.5 0.5)
					 (make-vect 1.0 0.5)
					 (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
					 (make-vect 1.0 0.0)
					 (make-vect 1.0 1.0)
					 (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
					 (make-vect 0.0 0.0)
					 (make-vect 0.65 0.35)
					 (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (lambda (frame)
	((transform-painter painter1
						(make-vect 0.0 0.0)
						(make-vect 0.5 0.0)
						(make-vect 0.0 1.0)) frame)
	((transform-painter painter2
						(make-vect 0.5 0.0)
						(make-vect 1.0 0.0)
						(make-vect 0.5 1.0)) frame)))

(define (flip-horiz painter)
  (transform-painter painter
					 (make-vect 1.0 0.0)
					 (make-vect 0.0 0.0)
					 (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
					 (make-vect 1.0 1.0)
					 (make-vect 0.0 1.0)
					 (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
					 (make-vect 0.0 1.0)
					 (make-vect 0.0 0.0)
					 (make-vect 1.0 1.0)))

(define (below painter1 painter2)
  (lambda (frame)
	((transform-painter painter1
						(make-vect 0.0 0.0)
						(make-vect 1.0 0.0)
						(make-vect 0.0 0.5)) frame)
	((transform-painter painter2
						(make-vect 0.0 0.5)
						(make-vect 1.0 0.5)
						(make-vect 0.0 1.0)) frame)))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
