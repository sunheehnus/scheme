(define (product term a next b)
  (if (> a b)
	1
	(* (term a) (product term (next a) next b))))

(define (factorial n)
  (define (inc x) (+ x 1))
  (define (self x) x)
  (product self 1 inc n))

(define (pi)
  (define ceil 1000)
  (define (square x) (* x x))
  (define (next x) (+ x 2))
  (* (/ (product square 4 next ceil) ceil (product square 3 next ceil)) 2.0 4))

(display (factorial 5))
(newline)
(display (pi))
(newline)
