#lang racket
(require data/queue
         rackunit
         threading)



(define (digit100 number)
  (define milli (/ number 1000))
  (floor (* 10 (- milli (floor milli)))))

(define (power-level x y serial)
  (define rackid (+ x 10))
  (~> (* rackid y)
      (+ serial)
      (* rackid)
      (digit100)
      (- 5)))

(module+ test
  (check-eq? (power-level 3 5 8) 4)
  (check-eq? (power-level 122 79 57) -5)
  (check-eq? (power-level 217 196 39) 0)
  (check-eq? (power-level 101 153 71) 4))


(define (build-grid left top width height serial)
  (for/list ([y (in-range top (+ top height))])
    (for/list ([x (in-range left (+ left width))])
      (power-level x y serial))))

(define (grid-ref grid x y)
  (if (or (< x 0) (< y 0))
      0
      (vector-ref (vector-ref grid y) x)))

(define (build-sum-grid width height serial)
  (define grid 
    (for/vector ([row height])
      (make-vector width 0)))
  ;; cumulative rows
  (for* ([y height]
         [x width])
    (vector-set! (vector-ref grid y) x (+ (power-level (add1 x) (add1 y) serial)
                                          (grid-ref grid (sub1 x) y))))
  ;; sum the cols
  (for* ([y (in-range 1 height)]
         [x width])
    (vector-set! (vector-ref grid y) x (+ (vector-ref (vector-ref grid y) x)
                                          (vector-ref (vector-ref grid (sub1 y)) x))))
  grid)

(define (square-sum grid x y size)
  (+ (grid-ref grid (sub1 x) (sub1 y))
     (- (grid-ref grid (+ x (sub1 size)) (+ y (sub1 size)))
        (grid-ref grid (+ x (sub1 size)) (sub1 y))
        (grid-ref grid (sub1 x) (+ y (sub1 size))))))

(define (max-sum width height serial #:square [square #f])
  (define grid (build-sum-grid width height serial))
  (define-values (min-size max-size)
    (if square
        (values square (add1 square))
        (values 1 (min height width))))
  (for*/fold ([max -inf.0]
              [mx 0]
              [my 0]
              [ms 0]
              #:result (values (add1 mx) (add1 my) ms max))
             ([size (in-range min-size max-size)])
    (define-values (sum x y)
      (for*/fold ([max -inf.0]
                  [mx #f]
                  [my #f])
                 ([y (- height size)]
                  [x (- width size)])
        (define sum (square-sum grid x y size))
        (if (> sum max)
            (values sum x y)
            (values max mx my))))
    (if (> sum max)
        (values sum x y size)
        (values max mx my ms))))

(module+ test
  (let-values ([(x y _ lvl) (max-sum 300 300 18 #:square 3)])
    (check-eq? x 33)
    (check-eq? y 45)
    (check-eq? lvl 29))

  (let-values ([(x y s lvl) (max-sum 300 300 18)])
    (check-eq? x 90)
    (check-eq? y 269)
    (check-eq? s 16)
    (check-eq? lvl 113))
  )


