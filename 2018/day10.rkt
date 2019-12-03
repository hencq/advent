#lang racket
(require rackunit
         threading)

(struct point (x y dx dy) #:transparent)

(define (read-data fname)
  (define regex
    (pregexp "position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>"))
  (for/list ([line (in-list (file->lines fname))])
    (apply point (map string->number (cdr (regexp-match regex line))))))

(define points (read-data "day10_test.txt"))

(define (step-point pt [steps 1])
  (define new-x (+ (point-x pt) (* steps (point-dx pt))))
  (define new-y (+ (point-y pt) (* steps (point-dy pt))))
  (struct-copy point pt [x new-x] [y new-y]))

(define (get-canvas-size points)
  (define xs (map point-x points))
  (define ys (map point-y points))
  (define-values (x y x2 y2) 
    (values (apply min xs) (apply min ys) (apply max xs) (apply max ys)))
  (values x y (add1 (- x2 x)) (add1 (- y2 y))))

(define (make-canvas points)
  (define-values (x y w h) (get-canvas-size points))
  (define canvas (for/vector ([j h])
                   (make-vector w #f)))
  (for ([p (in-list points)])
    (vector-set! (vector-ref canvas (- (point-y p) y)) (- (point-x p) x) #t))
  canvas)

(define (display-canvas canvas)
  (for ([j (vector-length canvas)])
    (define row (vector-ref canvas j))
    (for ([i (vector-length row)])
      (if (vector-ref row i)
          (display "#")
          (display ".")))
    (displayln "")))

(define (display-points points)
  (~> points
      (make-canvas)
      (display-canvas))
  points)

(define (step points [steps 1])
  (map (lambda (p) (step-point p steps)) points))

(define display-progression
  (case-lambda
    [(points start steps)
     (let ([points (step points start)])
       (for/fold ([points points])
                 ([i steps])
         (display-points points)
         (displayln "")
         (step points)))
     (void)]
    [(points steps)
     (display-progression points 0 steps)]))

(define puzzle (read-data "day10_input.txt"))

;; Notice how the area gets smaller
(define (find-min-area points)
  (define (get-area points)
    (define-values (x y w h) (get-canvas-size points))
    (* w h))
  (let loop ([points points]
             [area (get-area points)]
             [i 0])
    (define next (step points))
    (define next-area (get-area next))
    (if (< next-area area)
        (loop next next-area (add1 i))
        i)))
