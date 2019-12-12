#lang racket

(define (parse-input lines)
  (define (parse-line line)
    (list->vector
     (map string->number
          (cdr
           (regexp-match (pregexp "<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>") line)))))
  (list->vector (map parse-line lines)))

(define (vector-add v1 v2)
  (vector-map + v1 v2))

(define (vector-sub v1 v2)
  (vector-map - v1 v2))

(define (gdelta v1 v2)
  (vector-map (lambda (x1 x2)
                (cond
                  [(< x1 x2) 1]
                  [(= x1 x2) 0]
                  [(> x1 x2) -1]))
              v1 v2))

(define (vector-update! vec i f)
  (vector-set! vec i (f (vector-ref vec i))))

(define (gravity! positions velocities)
  (for ([pair (in-combinations (list 0 1 2 3) 2)])
    (define delta (gdelta (vector-ref positions (car pair))
                          (vector-ref positions (cadr pair))))
    (vector-update! velocities (car pair) (lambda (v)
                                            (vector-add v delta)))
    (vector-update! velocities (cadr pair) (lambda (v)
                                             (vector-sub v delta))))
  (for ([i (in-range 4)])
    (vector-update! positions i (lambda (p)
                                  (vector-add p (vector-ref velocities i))))))

(define (energy positions velocities)
  (define (abs-sum v)
    (foldl + 0 (vector->list (vector-map abs v))))
  (for/fold ([sum 0])
            ([p (in-vector positions)]
             [v (in-vector velocities)])
    (+ sum (* (abs-sum p) (abs-sum v)))))

(define (simulate init-pos n)
  (define positions (vector-copy init-pos))
  (define velocities (make-vector 4 (vector 0 0 0)))
  (for ([_ (in-range n)])
    (gravity! positions velocities))
  (values positions velocities (energy positions velocities)))

(module+ test
  (require rackunit)

  (let ([test (parse-input (string-split "<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>" "\n"))])
    (let-values ([(p v energy) (simulate test 10)])
      (check-eq? energy 179))
    (check-eq? (periods test) 2772))

  (let ([test (parse-input (string-split "<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>" "\n"))])
    (let-values ([(p v energy) (simulate test 100)])
      (check-eq? energy 1940))))

(define input (parse-input (string-split "<x=17, y=-7, z=-11>
<x=1, y=4, z=-1>
<x=6, y=-2, z=-6>
<x=19, y=11, z=9>" "\n")))

(simulate input 1000)


;; Needed a hint from Reddit to realize that the gravity per axis is independent
;; (e.g. gravity for x-axis only depends on x-positions). So we just need to find
;; when each axis loops around and then take the lowest common multiplier

(define (periods init-pos [n +inf.0])
  (define positions (vector-copy init-pos))
  (define velocities (make-vector 4 (vector 0 0 0)))
  (define (start-state? n)
    (andmap (lambda (init pos vel)
              (and
               (= (vector-ref init n) (vector-ref pos n))
               (= (vector-ref vel n) 0)))
            (vector->list init-pos)
            (vector->list positions)
            (vector->list velocities)))
  (let loop ([i 1]
             [x #f]
             [y #f]
             [z #f])
    (gravity! positions velocities)
    (if (or (and x y z ) (= i n))
        (lcm x y z)
        (loop (add1 i)
              (or x (if (start-state? 0) i #f))
              (or y (if (start-state? 1) i #f))
              (or z (if (start-state? 2) i #f))))))

(periods input)












































