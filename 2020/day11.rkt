#lang racket

(require "../utils.rkt")

(define test (string-split "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
"\n"))

(define test-area (read-area test ))
(area-dimensions test-area)

(define (adj-occupied area sq)
  (define x (car sq))
  (define y (cdr sq))
  (for/sum ([dx (in-list (list -1  0  1 1 1 0 -1 -1))]
            [dy (in-list (list -1 -1 -1 0 1 1  1  0))]
            #:when (equal? (hash-ref area (cons (+ x dx) (+ y dy)) #f) #\#))
    1))

(adj-occupied test-area (cons 0 0))
(define (seat-step area [adj-fun adj-occupied] [max-adj 4])
  (for/hash ([(k v) (in-hash area)])
    (cond
      [(and (eq? v #\L) (= (adj-fun area k) 0)) (values k #\#)]
      [(and (eq? v #\#) (>= (adj-fun area k) max-adj)) (values k #\L)]
      [else (values k v)]))) 

(define (seat-run area [adj-fun adj-occupied] [adj-max 4] #:max [max #f])
  (let loop ([area area]
             [i 0])
    (if (and max (> i max))
        area
        (let ([new-area (seat-step area adj-fun adj-max)])
          (if (equal? area new-area)
              new-area
              (loop new-area (add1 i)))))))

(define (count-occupied area)
  (length (filter (lambda (s) (eq? s #\#)) (hash-values area))))
(print-area (seat-step (seat-step test-area)))
(print-area test-area)
(adj-occupied (seat-step test-area) (cons 3 0))

(print-area (seat-run test-area))

(count-occupied (seat-run test-area))
(define input (read-area (file->lines "input11.txt")))

(count-occupied (seat-run input))

;;(count-occupied (seat-run input 1))

;; part 2

(define (cast-ray area sq dir)
  (let loop ([sq sq])
    (define new-sq (cons (+ (car sq) (car dir)) (+ (cdr sq) (cdr dir))))
    (if (not (hash-has-key? area new-sq))
        0
        (let ([occ (hash-ref area new-sq)])
          (cond
            [(eq? occ #\#) 1]
            [(eq? occ #\L) 0]
            [(eq? occ #\.) (loop new-sq)])))))

(define (count-cast-adj area sq)
  (for/sum ([i (in-list (list -1  0  1 1 1 0 -1 -1))]
            [j (in-list (list -1 -1 -1 0 1 1  1 0))])
    (cast-ray area sq (cons i j))))

(count-occupied (seat-run input count-cast-adj 5))

(define test-area2 (read-area (string-split ".......#.
...#.....
.#.......
.........
..#L....#
....#....
.........
#........
...#....." "\n")))

(define test-area3 (read-area (string-split ".##.##.
#.#.#.#
##...##
...L...
##...##
#.#.#.#
.##.##." "\n")))

(define test-area4 (read-area))
