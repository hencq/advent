#lang racket

(require "../utils.rkt")

(define input "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"
)

(define test (read-area input))

(define (count-trees area right down)
  (define cols (add1 (apply max (map car (hash-keys area)))))
  (define rows (add1 (apply max (map cdr (hash-keys area)))))
  (for/sum ([x (in-range 0 +inf.0 right)]
            [y (in-range 0 rows down)]
            #:when (eq? #\# (hash-ref area (cons (modulo x cols) y))))
    1))

(define day3 (read-area (file->lines "input3.txt")))

(count-trees test 3 1)
(define (part2 forest)
  (apply *
         (map (match-lambda [(list x y) (count-trees forest x y)])
              '((1 1) (3 1) (5 1) (7 1) (1 2)))))

(part2 test)

(part2 day3)

