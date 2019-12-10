#lang racket

(require rackunit)

(module+ test
  (let ([test ".#..#
.....
#####
....#
...##"])
    (check-equal? (part1 test) (cons (cons 3 4) 8)))

  (let ([test ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"])
    (check-equal? (part1 test) (cons (cons 11 13) 210))
    (define order (ordering test))
    (check-equal? (list-ref order 0) (cons 11 12))
    (check-equal? (list-ref order 1) (cons 12 1))
    (check-equal? (list-ref order 9) (cons 12 8))
    (check-equal? (list-ref order 199) (cons 8 2))
    (check-eq? (length order) 299)))

(define (locations input)
  (append*
   (for/list ([row (in-list (string-split input "\n"))]
              [y (in-naturals)])
     (for/list ([col (in-list (string->list row))]
                [x (in-naturals)]
                #:when (char=? col #\#))
       (cons x y)))))

(define (lines-of-sight asteroids)
  (for/hash ([a (in-list asteroids)])
    (define angles
      (for/list ([b (in-list asteroids)]
                 #:when (not (equal? a b)))
        (define dx (- (car a) (car b)))
        (define dy (- (cdr a) (cdr b)))
        (define angle (* -1 (atan dx dy)))
        (define magnitude (sqrt (+ (expt dx 2) (expt dy 2))))
        (when (< angle 0) (set! angle (+ angle (* 2 pi))))
        (list angle magnitude b)))
    (values a angles)))

(define (visible angles)
  (sort
   (for/list ([(x as) (in-hash angles)])
     (cons x (length (group-by car as =))))
   > #:key cdr))

(define (part1 input)
  (car
   (visible
    (lines-of-sight (locations input)))))

(define (ordering input)
  (define coord (car (part1 input)))
  (define lines (group-by car (hash-ref (lines-of-sight (locations input)) coord)))
  (define spiral (append* (for/list ([l (in-list lines)])
                            (for/list ([asteroid (in-list (sort l < #:key cadr))]
                                       [i (in-naturals)])
                              (cons (+ (car asteroid)
                                       (* 2 pi i))
                                    (caddr asteroid))))))
  (map cdr (sort spiral < #:key car)))

;;part1
(part1 (file->string "input10.txt"))
;;part 2
(list-ref (ordering (file->string "input10.txt")) 199)


