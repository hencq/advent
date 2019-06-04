#lang racket
(require rackunit)

(define (read-input lines)
  (for/vector ([line (in-list lines)])
    (for/vector ([c (in-string line)])
      c)))

(define (display-field field)
  (for ([row (in-vector field)])
    (for ([ch (in-vector row)])
      (display ch))
    (displayln "")))

(define (field-ref field x y)
  (vector-ref (vector-ref field y) x))

(define (adjacent field x y)
  (define height (vector-length field))
  (define width (vector-length (vector-ref field 0)))
  (for*/list ([x* (in-range (sub1 x) (+ 2 x))]
              [y* (in-range (sub1 y) (+ 2 y))]
              #:unless (or (< x* 0) (>= x* width))
              #:unless (or (< y* 0) (>= y* height))
              #:unless (and (= x* x) (= y* y)))
    (field-ref field x* y*)))

(define (count-eqv xs y)
  (length (filter (curry eqv? y) xs)))

(define (step field)
  (for/vector ([y (in-range (vector-length field))])
    (for/vector ([x (in-range (vector-length (vector-ref field y)))])
      (define sq (field-ref field x y))
      (define adjs (adjacent field x y))
      (cond
        [(eqv? sq #\.) (if (>= (count-eqv adjs #\|) 3)
                           #\|
                           #\.)]
        [(eqv? sq #\|) (if (>= (count-eqv adjs #\#) 3)
                           #\#
                           #\|)]
        [(eqv? sq #\#) (if (and (>= (count-eqv adjs #\#) 1)
                                (>= (count-eqv adjs #\|) 1))
                           #\#
                           #\.)]
        [else sq]))))

(define (count-resources field)
  (for*/fold ([trees 0]
              [yards 0])
             ([row (in-vector field)]
              [sq (in-vector row)])
    (cond
      [(eqv? sq #\|) (values (add1 trees) yards)]
      [(eqv? sq #\#) (values trees (add1 yards))]
      [else (values trees yards)])))

(define (resource-value field [n 10])
  (define new-field
    (for/fold ([field field])
              ([i (in-range n)])
      (step field)))
  (define-values (t y) (count-resources new-field))
  (* t y))

(module+ test
  (define test (read-input (list
                          ".#.#...|#."
                          ".....#|##|"
                          ".|..|...#."
                          "..|#.....#"
                          "#.#|||#|#|"
                          "...#.||..."
                          ".|....|..."
                          "||...#|.#|"
                          "|.||||..|."
                          "...#.|..|.")))
  (check-eq? (resource-value test) 1147))


;; Part 1
;; (define input (read-input (file->lines "day18_input.txt")))
;; (resource-value input)

(module+ puzzle
  (define input (read-input (file->lines "day18_input.txt")))
  (resource-value input))


;; Part 2
;; Notice that after a while the patterns start iterating.
;; By looking at scores that iterate multiple times we can find out the period
;; by subtracting 2 iterations. We then look at the scores by remainder
;; For our example it turns out to be 28
;; (remainder 1000000000 28) is 20, so we just do:
;; (hash-ref (modulo-scores input) 20)

(define (modulo-scores field)
  (define scores
    (for/fold ([field field]
               [scores (hash)]
               #:result scores)
              ([i (in-range 1000)])
      (define new-field (step field))
      (let-values ([(t y) (count-resources new-field)])
        (values new-field (hash-update scores (* t y) (curry cons (add1 i)) '())))))
  (for/hash ([(score iterations) (in-hash scores)]
             #:when (> (length iterations) 1))
    (define a (car iterations))
    (define b (cadr iterations))
    (values (remainder a (- a b)) score)))

(module+ puzzle
  (hash-ref (modulo-scores input) 20))
