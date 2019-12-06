#lang racket

(require graph
         rackunit)

(define (input->dists input from)
  (define-values (dists _)
    (bfs
     (unweighted-graph/undirected (map (lambda (s)
                                         (string-split s ")"))
                                       (string-split input "\n")))
     from))
  dists)

(define (part1 input)
  (apply + (hash-values (input->dists input "COM"))))

(define (part2 s)
  (define dists (input->dists s "YOU"))
  (- (hash-ref dists "SAN") 2))

(define input (file->string "input6.txt"))
(part1 input)
(part2 input)


(module+ test
  (define test1 "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")
    (check-eq? (part1 test1) 42)
  (define test2 "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")
  (check-eq? (part2 test2) 4))

