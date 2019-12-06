#lang racket

(require rackunit
         threading
         "../utils.rkt")

(module+ test
  (define test "COM)B
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
  (check-eq? (part1 test) 42))


(define (make-digraph input)
  (define orbits (map (lambda (s) (string-split s ")")) (string-split input "\n")))
  (for/fold ([graph (hash)])
            ([o (in-list orbits)])
    (hash-update graph (car o) (curry cons (cadr o)) '())))

(define (checksum graph)
  (let loop ([node "COM"]
             [count 0])
    (if (hash-has-key? graph node)
        (foldl (lambda (n total)
                 (+ total (loop n (add1 count)))) count (hash-ref graph node))
        count)))

(define (part1 s)
  (checksum (make-digraph s)))

(define (make-graph input)
  (define orbits (map (lambda (s) (string-split s ")")) (string-split input "\n")))
  (for/fold ([graph (hash)])
            ([o (in-list orbits)])
    (~> graph
        (hash-update (car o) (curry cons (cadr o)) '())
        (hash-update (cadr o) (curry cons (car o)) '()))))

(define (calc-dists graph [root "YOU"])
  (define q (deque))
  (push-end! q (cons root 0))
  (let loop ([dists (hash)])
    (if (deque-empty? q)
        dists
        (match-let ([(cons n dist) (pop-front! q)])
          (if (hash-has-key? dists n)
              (loop dists)
              (let ()
                (for ([adj (in-list (hash-ref graph n))])
                  (push-end! q (cons adj (add1 dist))))
                (loop (hash-set dists n dist))))))))

(define (part2 s)
  (define dists (calc-dists (make-graph s)))
  (- (hash-ref dists "SAN") 2))

(module+ test
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

(define input (file->string "input6.txt"))
(part1 input)
(part2 input)

