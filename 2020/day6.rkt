#lang racket

(define test (string-split "abc

a
b
c

ab
ac

a
a
a
a

b" "\n"))

(define (split-by xs pred)
  (let loop ([result '()]
             [current '()]
             [xs xs])
    (if (empty? xs)
        (reverse (cons (reverse current) result))
        (let ([x (car xs)])
          (if (pred x)
              (loop (cons (reverse current) result) '() (cdr xs))
              (loop result (cons x current) (cdr xs)))))))

(define (read-groups lines)
  (for/list ([group (split-by lines (lambda (l) (equal? l "")))])
    (for*/set ([person (in-list group)]
                [q (in-list (string->list person))])
      q)))

(define (part1 lines)
  (apply + (map set-count (read-groups lines))))

(part1 test)

(define input (file->lines "input6.txt"))

(part1 input)

(define (read-common-answers lines)
  (for/list ([group (split-by lines (lambda (l) (equal? l "")))])
    (for/fold ([qs (list->set (string->list (car group)))])
              ([person (in-list (cdr group))])
      (set-intersect qs (list->set (string->list person))))))

(define (part2 lines)
  (apply + (map set-count (read-common-answers lines))))

(part2 test)
(part2 input)
