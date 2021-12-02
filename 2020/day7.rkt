#lang racket

(module+ test
  (require rackunit)
  (define test (string-split "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags." "\n")))

(define (parse-rules rules)
  (define matcher (pregexp "(.*) bags contain (no other bags|.*)"))
  (define bag-matcher (pregexp "(\\d+) (.*) bag"))
  (for/hash ([rule (in-list rules)])
    (match-define (list _ bag contents)  (regexp-match matcher rule))
    (if (equal? contents "no other bags")
        (values bag '())
        (let* ([contents (for/list ([bag (in-list (string-split contents ", "))])
                           (match-define (list _ n type)(regexp-match bag-matcher bag))
                           (cons (string->number n) type))])
          (values bag contents)
        ))))

(define (pivot-tree tree root)
  (let loop ([node root]
             [result (hash)])
    (define children (for*/list ([(bag contents) (in-hash tree)]
                                 [types (in-value (list->set (map cdr contents)))]
                                 #:when (set-member? types node))
                       bag))
    (if (empty? children)
        (hash-set result node '())
        (for/fold ([result (hash-set result node children)])
                  ([child (in-list children)])
          (loop child result)))))

(define (part1 rules)
  (define tree (pivot-tree (parse-rules rules) "shiny gold"))
  (sub1 (hash-count tree)))

(define input (file->lines "input7.txt"))

(module+ test
  (check-equal? (part1 test) 4))
(part1 input)

;;; Part 2

(define (count-bags tree root)
  (define children (hash-ref tree root))   
  (for/fold ([count 1])
            ([child (in-list children)])
    (match-define (cons num type) child)
    (+ count (* num (count-bags tree type)))))

(define (part2 rules)
  (sub1 (count-bags (parse-rules rules) "shiny gold")))

(module+ test
  (check-equal? (part2 test) 32)

  (define test2 (string-split  "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags." "\n"))

  (check-equal? (part2 test2) 126))

(part2 input)
