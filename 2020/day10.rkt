#lang racket

(define test (list 16
                   10
                   15
                   5
                   1
                   11
                   7
                   19
                   6
                   12
                   4))

(define test2 (list 28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3))
(define (built-in-rating jolts)
  (+ 3 (apply max jolts)))

(built-in-rating test)

(define (find-chain jolts)
  (define sorted (sort jolts <))
  (for/fold ([last (car sorted)]
             [steps (hash 3 1 (car sorted) 1)]
             #:result (* (hash-ref steps 1) (hash-ref steps 3)))
            ([j (in-list (cdr sorted))])
    (define step (- j last))
    (values j (hash-update steps step add1 0))))

(find-chain test)

(define input (map string->number (file->lines "input10.txt")))

(find-chain input)

;; part 2

(define (neighbors jolts)
  (define input (car jolts))
  (for/list ([jt (in-list (cdr jolts))]
             #:break (> (- jt input) 3)
             #:when (set-member? (set 1 2 3) (- jt input)))
    jt))

(define (build-graph jolts)
  (define sorted (sort jolts <))
  (let loop ([todo (cons 0 sorted)]
             [graph (hash)]) 
    (if (empty? todo)
        graph
        (loop (cdr todo) (hash-set graph (car todo) (neighbors todo))))))

(define (count-paths graph [start 0])
  (define cache (make-hash))
  (let walker ([start start])
    (if (hash-has-key? cache start)
        (hash-ref cache start)
        (let ([adjs (hash-ref graph start)])
          (if (empty? adjs)
              1
              (let ([paths (for/fold ([sum 0])
                                      ([x (in-list adjs)])
                             (+ sum (walker x)))])
                (hash-set! cache start paths)
                paths))))))
(count-paths (build-graph input))

