#lang racket
(require data/heap
         rackunit)

(struct matrix (vec rows cols target-x target-y))

(define (matrix-ref mat x y)
  (vector-ref (matrix-vec mat) (+ (* y (matrix-cols mat)) x)))

(define (matrix-set! mat x y val)
  (vector-set! (matrix-vec mat) (+ (* y (matrix-cols mat)) x) val))

(define (cave target-x target-y depth)
  (define-values (cols rows) (values (+ 50 target-x) (+ 50 target-y)))
  (define mat (matrix (make-vector (* rows cols) 0) rows cols target-x target-y))
  (for* ([y (in-range rows)]
         [x (in-range cols)])
    (define index (match/values (values x y)
                    [(0 0) 0]
                    [((? (curry = target-x)) (? (curry = target-y))) 0]
                    [(x 0) (* x 16807)]
                    [(0 y) (* y 48271)]
                    [(x y) (* (matrix-ref mat (sub1 x) y) (matrix-ref mat x (sub1 y)))]))
    (define erosion (modulo (+ index depth) 20183))
    (matrix-set! mat x y erosion))
  (for* ([y (in-range rows)]
         [x (in-range cols)])
    (define danger (modulo (matrix-ref mat x y) 3))
    (matrix-set! mat x y danger))
  mat)

(define (total-danger cave)
  (for*/fold ([sum 0])
            ([x (in-range (add1 (matrix-target-x cave)))]
             [y (in-range (add1 (matrix-target-y cave)))])
    (+ sum (matrix-ref cave x y))))

(module+ test
  (check-eq? (total-danger (cave 10 10 510)) 114))

(define (show-map cave)
  (for ([y (in-range (matrix-cols cave))])
    (for ([x (in-range (matrix-rows cave))])
      (display
       (case (matrix-ref cave x y)
         [(0) "."]
         [(1) "="]
         [(2) "|"])))
    (displayln "")))


;;Part 2

(define tools (vector (list 'torch 'climb)
                      (list 'climb 'none)
                      (list 'torch 'none)))

(struct node (x y tool) #:transparent)

(define (adjacent cave x y)
  (apply append
         (for/list ([dx (in-list (list (sub1 x) x (add1 x) x))]
                    [dy (in-list (list y (sub1 y) y (add1 y)))]
                    #:when (>= dx 0)
                    #:when (>= dy 0)
                    #:when (< dx (matrix-cols cave))
                    #:when (< dy (matrix-rows cave)))
           (define from-type (matrix-ref cave x y))
           (define to-type (matrix-ref cave dx dy))
           (for/list ([tool (in-list (vector-ref tools to-type))]
                      #:when (set-member? (vector-ref tools from-type) tool))
             (node dx dy tool)))))

(define (cost from to)
  (if (eqv? (node-tool from) (node-tool to))
      1
      8))

(define (find-path cave)
  (define (dist<=? n1 n2)
    (<= (cdr n1) (cdr n2)))
  (define target (node (matrix-target-x cave)
                       (matrix-target-y cave)
                       'torch))
  (define start (node 0 0 'torch))
  (define nodes (make-heap dist<=?))
  (define dists (make-hash (list (cons start 0))))
  (heap-add! nodes (cons start 0))
  (let loop ([seen (set)])
    (if (= (heap-count nodes) 0)
        (hash-ref dists target #f)
        (let* ([node (car (heap-min nodes))]
               [dist (hash-ref dists node)])
          (heap-remove-min! nodes)
          (cond
            [(set-member? seen node) (loop seen)]
            [(equal? node target) (hash-ref dists target)]
            [else (for ([adj (in-list (adjacent cave (node-x node) (node-y node)))]
                        #:unless (set-member? seen adj))
                    (define alt (+ dist (cost node adj)))
                    (when (< alt (hash-ref dists adj +inf.0))
                      (hash-set! dists adj alt)
                      (heap-add! nodes (cons adj alt))))
                  (loop (set-add seen node))])))))

(module+ test
  (define c (cave 10 10 510))
  (define-values (t prev) (find-path c))
  (check-eq? t 45))

(define (show-path prev x y)
  (define target (node x y 'torch))
  (let loop ([path (list target)])
    (if (hash-has-key? prev (car path))
        (loop (cons (hash-ref prev (car path)) path))
        path)))
