#lang racket

(require "../intcode.rkt"
         graph)

(define robot (new-prog (file->string "input15.txt")))

(define (move pos dir)
  (match-define (cons x y) pos)
  (case dir
    [(1) (cons x (add1 y))]
    [(2) (cons x (sub1 y))]
    [(3) (cons (sub1 x) y)]
    [(4) (cons (add1 x) y)]))

(define (back dir)
  (case dir
    [(1) 2]
    [(2) 1]
    [(3) 4]
    [(4) 3]))

(define (explore robot)
  (restart! robot)
  (define goal #f)
  (define graph
    (let walk ([graph (hash)]
               [pos (cons 0 0)]
               [prev #f])
      (if (hash-has-key? graph pos)
          graph
          (let* ([dirs (filter (lambda (d)
                                 (or (not prev)
                                     (not (= d prev))))
                               (list 1 2 3 4))]
                 [newgraph (for/fold ([graph (hash-set graph pos '())])
                                     ([d (in-list dirs)])
                             (define sq (car (run! robot (list d))))
                             (case sq
                               [(0) graph] ;wall
                               [(1) (walk (hash-update graph pos (curry cons (move pos d)))
                                          (move pos d) (back d))]
                               [(2) (set! goal (move pos d))
                                    (walk (hash-update graph pos (curry cons (move pos d)))
                                          (move pos d) (back d))]))])
            (when prev
              (run! robot (list prev)))
            newgraph))))
  (values graph goal))

(define (distances robot)
  (define-values (tree goal) (explore robot))
  (define graph (unweighted-graph/undirected 
                 (append*
                  (for/list ([(pos adjs) (in-hash tree)])
                    (for/list ([a (in-list adjs)])
                      (list pos a))))))
  (define-values (dists preds) (bfs graph goal))
  dists)

(define (shortest-path robot)
  (define dists (distances robot))
  (hash-ref dists (cons 0 0)))

(shortest-path robot)

(define (fill-time robot)
  (define dists (distances robot))
  (apply max (hash-values dists)))

(fill-time robot)
