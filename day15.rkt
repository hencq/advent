#lang racket
(require graph
         rackunit)

(struct game (map players graph) #:transparent)

(define (read-map fname)
  (define lines (file->lines fname))
  (define height (length lines))
  (define width (string-length (car lines)))
  (define players '())
  (define gmap
    (for/vector ([l (in-list lines)]
                 [y (in-naturals)])
      (for/vector ([ch (in-list (string->list l))]
                   [x (in-naturals)])
        (cond
          [(or (eq? ch #\G)
               (eq? ch #\E)) (begin
                               (set! players (cons (cons (cons x y) ch) players))
                               #\.)]
          [else ch]))))

  (define edges
    (for*/list ([x width]
                [y height])
      (cons (cons x y)
            (for/list ([dx (in-list (list (- x 1) x (+ x 1) x))]
                       [dy (in-list (list y (- y 1) y (+ y 1)))]
                          #:unless (or (< dx 0)
                                       (< dy 0)
                                       (>= dx width)
                                       (>= dy height))
                          #:when (eq? #\. (vector-ref (vector-ref gmap dy) dx)))
                (cons dx dy)))))
  (define graph (unweighted-graph/adj edges))
  (game gmap players graph))

(define test-state (read-map "day15_test.txt"))

(define (loc<=? l1 l2)
  (or
   (< (cdr l1) (cdr l2))
   (and (= (cdr l1) (cdr l2))
        (<= (car l1) (car l2)))))

(define (sort-players players)
  (sort players (lambda (p1 p2) (loc<=? (car p1) (car p2)))))

(define (floor-square? game loc)
  (and (>= (car loc) 0)
       (>= (cdr loc) 0)
       (< (car loc) (vector-length (game-map game)))
       (< (cdr loc) (vector-length (vector-ref (game-map game) 0)))
       (eq? (vector-ref (vector-ref (game-map game) (cdr loc)) (car loc)) #\.)))

(define (square-free? game loc)
  (and (floor-square? game loc)
       (not (dict-ref (game-players game) loc #f))))

(define (adjacent game loc)
  (define-values (x y) (values (car loc) (cdr loc)))
  (define potential (list (cons       x (sub1 y))
                          (cons (sub1 x)      y)
                          (cons (add1 x)      y)
                          (cons       x (add1 y))))
  (filter (curry square-free? game) potential))

(define (add-source game base-graph loc)
  (define graph (graph-copy base-graph))
  (for ([adj (in-list (adjacent game loc))])
    (add-edge! graph loc adj))
  graph)

(define (targets game player)
  (define type (cond
                 [(eq? (cdr player) #\E) #\G]
                 [(eq? (cdr player) #\G) #\E]))
  (filter (lambda (p)
            (eq? type (cdr p)))
          (game-players game)))

(define (shortest-paths graph start)
  (define-values (dists prevs) (dijkstra graph start))
  (values dists
          (for/hash ([(dest steps) (in-hash dists)]
                     #:unless (not (hash-ref prevs dest)))
            (values dest
                    (for/fold ([square dest])
                              ([rem (in-range 0 (sub1 steps))])
                      (hash-ref prevs square))))))


(define (routes game player)
  (define/match (route<=? route1 route2)
    [((cons dist1 loc1) (cons dist2 loc2)) (or
                                            (< dist1 dist2)
                                            (and (= dist1 dist2) (loc<=? loc1 loc2)))])
  
  (define graph (graph-copy (game-graph game)))
  (for ([p (in-list (game-players game))]
        #:unless (equal? player p))
    (remove-vertex! graph  (car  p)))
  
  (define-values (dists nexts) (shortest-paths graph (car player)))
  (define target-squares (apply append (map (compose (curry get-neighbors (game-graph game)) car)
                                            (targets game player))))
  (cdar
   (sort 
    (for/list ([ts (in-list target-squares)]
               #:unless (eq? (hash-ref dists ts) +inf.0))
      (cons (hash-ref dists ts) (hash-ref nexts ts #f)))
    route<=?)))

(define (in-range? game player)
  (define adjs (get-neighbors (game-graph game) (car player)))
  (ormap (compose (curry set-member? adjs) car) (targets game player)))


(define (make-move old player)
  (if (in-range? old player)
      old
      (let ([new-loc (routes game player)])
        (struct-copy game old [players (cons ( ))]))))

(for/fold ([posns (hash)])
          ([p (in-list (game-players test-state))])
  (hash-set posns (routes test-state p) (cdr p)))

(define (display-board game)
  (for ([row (in-vector (game-map game))]
        [y (in-naturals)])
    (for ([square (in-vector row)]
          [x (in-naturals)])
      (define p (dict-ref (game-players game) (cons x y) #f))
      (if p
          (display p)
          (display square)))
    (displayln "")))
