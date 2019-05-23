#lang racket
(require graph
         data/queue
         rackunit)

(struct game (map players) #:transparent)

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

  (game gmap players))

(define test-state (read-map "day15_test.txt"))

(define (game-ref game x y)
  (vector-ref (vector-ref (game-map game) y) x))

(define (occupied? game loc)
  (dict-ref (game-players game) loc #f))

(define (loc<=? l1 l2)
  (or
   (< (cdr l1) (cdr l2))
   (and (= (cdr l1) (cdr l2))
        (<= (car l1) (car l2)))))

(define (sort-players players)
  (sort players (lambda (p1 p2) (loc<=? (car p1) (car p2)))))

(define (adjacent game pos)
  (define-values (x y) (values (car pos) (cdr pos)))
  (for/list ([dy (in-list (list (- y 1) y y (+ y 1)))]
             [dx (in-list (list x (- x 1) (+ x 1) x))]
             #:unless (or (< dx 0)
                          (< dy 0)
                          (>= dx (vector-length (game-map game)))
                          (>= dy (vector-length (vector-ref (game-map game) 0))))
             #:when (eq? (game-ref game dx dy) #\.))
    (cons dx dy)))

(define (find-move game player)
  (define/match (step<=? a b)
    [((cons loc1 dist1) (cons loc2 dist2)) (or (< dist1 dist2)
                                               (and (= dist1 dist2)
                                                    (loc<=? loc1 loc2)))])
  (define goals (apply append (for/list ([o (opponents game player)])
                                (adjacent game (car o)))))
  (define todo (make-queue))
  (define dists (make-hash (list (cons (car player) (cons #f 0)))))
  (define seen (mutable-set))
  (enqueue! todo (car player))
  (let loop ()
    (if (queue-empty? todo)
        dists
        (let ([sq (dequeue! todo)])
          (for ([pos (adjacent game sq)]
                #:unless (occupied? game pos)
                #:unless (set-member? seen pos))
            (enqueue! todo pos)
            (match (hash-ref dists sq)
              [(cons step dist) (hash-set! dists pos (cons (or step pos) (add1 dist)))])
            (set-add! seen pos))
          (loop))))
  (define steps 
    (sort 
     (for/list ([t (in-list goals)]
                #:when (hash-ref dists t #f))
       (hash-ref dists t #f))
     step<=?))
  (if (empty? steps)
      #f
      (caar steps)))

(define (opponents game player)
  (define type (cond
                 [(eq? (cdr player) #\E) #\G]
                 [(eq? (cdr player) #\G) #\E]))
  (filter (lambda (p)
            (eq? type (cdr p)))
          (game-players game)))

(define (in-range? game player)
  (define adjs (adjacent game (car player)))
  (ormap (compose (curry set-member? adjs) car) (opponents game player)))


(define (update-loc game-state player loc)
  (define new (cons loc (cdr player)))
  (define players (cons new (dict-remove (game-players game-state) (car player))))
  (struct-copy game game-state [players players]))

(define (move-player game player)
  (if (in-range? game player)
      game
      (let ([step (find-move game player)])
        (if step
            (update-loc game player step)
            game))))

(define (move-all state)
  (for/fold ([state state])
            ([p (sort-players (game-players state))])
    (move-player state p)))


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
