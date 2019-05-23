#lang racket
(require graph
         data/queue
         rackunit)

(struct game (map players) #:transparent)
(struct unit (pos type hp ap) #:transparent)

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
                               (set! players (cons (unit (cons x y) ch 200 3) players))
                               #\.)]
          [else ch]))))

  (game gmap players))

(define test-state (read-map "day15_test.txt"))

(define (game-ref game x y)
  (vector-ref (vector-ref (game-map game) y) x))

(define (occupied? game pos)
  (ormap (lambda (p) (equal? (unit-pos p) pos)) (game-players game)))

(define (pos<=? p1 p2)
  (or
   (< (cdr p1) (cdr p2))
   (and (= (cdr p1) (cdr p2))
        (<= (car p1) (car p2)))))

(define (sort-players players)
  (sort players (lambda (p1 p2) (pos<=? (unit-pos p1) (unit-pos p2)))))

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
    [((cons pos1 dist1) (cons pos2 dist2)) (or (< dist1 dist2)
                                               (and (= dist1 dist2)
                                                    (pos<=? pos1 pos2)))])
  (define goals (apply append (for/list ([o (opponents game player)])
                                (adjacent game (unit-pos o)))))
  (define todo (make-queue))
  (define dists (make-hash (list (cons (unit-pos player) (cons #f 0)))))
  (define seen (mutable-set))
  (enqueue! todo (unit-pos player))
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
                 [(eq? (unit-type player) #\E) #\G]
                 [(eq? (unit-type player) #\G) #\E]))
  (filter (lambda (p)
            (eq? type (unit-type p)))
          (game-players game)))

(define (in-range? game player)
  (define adjs (adjacent game (unit-pos player)))
  (ormap (compose (curry set-member? adjs) unit-pos) (opponents game player)))

(define (in-range game player)
  (define adjs (adjacent game (unit-pos player)))
  (sort
   (filter (compose (curry set-member? adjs) unit-pos) (opponents game player))
   (lambda (p1 p2) (pos<=? (unit-pos p1) (unit-pos p2)))))

(define (set-unit-pos state player pos)
  (define players (for/list ([p (game-players state)])
                    (if (eq? p player)
                        (struct-copy unit player [pos pos])
                        p)))
  (struct-copy game state [players players] ))

(define (move-player game player)
  (if (in-range? game player)
      game
      (let ([step (find-move game player)])
        (if step
            (set-unit-pos game player step)
            game))))

(define (move-all state)
  (for/fold ([state state])
            ([p (sort-players (game-players state))])
    (move-player state p)))

(define (display-board game)
  (define players (for/hash ([p (game-players game)])
                    (values (unit-pos p) (unit-type p))))
  (for ([row (in-vector (game-map game))]
        [y (in-naturals)])
    (for ([square (in-vector row)]
          [x (in-naturals)])
      (define p (hash-ref players (cons x y) #f))
      (if p
          (display p)
          (display square)))
    (displayln "")))

