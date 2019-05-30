#lang racket
(require graph
         data/queue
         rackunit)

(struct game (map players) #:transparent)
(struct unit (type hp ap) #:transparent #:mutable)

(define (read-map fname-or-lines)
  (define lines (if (string? fname-or-lines) (file->lines fname-or-lines) fname-or-lines))
  (define height (length lines))
  (define width (string-length (car lines)))
  (define players (make-hash))
  (define gmap
    (for/vector ([l (in-list lines)]
                 [y (in-naturals)])
      (for/vector ([ch (in-list (string->list l))]
                   [x (in-naturals)])
        (cond
          [(or (eq? ch #\G)
               (eq? ch #\E)) (begin
                               (hash-set! players (cons x y) (unit ch 200 3))
                               #\.)]
          [else ch]))))

  (game gmap players))

(module+ test
  (define test (read-map (list "#########"
                               "#G..G..G#"
                               "#.......#"
                               "#.......#"
                               "#G..E..G#"
                               "#.......#"
                               "#.......#"
                               "#G..G..G#"
                               "#########")))
  (let-values ([(rounds hp score) (play! test +inf.0)])
    (check-eq? rounds 18)
    (check-eq? hp 1546))
  
  (define test2 (read-map (list "#######"   
                                "#.G...#"
                                "#...EG#"
                                "#.#.#G#"
                                "#..G#E#"
                                "#.....#"   
                                "#######")))
  (let-values ([(rounds hp score) (play! test2 +inf.0)])
    (check-eq? hp 590)
    (check-eq? rounds 47)
    (check-eq? score 27730))
  
  (define test3 (read-map (list "#######"
                                "#G..#E#"
                                "#E#E.E#"
                                "#G.##.#"
                                "#...#E#"
                                "#...E.#"
                                "#######")))
  (let-values ([(rounds hp score) (play! test3 +inf.0)])
    (check-eq? score 36334))
  
  (define test4 (read-map (list "#######"
                                "#E..EG#"
                                "#.#G.E#"
                                "#E.##E#"
                                "#G..#.#"
                                "#..E#.#"
                                "#######")))
  (let-values ([(rounds hp score) (play! test4 +inf.0)])
    (check-eq? score 39514))
    
  (define test5 (read-map (list "#######"
                                "#E.G#.#"
                                "#.#G..#"
                                "#G.#.G#"
                                "#G..#.#"
                                "#...E.#"
                                "#######")))
  (let-values ([(rounds hp score) (play! test5 +inf.0)])
    (check-eq? score 27755))
  
  (define test6 (read-map (list "#######"
                                "#.E...#"
                                "#.#..G#"
                                "#.###.#"
                                "#E#G#G#"
                                "#...#G#"
                                "#######")))
  (let-values ([(rounds hp score) (play! test6 +inf.0)])
    (check-eq? score 28944))
  
  (define test7 (read-map (list "#########"
                                "#G......#"
                                "#.E.#...#"
                                "#..##..G#"
                                "#...##..#"
                                "#...#...#"
                                "#.G...G.#"
                                "#.....G.#"
                                "#########")))
    (let-values ([(rounds hp score) (play! test7 +inf.0)])
      (check-eq? score 18740))

    (define test8 (read-map "day15_input.txt"))
    (let-values ([(rounds hp score) (play! test8 +inf.0)])
      (check-eq? score 191216)))

(define test-state (read-map "day15_test.txt"))

(define (game-ref game x y)
  (vector-ref (vector-ref (game-map game) y) x))

(define (pos<=? p1 p2)
  (or
   (< (cdr p1) (cdr p2))
   (and (= (cdr p1) (cdr p2))
        (<= (car p1) (car p2)))))

(define (sort-players game)
  (sort (hash-keys (game-players game)) pos<=?))

(define test (read-map (list "#########"
                               "#G..G..G#"
                               "#.......#"
                               "#.......#"
                               "#G..E..G#"
                               "#.......#"
                               "#.......#"
                               "#G..G..G#"
                               "#########")))

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

(define (find-move game pos player)
  (define/match (step<=? a b)
    [((list g1 pos1 dist1) (list g2 pos2 dist2)) (or (< dist1 dist2)
                                                     (and (= dist1 dist2)
                                                          (or (pos<=? g1 g2)
                                                              (and (equal? g1 g2)
                                                                   (pos<=? pos1 pos2)))))])

  (define enemy-squares (list->set
                         (apply append
                                (for/list ([(pt en) (in-hash (game-players game))]
                                           #:when (not (eq? (unit-type en) (unit-type player))))
                                  (adjacent game pt)))))
 
  
  (define todo (make-queue))
  (define dists (make-hash (list (cons pos 0))))
  (define steps (make-hash))
  
  (define (add-adjacents! sq)
    (for ([pos (adjacent game sq)]
          #:unless (hash-has-key? (game-players game) pos)
          #:unless (hash-has-key? dists pos))
      (enqueue! todo pos)
      (define distance (hash-ref dists sq))
      (define step (hash-ref steps sq pos))
      (hash-set! dists pos (add1 distance))
      (hash-set! steps pos step)))
  
  (enqueue! todo pos)
  (let loop ([found #f]
             [best-dist +inf.0])
    (if (or (queue-empty? todo) (set-empty? enemy-squares))
        ;; Stop when all squares are visited or all enemy-squares are looked at
        (hash-ref steps found pos)
        (let* ([sq (dequeue! todo)]
               [sq-dist (hash-ref dists sq)])
          (if (and found (> sq-dist best-dist))
              ;; Stop when distance > best distance
              (hash-ref steps found pos)
              (let ()
                (add-adjacents! sq)
                (if (and (set-member? enemy-squares sq)
                         (or (not found)
                             (pos<=? sq found)))
                    (loop sq sq-dist)
                    (loop found best-dist))))))))

(define (move-player! game pos)
  (define player (hash-ref (game-players game) pos))
  (define step (find-move game pos player))
  (when (not (equal? pos step))
    (hash-remove! (game-players game) pos)
    (hash-set! (game-players game) step player))
  (attack! game step player))

(define (move-all! state)
  (let loop ([players (sort-players state)])
    (cond
      [(empty? players) #t]
      [(finished? state) #f]
      [else 
       (let* ([pt (car players)]
              [plr (hash-ref (game-players state) pt #f)])
         (when plr
           (move-player! state pt))
         (loop (cdr players)))])))

(define (attack! game pos player)
  (define-values (enemy-pos enemy)
    (for/fold ([ep #f]
               [en #f])
              ([sq (in-list (adjacent game pos))])
      (define unit (hash-ref (game-players game) sq #f))
      (if (and unit
               (not (eq? (unit-type unit) (unit-type player)))
               (or (not ep) (< (unit-hp unit) (unit-hp en))))
          (values sq unit)
          (values ep en))))
  (when enemy
    (let* ([hp (unit-hp enemy)]
           [new-hp (- hp (unit-ap player))])
      (set-unit-hp! enemy new-hp)
      (when (<= new-hp 0)
        (hash-remove! (game-players game) enemy-pos)))))

(define (display-board game)
  (for ([row (in-vector (game-map game))]
        [y (in-naturals)])
    (for ([square (in-vector row)]
          [x (in-naturals)])
      (define plr (hash-ref (game-players game) (cons x y) #f))
      (if plr
          (display (unit-type plr))
          (display square)))
    (for ([pt (in-list (sort (hash-keys (game-players game)) pos<=?))]
          #:when (= (cdr pt) y))
      (define plr (hash-ref (game-players game) pt))
      (printf " ~a(~a)" (unit-type plr) (unit-hp plr)))
    (displayln "")))

(define (finished? state)
  (define players (hash-values (game-players state)))
  (andmap (lambda (p) (eq? (unit-type (car players))
                           (unit-type p)))
          (cdr players)))

(define (play! state [n 1] #:show [show #f])
  (define rounds 
    (let loop ([i 0])
      (if (= i n)
          i
          (begin
            (when show
              (printf "Turn ~a~%" i)
              (display-board state))
            (if (move-all! state)
                (loop (add1 i))
                i)))))
  (printf "Turn ~a~%" rounds)
  (display-board state)
  (define total-hp (for/sum ([(_ p) (in-hash (game-players state))]) (unit-hp p)))
  (printf "Rounds: ~a HP: ~a Score: ~a~%" rounds total-hp (* rounds total-hp))
  (values rounds total-hp (* rounds total-hp)))
