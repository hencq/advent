#lang racket
(require graph
         data/queue
         rackunit)

(struct game (map [players #:mutable]) #:transparent)
(struct unit (pos type hp ap) #:transparent #:mutable)

(define (read-map fname-or-lines)
  (define lines (if (string? fname-or-lines) (file->lines fname-or-lines) fname-or-lines))
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

(define (occupied? game pos)
  (ormap (lambda (p) (equal? (unit-pos p) pos)) (game-players game)))

(define (pos<=? p1 p2)
  (or
   (< (cdr p1) (cdr p2))nn
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
    [((list g1 pos1 dist1) (list g2 pos2 dist2)) (or (< dist1 dist2)
                                                     (and (= dist1 dist2)
                                                          (or (pos<=? g1 g2)
                                                              (and (equal? g1 g2)
                                                                   (pos<=? pos1 pos2)))))])
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
       (match (hash-ref dists t #f)
         [(cons step dist) (list t step dist)]))
     step<=?))
  (if (empty? steps)
      #f
      (match (car steps)
        [(list t step dist) step])))


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


(define (move-player! game player)
  (if (empty? (opponents game player))
      #f
      (begin
        (when (not (in-range? game player))
          (let ([step (find-move game player)])
            (when step
              (set-unit-pos! player step))))
        (attack! game player)
        #t)))

(define (move-all! state)
  (let loop ([players (sort-players (game-players state))])
    (if (empty? players)
        #t
        (let ([p (car players)])
          (if (> (unit-hp p) 0)
              (if (move-player! state p)
                  (loop (cdr players))
                  #f)
              (loop (cdr players)))))))

(define (attack! game player)
  (define adjs (adjacent game (unit-pos player)))
  (define enemies (sort
                   (filter (compose (curry set-member? adjs) unit-pos) (opponents game player))
                   (lambda (p1 p2)
                     (or (< (unit-hp p1) (unit-hp p2))
                         (and (= (unit-hp p1) (unit-hp p2))
                              (pos<=? (unit-pos p1) (unit-pos p2)))))))
  (when (not (empty? enemies))
    (let* ([enemy (car enemies)]
           [hp (unit-hp enemy)]
           [new-hp (- hp (unit-ap player))])
      ;; (printf "Attacking ~a(~a,~a) ~a->~a~%"
      ;;         (unit-type enemy) (car (unit-pos enemy)) (cdr (unit-pos enemy))
      ;;         hp new-hp)
      (set-unit-hp! enemy new-hp)
      (when (<= new-hp 0)
        (set-game-players! game (remove enemy (game-players game)))))))

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
    (for ([p (in-list (sort-players (game-players game)))]
          #:when (= (cdr (unit-pos p)) y))
      (printf " ~a(~a)" (unit-type p) (unit-hp p)))
    (displayln "")))

(define test (read-map "day15_test7.txt"))
(define input (read-map "day15_input.txt"))

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
  (define total-hp (for/sum ([p (game-players state)]) (unit-hp p)))
  (printf "Rounds: ~a HP: ~a Score: ~a~%" rounds total-hp (* rounds total-hp))
  (values rounds total-hp (* rounds total-hp)))
