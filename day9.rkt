#lang racket
(require rackunit
         threading
         profile)

(struct Node (prev next val) #:mutable #:transparent)
(struct Deque (head tail) #:mutable #:transparent)

(define (deque . xs)
  (if (empty? xs)
      (Deque #f #f)
      (list->deque xs)))

(define (push-front! dq x)
  (let* ([old (Deque-head dq)]
         [new (Node #f old x)])
    (if old
        (set-Node-prev! old new)
        (set-Deque-tail! dq new))
    (set-Deque-head! dq new)
    dq))

(define (push-end! dq x)
  (let* ([old (Deque-tail dq)]
         [new (Node old #f x)])
    (if old
      (set-Node-next! old new)
      (set-Deque-head! dq new))
    (set-Deque-tail! dq new)
    dq))

(define (peek-front dq)
  (Node-val (Deque-head dq)))

(define (peek-end dq)
  (Node-val (Deque-tail dq)))

(define (pop-front! dq)
  (define head (Deque-head dq))
  (if head
      (let ([new (Node-next head)])
        (if new
            (set-Node-prev! new #f)
            (set-Deque-tail! dq #f))
        (set-Deque-head! dq new)
        (Node-val head))
      #f))

(define (pop-end! dq)
  (define tail (Deque-tail dq))
  (if tail
      (let ([new (Node-prev tail)])
        (if new
            (set-Node-next! new #f)
            (set-Deque-head! dq #f))
        (set-Deque-tail! dq new)
        (Node-val tail))
      #f))

(define (deque->list dq)
  (let loop ([node (Deque-tail dq)]
             [xs '()])
    (if (not node)
        xs
        (loop (Node-prev node) (cons (Node-val node) xs)))))

(define (list->deque xs)
  (define dq (deque))
  (for ([x xs])
    (push-end! dq x))
  dq)

(define (rotate! dq n)
  (cond
    [(= n 0) (void)]
    [(> n 0) (for ([i (in-range n)])
               (define front (pop-front! dq))
               (push-end! dq front))]
    [(< n 0) (for ([i (in-range (- n))])
               (define end (pop-end! dq))
               (push-front! dq end))]))


(define (make-move! circle marble)
  (if (= 0 (modulo marble 23))
      (let ()
        (rotate! circle -7)
        (define stone (pop-front! circle))
        (+ marble stone))
      (let ()
        (rotate! circle 2)
        (push-front! circle marble)
        0)))

(define (play-game players max-marble)
  (define circle (deque 0))
  (define points 
    (for/fold ([points (hash)])
              ([marble (in-range 1 (add1 max-marble))]
               [player (in-cycle (range 1 (add1 players)))])
      (define score (make-move! circle marble))
      (hash-update points player (curry + score) 0)))
  (values (deque->list circle) points))

(define (max-score players max-marble)
  (define-values (circle points) (play-game players max-marble))
  (apply max (map cdr (hash->list points))))

(module+ test
  (check-eq? (max-score 10 1618) 8317)
  (check-eq? (max-score 13 7999) 146373)
  (check-eq? (max-score 17 1104) 2764)
  (check-eq? (max-score 21 6111) 54718)
  (check-eq? (max-score 30 5807) 37305)
  (check-eq? (max-score 428 70825) 398502)
  )



