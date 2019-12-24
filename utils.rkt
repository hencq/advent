#lang racket

(require rackunit
         racket/struct)

(provide
 take-upto
 drop-upto
 partition
 partition-by
 deque
 deque-empty?
 pop-front!
 peek-front
 push-front!
 pop-end!
 peek-end
 push-end!
 rotate!
 deque->list
 list->deque
 
 read-area
 print-area)

(define (take-upto lst pos)
  (with-handlers
    ([exn:fail:contract? (lambda (e) lst)])
    (take lst pos)))

(module+ test
  (check-equal? (take-upto (list 1 2 3 4) 3) (list 1 2 3))
  (check-equal? (take-upto (list 1 2) 3) (list 1 2)))

(define (drop-upto lst pos)
  (with-handlers
    ([exn:fail:contract? (lambda (e) '())])
    (drop lst pos)))

(module+ test
  (check-equal? (drop-upto (list 1 2 3 4) 3) (list 4))
  (check-equal? (drop-upto (list 1 2) 3) '()))

(define (partition coll n [step #f] [pad #f])
  (when (not step)
    (set! step n))
  (let loop ([coll coll]
             [acc '()])
    (if (empty? coll)
        (reverse acc)
        (let ([p (take-upto coll n)])
          (loop (drop-upto coll step)
                (cond
                  [(= (length p) n) (cons p acc)]
                  [pad (cons (append p (take-upto pad (- n (length p)))) acc)]
                  [else acc]))))))


(module+ test
  (check-equal? (partition (list 1 2 3 4) 2) (list (list 1 2) (list 3 4)))
  (check-equal? (partition (list 1 2 3 4) 2 1) '((1 2) (2 3) (3 4)))
  (check-equal? (partition (list 1 2 3 4) 2 1 '(a)) '((1 2) (2 3) (3 4) (4 a)))
  (check-equal? (partition (list 1 2 3 4) 2 3) '((1 2)))
  (check-equal? (partition (list 1 2 3 4) 2 3 '()) '((1 2) (4))))

(define (partition-by coll f)
  (let loop ([coll coll]
             [acc '()]
             [wip '()]
             [last #f])
    (if (empty? coll)
        (reverse (cons (reverse wip) acc))
        (let* ([head (car coll)]
               [cur (f head)])
          (cond
            [(empty? wip) (loop (cdr coll) acc (cons head wip) cur)]
            [(equal? cur last) (loop (cdr coll) acc (cons (car coll) wip) cur)]
            [else (loop (cdr coll) (cons (reverse wip) acc) (list head) cur)])))))

(module+ test
  (check-equal? (partition-by (list 1 1 2 3 3 4) values) '((1 1) (2) (3 3) (4)))
  (check-equal? (partition-by (list 1 2 3 4 5) (curry = 3)) '((1 2) (3) (4 5))))


;; === Deque ===

;; A simple deque implemented as a linked list
;; It can be used as a regular queue or a stack
;; It also supports a rotate operation to use it as a ring

(struct Node (prev next val) #:mutable #:transparent)
(struct Deque (head tail) #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'deque)
      (lambda (obj) (deque->list obj))))])

(define (deque . xs)
  (if (empty? xs)
      (Deque #f #f)
      (list->deque xs)))

(define (deque-empty? dq)
  (not (Deque-head dq)))

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

(define (rotate! dq [n 1])
  (cond
    [(= n 0) (void)]
    [(> n 0) (for ([i (in-range n)])
               (define front (pop-front! dq))
               (push-end! dq front))]
    [(< n 0) (for ([i (in-range (- n))])
               (define end (pop-end! dq))
               (push-front! dq end))]))


;; Some helpers to deal with map based questions
;; E.g. to quickly read a map into a hash table and print it back

(define (rotate-pos pos [dir 1])
  (cons (* -1 dir (cdr pos)) (* dir (car pos))))

(define (area-dimensions area)
  (for/fold ([dimensions (list #f #f #f #f)])
            ([pos (in-list (hash-keys area))])
    (match-define (list x1 y1 x2 y2) dimensions)
    (list (if x1 (min x1 (car pos)) (car pos))
          (if y1 (min y1 (cdr pos)) (cdr pos))
          (if x2 (max x2 (car pos)) (car pos))
          (if y2 (max y2 (cdr pos)) (cdr pos)))))

(define (print-area area [tile-fun values] [dimensions #f])
  (when (not dimensions)
    (set! dimensions (area-dimensions area)))
  (when (hash? tile-fun)
    (set! tile-fun (hash->fun tile-fun)))
  
  (match-define (list x1 y1 x2 y2) dimensions)
  (for ([y (in-range y1 (add1 y2))])
    (for ([x (in-range x1 (add1 x2))])
      (define tile (tile-fun (hash-ref area (cons x y) #f)))
      (if tile
          (printf "~a" tile)
          (printf " ")))
    (printf "~%")))

(define (read-area lines [char-fun values] #:ignore [ignore (set)])
  (when (string? lines)
    (set! lines (string-split lines "\n")))
  (when (hash? char-fun)
    (set! char-fun (hash->fun char-fun)))
  
  (for/fold ([area (hash)])
            ([row (in-list lines)]
             [y (in-naturals)])
    (for/fold ([area area])
              ([col (in-list (string->list row))]
               [x (in-naturals)])
      (if (set-member? ignore col)
          area
          (let ([tile (char-fun col)])
            (if tile
                (hash-set area (cons x y) tile)
                area))))))

(define (hash->fun h [else #f])
  (lambda (k [else else])
    (hash-ref h k else)))

;; (define test "###########
;; #         #
;; #   #     #
;; # $ #  @  #
;; ###########")

;; (print-area (read-area test #:ignore '(#\space)))



