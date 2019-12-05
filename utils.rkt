#lang racket

(require rackunit
         racket/struct)

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
