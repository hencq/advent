#lang racket
(require (rename-in pfds/deque/bankers (map dq:map)) 
         rackunit
         threading
         profile)

(define (add-marble circle pos n)
  (define len (length circle))
  (define insert-point (modulo pos len))
  (append (list n) (drop circle insert-point) (take circle insert-point)))

(define (remove-marble circle pos)
  (define len (length circle))
  (define cursor (modulo (- pos) len))
  (define-values (left right) (split-at circle cursor))
  (values 
    ;; (append (drop circle (add1 cursor)) (take circle cursor))
    (append (cdr right) left)
   (list-ref circle cursor)))

(define (make-move points player circle marble)
  (if (= (modulo marble 23) 0)
      (let-values ([(new-circle removed) (remove-marble circle 7)])
        (values (hash-update points player (curry + removed marble) 0) new-circle))
      (values points (add-marble circle 2 marble))))

(define (play-game players max-marble)
  (for/fold ([points (hash)]
             [circle (list 0)])
            ([marble (in-range 1 (add1 max-marble))]
             [player (in-cycle (range 1 (add1 players)))])
    (make-move points player circle marble)))

(define (max-score players max-marble)
  (define-values (points circle) (play-game players max-marble))
  (apply max (map cdr (hash->list points))))

(define (rotate circle n)
  (let loop ([n n]
             [circle circle])
    (cond
      [(= n 0) circle]
      [(> n 0) (loop (sub1 n) (enqueue-front (last circle) (init circle)))]
      [(< n 0) (loop (add1 n) (enqueue (head circle) (tail circle)))])))

(module+ test
  (check-eq? (max-score 10 1618) 8317)
  (check-eq? (max-score 13 7999) 146373)
  (check-eq? (max-score 17 1104) 2764)
  (check-eq? (max-score 21 6111) 54718)
  (check-eq? (max-score 30 5807) 37305)
  ;; (check-eq? (max-score 428 70825) 398502)
  )



