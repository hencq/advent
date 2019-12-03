#lang racket
(require rackunit)

(struct Queue (vec pos e1 e2) #:mutable)

(define (make-queue [size 32])
  (define q (Queue (make-vector size) 0 0 1))
  (queue-push! q 3)
  (queue-push! q 7)
  q)

(define (queue-ref q i)
  (define vec (Queue-vec q))
  (vector-ref vec (modulo i (vector-length vec))))

(define (queue-push! q x)
  (when (= (add1 (Queue-pos q)) (vector-length (Queue-vec q)))
    (define new-vec (make-vector (* 2 (vector-length (Queue-vec q)))))
    (vector-copy! new-vec 0 (Queue-vec q))
    (set-Queue-vec! q new-vec))
  (vector-set! (Queue-vec q) (Queue-pos q) x)
  (set-Queue-pos! q (add1 (Queue-pos q))))

(define queue-length  Queue-pos)

(define (display-queue q)
  (for ([i (Queue-pos q)])
    (cond
      [(= i (Queue-e1 q)) (printf "(~a)" (queue-ref q i))]
      [(= i (Queue-e2 q)) (printf "[~a]" (queue-ref q i))]
      [else (printf " ~a " (queue-ref q i))]))
  (displayln ""))

(define (queue->string q)
  (string-join
   (for/list ([i (Queue-pos q)])
     (format "~a" (queue-ref q i)))
   ""))

(define (number->digits n)
  (if (= n 0)
      (list 0)
      (let loop ([digits '()]
                 [n n])
        (if (= 0 n)
            digits
            (loop (cons (modulo n 10) digits) (quotient n 10))))))

(define (step! q)
  (define e1s (queue-ref q (Queue-e1 q)))
  (define e2s (queue-ref q (Queue-e2 q)))
  (define sum (+ e1s e2s))
  (for ([d (in-list (number->digits sum))])
    (queue-push! q d))
  (set-Queue-e1! q (modulo (+ 1 e1s (Queue-e1 q)) (Queue-pos q)))
  (set-Queue-e2! q (modulo (+ 1 e2s (Queue-e2 q)) (Queue-pos q))))

(define (step-until! q recipes)
  (let loop ()
    (if (>= (Queue-pos q) recipes)
        (void)
        (begin
          (step! q)
          (loop)))))

(define (next-scores iter [n 10])
  (define q (make-queue))
  (step-until! q (+ iter n))
  (for/list ([i (in-range iter (+ iter n))])
    (queue-ref q i)))

(define q (make-queue 4))
(for ([i 20])
  (display-queue q)
  (step! q))

(define (search digits #:max [max #f] #:show [show #f])
  (define needle (map (compose string->number string) (string->list digits)))
  (define needle-len (length needle))
  (define q (make-queue))
  (let loop ([todo needle]
             [pos 0])
    (let grab ()
      (if (> (queue-length q) pos)
          (void)
          (begin
            (step! q)
            (grab))))
    (when show
      (printf "pos: ~a [~a] len: ~a needle: ~a~%" pos (queue-ref q pos) (queue-length q) todo))
    (cond
      [(< pos 0) pos]
      [(and max (>= pos max)) pos]
      [(empty? todo) (- pos needle-len)]
      [(= (car todo) (queue-ref q pos)) (loop (cdr todo) (add1 pos))]
      [else (loop needle (add1 (- pos (- needle-len (length todo)))))])))
