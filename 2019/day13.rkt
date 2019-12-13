#lang racket

(require "../intcode.rkt"
         "../utils.rkt")

(define input (new-prog (file->string "input13.txt")))


(define (part1 input)
  (restart! input)
  (define outputs (run! input))
  (define screen (for/hash ([tile (in-list (partition outputs 3))])
                   (values (cons (car tile) (cadr tile)) (caddr tile))))

  (length (filter (curry = 2) (hash-values screen))))

(part1 input)

(define (display-output out)
  (define score 0)
  (for ([row (in-list (group-by cadr (partition out 3)))])
    (for ([col (in-list row)])
      (if (= (car col) -1)
          (set! score (caddr col))
          (let ([sq (case (caddr col)
                      [(0) " "]
                      [(1) "#"]
                      [(2) "@"]
                      [(3) "-"]
                      [(4) "*"])])
            (printf "~a" sq))))
    (printf "~%"))
  (printf "~%== ~a ==~%" score))


;; OK, I kinda want to actually play the game now, but let's cheat and build an automatic
;; bot that will just move the paddle in the direction of the ball

(define (auto-play game)
  (restart! game)
  (set-cell! game 0 2)
  (define score 0)
  (let loop ([in (list)]
             [state (hash)])
    (if (prog-done game)
        score
        (let* ([out (run! game in)]
               [newstate (for/fold ([state state])
                                   ([tile (in-list (partition out 3))])
                           (hash-set state (cons (car tile) (cadr tile)) (caddr tile)))]
               [ball (car (filter (compose (curry = 4) cdr) (hash->list newstate)))]
               [paddle (car (filter (compose (curry = 3) cdr) (hash->list newstate)))])
          (set! score (hash-ref newstate (cons -1 0)))
          (define in 
            (cond
              [(< (caar ball) (caar paddle)) '(-1)]
              [(= (caar ball) (caar paddle)) '(0)]
              [(> (caar ball) (caar paddle)) '(1)]))
          (loop in newstate)))))

(auto-play input)
