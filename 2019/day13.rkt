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
  (let loop ([in (list)]
             [score 0]
             [ball #f]
             [paddle #f])
    (if (prog-done game)
        score
        (let ([out (run! game in)])
          (let-values ([(score ball paddle)
                        (for/fold ([score score]
                                   [ball ball]
                                   [paddle paddle])
                                  ([tile (in-list (partition out 3))])
                          (cond
                            [(= (car tile) -1) (values (caddr tile)
                                                       ball paddle)]
                            [(= (caddr tile) 4) (values score
                                                        (car tile)
                                                        paddle)]
                            [(= (caddr tile) 3) (values score
                                                        ball
                                                        (car tile))]
                            [else (values score ball paddle)]))])
            (define in 
              (cond
                [(< ball paddle) '(-1)]
                [(= ball paddle) '(0)]
                [(> ball paddle) '(1)]))
            (loop in score ball paddle))))))

(auto-play input)
