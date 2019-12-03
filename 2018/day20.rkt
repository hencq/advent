#lang racket
(require data/queue
         rackunit)

(define (move room xdir ydir)
  (cons (+ (car room) xdir)
        (+ (cdr room) ydir)))

(define (add-door doors room xdir ydir)
  (define new-room (move room xdir ydir))
  (set-add 
   (set-add doors (cons room new-room))
   (cons new-room room)))

;; Recursive descent parser for the regexes
(define (parse str)
  (define-values (p doors r) (chain str 0 (set) (cons 0 0)))
  doors)

;; Match a chain of characters
;; If we encounter a ( we branch
;; Otherwise (| or )) we exit the chain
(define (chain str pos doors room)
  (let ([ch (string-ref str pos)])
    (cond
      [(eqv? ch #\^) (chain str (add1 pos) doors room)]
      [(eqv? ch #\$) (values pos doors room)]
      [(eqv? ch #\N) (chain str (add1 pos) (add-door doors room 0 -1) (move room 0 -1))]
      [(eqv? ch #\E) (chain str (add1 pos) (add-door doors room 1 0) (move room 1 0))]
      [(eqv? ch #\S) (chain str (add1 pos) (add-door doors room 0 1) (move room 0 1))]
      [(eqv? ch #\W) (chain str (add1 pos) (add-door doors room -1 0) (move room -1 0))]
      [(eqv? ch #\() (branch str (add1 pos) doors room)]
      [else (values pos doors room)])))

;; Match a branch
;; When we reach the end of the branch, fold all the branches
;; Otherwise try to match a chain and add it to the branches
(define (branch str pos doors room)
  (let loop ([branches '()]
             [pos pos]
             [doors doors])
    (let ([ch (string-ref str pos)])
      (cond
        [(eqv? ch #\)) (for/fold ([p pos]
                                  [doors doors]
                                  [room room])
                                 ([room (in-list branches)])
                         (chain str (add1 pos) doors room))]
        [(eqv? ch #\|) (loop branches (add1 pos) doors)]
        [else (let-values ([(pos doors branch) (chain str pos doors room)])
                (loop (cons branch branches) pos doors))]))))

(define (doors->rooms doors)
  (for/fold ([rooms (set)])
            ([door (in-set doors)])
    (define from (car door))
    (define to (cdr door))
    (set-add (set-add rooms from) to)))

(define (show-map doors)
  (define rooms (set->list (doors->rooms doors)))
  (define x-min (car (argmin car rooms)))
  (define x-max (car (argmax car rooms)))
  (define y-min (cdr (argmin cdr rooms)))
  (define y-max (cdr (argmax cdr rooms)))

  ;; print top border
  (printf "#")
  (for ([d (in-range x-min (add1 x-max))])
    (printf "##"))
  (newline)
  
  (for ([y (in-range y-min (add1 y-max))])
    (printf "#")
    (for ([x (in-range x-min (add1 x-max))])
      (if (and (= x 0) (= y 0))
          (printf "X")
          (printf "."))
      (define d (cons x y))
      (if (set-member? doors (cons d (move d 1 0)))
          (printf "|")
          (printf "#")))
    (printf "~%#")
    (for ([x (in-range x-min (add1 x-max))])
      (define d (cons x y))
      (if (set-member? doors (cons d (move d 0 1)))
          (printf "-#")
          (printf "##")))
    (printf "~%")))

(define (distances doors)
  (define doorhash
    (for/fold ([doorhash (hash)])
              ([door (in-set doors)])
      (match door
        [(cons from to) (hash-update doorhash from (curry cons to) '())])))
  (define todo (make-queue))
  (enqueue! todo (cons 0 0))
  (define dists (make-hash (list (cons (cons 0 0) 0))))
  (let loop ()
    (if (queue-empty? todo)
        dists
        (let ([room (dequeue! todo)])
          (for ([r (in-list (hash-ref doorhash room))])
            (when (not (hash-has-key? dists r))
              (enqueue! todo r)
              (hash-set! dists r (add1 (hash-ref dists room)))))
          (loop)))))

(module+ test
  (define doors (parse "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"))
  (check-eq? (farthest-room doors) 23)
  (check-eq? (farthest-room (parse "^E(NN|S)E$")) 4))

(define (farthest-room doors)
  (define dists (distances doors))
  (apply max (hash-values dists)))

(module+ puzzle
  (define input (parse (file->string "day20_input.txt")))

  (farthest-room input)

  (define (min-distance doors [n 1000])
    (define dists (distances doors))
    (length
     (filter (lambda (d)
               (>= d 1000)) (hash-values dists))))
  (min-distance input))

