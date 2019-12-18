#lang racket

(require "../intcode.rkt"
         "../utils.rkt")

(define ascii (new-prog (file->string "input17.txt")))

(define (view)
  (restart! ascii)
  (run! ascii))

(define (display-scaffolds points)
  (for ([p (in-list points)])
    (printf "~a" (integer->char p))))

(display-scaffolds (view))

(define (build-map points)
  (for/fold ([area (hash)]
             [x 0]
             [y 0]
             #:result area)
            ([p (in-list points)])
    (case p
      [(35 94 62 60 118) (values (hash-set area (make-rectangular x y) (integer->char p))
                                 (add1 x) y)]
      [(10) (values area 0 (add1 y))]
      [else (values area (add1 x) y)])))


(define (intersections area)
  (define adj '(+i 1 -i -1))
  (filter values
          (for/list ([p (in-list (hash-keys area))])
            (define neighbors (map (curry + p) adj))
            (if (andmap (curry hash-has-key? area) neighbors)
                p
                #f))))

(define (alignment-sum intersections)
  (for/fold ([sum 0])
            ([p (in-list intersections)])
    (+ sum (* (real-part p) (imag-part p)))))

(alignment-sum (intersections (build-map (view))))

;; Part 2

(define test-area (map char->integer (string->list
                                      "#######...#####
#.....#...#...#
#.....#...#...#
......#...#...#
......#...###.#
......#.....#.#
^########...#.#
......#.#...#.#
......#########
........#...#..
....#########..
....#...#......
....#...#......
....#...#......
....#####......")))

(display-scaffolds test-area)

(build-map test-area)

(define (find-path area)
  (define (floor? p) (hash-has-key? area p))
  (define dirs (hash #\^ -i #\> 1 #\v +i #\< -1))
  (define start (for/first ([(p c) (in-hash area)]
                            #:when (set-member? '(#\^ #\> #\v #\<) c))
                  p))
  (define dir (hash-ref dirs (hash-ref area start)))
  (define path
    (let walk ([pos start]
               [dir dir]
               [path '()]
               [step 0])
      (define left (* dir -i))
      (define right (* dir +i))
      (cond
        [(floor? (+ pos dir)) (walk (+ pos dir) dir path (add1 step))]
        [(floor? (+ pos right)) (walk (+ pos right) right (cons #\R (cons step path)) 1)]
        [(floor? (+ pos left)) (walk (+ pos left) left (cons #\L (cons step path)) 1)]
        [else (reverse (cons step path))])))
  (if (= 0 (car path)) (cdr path) path))

(define (print-path path)
  (printf "~a" (car path))
  (for ([p (in-list (cdr path))])
    (printf ",~a" p)))

(define (path->ints path)
  (string->list
   (string-join (for/list ([p (in-list path)])
                  (format "~a" p))
                ",")))


(define (subseqs path #:min [minlen 4] #:max [maxlen 20])
  (define subs
    (let loop ([path path]
               [subs '()])
      (if (empty? path)
          subs
          (loop (cdr path)
                (for*/fold ([subs subs])
                           ([n (in-range minlen (add1 (min maxlen (length path))))]
                            [sub (in-value (take path n))]
                            #:when (empty? (set-intersect '(#\A #\B #\C) sub)))
                  (set-add subs sub))))))
  (sort subs > #:key (lambda (sub)
                       (define occ (occurrences path sub))
                       (* occ (sub1 (length sub))))))

(define (occurrences path sub)
  (define len (length sub))
  (let loop ([n 0]
             [path path])
    (if (< (length path) len)
        n
        (let ([pre (take path len)])
          (if (equal? pre sub)
              (loop (add1 n) (drop path len))
              (loop n (cdr path)))))))

(define (replace-all path sub to)
  (define len (length sub))
  (let loop ([stack '()]
             [path path])
    (if (< (length path) len)
        (append (reverse stack) path)
        (let ([pre (take path len)])
          (if (equal? pre sub)
              (loop (cons to stack) (drop path len))
              (loop (cons (car path) stack) (cdr path)))))))

(define (replace-by-longest path)
  (for/fold ([path path]
             [subs '()])
            ([rep (in-list '(#\A #\B #\C))])
    (define sub (car (subseqs path)))
    (values
     (replace-all path sub rep)
     (cons sub subs))))


(define input-path (path->ints (find-path (build-map (view)))))
(replace-by-longest input-path)

(display-scaffolds (view))

(list->string input-path)


"L,6,R,12,L,6,R,12,L,10,L,4,L,6,L,6,R,12,L,6,R,12,L,10,L,4,L,6,L,6,R,12,L,6,L,10,L,10,L,4,L,6,R,12,L,10,L,4,L,6,L,10,L,10,L,4,L,6,L,6,R,12,L,6,L,10,L,10,L,4,L,6"

(define input (map char->integer
                   (string->list (string-join (list 
                                               "A,B,A,B,A,C,B,C,A,C\n"
                                               ;A
                                               "L,6,R,12,L,6\n"

                                               ;B
                                               "R,12,L,10,L,4,L,6\n"

                                               ;C
                                               "L,10,L,10,L,4,L,6\n"
                                               "n\n")
                                              ""))))

(restart! ascii)
(set-cell! ascii 0 2)
(run! ascii input)

