#lang racket

(require "../utils.rkt"
         data/heap
         threading)

(define test0
  "#########
#b.A.@.a#
#########")

(define test
  "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################")

(define test2 "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################")

(define test3 "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################")

(define test4 "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################")


(define test5 "###############
#d.ABC.#.....a#
######@#@######
###############
######@#@######
#b.....#.....c#
###############")

(define test6 "#############
#DcBa.#.GhKl#
#.###@#@#I###
#e#d#####j#k#
###C#@#@###J#
#fEbA.#.FgHi#
#############")

(define (distances area start)
  (define (square pos)
    (hash-ref area pos #f))
  (define (floor? sq)
    (or (equal? sq #\.)
        (equal? sq #\@)
        (and (char? sq) (char>=? sq #\0) (char<=? sq #\9))))
  (define (door? sq)
    (and (char? sq) (char-upper-case? sq)))
  (define (key? sq)
    (and (char? sq) (char-lower-case? sq)))

  (define q (deque))
  (define (add-neighbors! seen pos doors dist)
    (for ([dx (in-list '(1 0 -1 0))]
          [dy (in-list '(0 1 0 -1))])
      (define adj (cons (+ (car pos) dx) (+ (cdr pos) dy)))
      (when (not (set-member? seen adj))
        (push-end! q (list adj doors (add1 dist))))))

  (push-end! q (list start '() 0))
  (let loop ([seen (set)]
             [key-dists (hash)])
    (if (deque-empty? q)
        key-dists
        (match-let* ([(list pos doors dist) (pop-front! q)]
                     [sq (square pos)])
          (cond
            [(floor? sq) (add-neighbors! seen pos doors dist)
                        (loop (set-add seen pos) key-dists)]
            [(key? sq) (add-neighbors! seen pos doors dist)
                       (loop (set-add seen pos) (hash-set key-dists sq (cons doors dist)))]
            [(door? sq) (add-neighbors! seen pos (cons (char-downcase sq) doors) dist)
                        (loop (set-add seen pos) key-dists)]
            [else (loop (set-add seen pos) key-dists)])))))

(define (number-entrances area)
  (define counter (char->integer #\0))
  (for/hash ([(pos sq) (in-hash area)])
    (if (char=? sq #\@)
        (let ()
          (set! counter (add1 counter))
          (values pos (integer->char counter)))
        (values pos sq))))

(define (shortest-path area)
  (set! area (number-entrances area))
  (define keys (for/hash ([(pos sq) (in-hash area)]
                          #:when (or (char=? sq #\@)
                                     (and (char? sq) (char>=? sq #\0) (char<=? sq #\9))
                                     (char-lower-case? sq)))
                 (values sq pos)))
  (define dists (for/hash ([(key pos) (in-hash keys)])
                  (values key (distances area pos))))
  (define entrances (for/hash ([sq (in-list (hash-keys keys))]
                               #:when (and (char? sq) (char>=? sq #\0) (char<=? sq #\9)))
                      (values sq sq)))
  
  (define (adjacents from keys)
    (for*/list ([(to node) (in-hash (hash-ref dists from))]
                [doors (in-value (car node))]
                [dist (in-value (cdr node))]
                #:when (and (not (set-member? keys to))
                            (andmap (curry set-member? keys) doors)))
      (cons to dist)))
  
  (let loop ([paths (hash (cons entrances (set)) 0)])
    (define dists (for*/fold ([dists (hash)])
                             ([(path pathlen) (in-hash paths)]
                              [entrances (in-value (car path))]
                              [keys (in-value (cdr path))]
                              [(entrance last-pos) (in-hash entrances)]
                              [adj (in-list (adjacents last-pos keys))])
                    (match-define (cons key dist) adj)
                    (define found (set-add (cdr path) key))
                    (define newnode (cons (hash-set entrances entrance key) found))
                    (define cached (hash-ref dists newnode #f))
                    (if (and cached (< cached (+ pathlen dist)))
                        dists
                        (hash-set dists newnode (+ pathlen dist)))))
    
    (if (= 0 (hash-count dists))
        (apply min (hash-values paths))
        (loop dists))))

(define (part1 mapstr)
  (shortest-path (read-area mapstr #:ignore '(#\#))))

(define (part2-area input)
  (define area (read-area input #:ignore '(#\#)))
  (define entrance (for/first ([(pos sq) (in-hash area)]
                               #:when (char=? sq #\@))
                     pos))
  (define x (car entrance))
  (define y (cdr entrance))
  (~> area
      (hash-set entrance #\#)
      (hash-set (cons (sub1 x) y) #\#)
      (hash-set (cons (add1 x) y) #\#)
      (hash-set (cons x (sub1 y)) #\#)
      (hash-set (cons x (add1 y)) #\#)
      (hash-set (cons (sub1 x) (sub1 y)) #\@)
      (hash-set (cons (sub1 x) (add1 y)) #\@)
      (hash-set (cons (add1 x) (sub1 y)) #\@)
      (hash-set (cons (add1 x) (add1 y)) #\@)))

(define (part2 input)
  (define area (part2-area input))
  (shortest-path area))

(define input (file->string "input18.txt"))
(time (part1 input))
(time (part2 input))



















