#lang racket

(require "../utils.rkt"
         data/heap)

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

(struct node (pos keys) #:transparent)

(define (find-keys mapstr)
  (define area (read-area mapstr #:ignore '(#\.)))
  (define keys (map (compose car string->list)
                    (regexp-match* "[a-z]" mapstr)))
  (define (found-all? n)
    (set-empty? (set-subtract keys (node-keys n))))
  (define (wall? n)
    (equal? (hash-ref area (node-pos n) #f) #\#))
  (define (door? n)
    (define sq (hash-ref area (node-pos n) #f))
    (and (char? sq) (char>=? sq #\A) (char<=? sq #\Z)))
  (define (can-enter? n)
    (if (wall? n)
        #f
        (if (door? n)
            (let ([needed (char-downcase (hash-ref area (node-pos n) #f))])
              (set-member? (node-keys n) needed))
            #t)))
  (define (add-neighbors! n steps seen)
    (define keyring (let ([sq (hash-ref area (node-pos n) #f)])
                      (if (and (char? sq) (char>=? sq #\a) (char<=? sq #\z))
                          (set-add (node-keys n) sq)
                          (node-keys n))))
    (for ([dx (in-list '(1 0 -1 0))]
          [dy (in-list '(0 1 0 -1))])
      (match-define (cons x y) (node-pos n))
      (define adj (struct-copy node n
                               [pos (cons (+ x dx) (+ y dy))]
                               [keys keyring]))
      (when (and (not (set-member? seen adj)) (not (wall? adj))
                 (can-enter? adj))
        (push-end! q (cons adj (add1 steps))))))
  (define start (for/first ([(p t) (in-hash area)]
                          #:when (equal? t #\@))
                  p))
  (define q (deque))
  (push-end! q (cons (node start '()) 0))
  (let walk ([seen (set)])
    (if (deque-empty? q)
        #f
        (let* ([item (pop-front! q)]
               [cur (car item)]
               [steps (cdr item)])
          (printf "@~a (~a): ~a~%" (node-pos cur) (node-keys cur)
                  (hash-ref area (node-pos cur) #\.))
          (cond
            [(found-all? cur) (values (sub1 steps) (node-keys cur))]
            [else (add-neighbors! cur steps seen) (walk (set-add seen cur))])))))

(define (distances area start)
  (define (square pos)
    (hash-ref area pos #f))
  (define (floor? sq)
    (or (equal? sq #\.)
        (equal? sq #\@)))
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


(define test-area (read-area test #:ignore '(#\#)))

(distances test-area (cons 15 1))

;(min-dists test-area)
;(distances test-area (cons 15 1) '())

(define (shortest-path mapstr)
  (define area (read-area mapstr #:ignore '(#\#)))
  (define keys (for/hash ([(pos sq) (in-hash area)]
                          #:when (or (char=? sq #\@)
                                     (char-lower-case? sq)))
                 (values sq pos)))
  (define dists (for/hash ([(key pos) (in-hash keys)])
                  (values key (distances area pos))))
  (define (adjacents from-keys)
    (match-define (cons from keys) from-keys)
    (for*/list ([(to node) (in-hash (hash-ref dists from))]
                [doors (in-value (car node))]
                [dist (in-value (cdr node))]
                #:when (and (not (set-member? keys to))
                            (andmap (curry set-member? keys) doors)))
      (cons to dist)))
  
  (let loop ([paths (hash (cons #\@ (set)) 0)])
    (define dists (for*/fold ([dists (hash)])
                             ([(path pathlen) (in-hash paths)]
                              [adj (in-list (adjacents path))])
                    (match-define (cons key dist) adj)
                    (define found (set-add (cdr path) key))
                    (define newnode (cons key found))
                    (define cached (hash-ref dists newnode #f))
                    (if (and cached (< cached (+ pathlen dist)))
                        dists
                        (hash-set dists newnode (+ pathlen dist)))))
    (if (= 0 (hash-count dists))
        (apply min (hash-values paths))
        (loop dists))))

(time (shortest-path test4))

(define input (file->string "input18.txt"))
(time (shortest-path input))















