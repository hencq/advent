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


(define (distances area start-keys)
  (match-define (cons start keys) start-keys)
  (define (square pos)
    (hash-ref area pos #f))
  (define (floor? pos)
    (define sq (square pos))
    (or 
     (equal? sq #\.)
     (equal? sq #\@)
     (and (char? sq) (set-member? keys (char-downcase sq)))))
  (define (unlocked-door? pos)
    (define sq (square pos))
    (and (char? sq) (char>=? sq #\A) (char<=? sq #\Z)
         (set-member? keys (char-downcase sq))))
  (define (key? pos)
    (define sq (square pos))
    (and (char? sq) (char>=? sq #\a) (char<=? sq #\z)
         (not (set-member? keys sq))))
  (define q (deque))
  (define (add-neighbors! pos dist dists)
    (for ([dx (in-list '(1 0 -1 0))]
          [dy (in-list '(0 1 0 -1))])
      (define adj (cons (+ (car pos) dx) (+ (cdr pos) dy)))
      (when (and (floor? pos)
                 (not (hash-has-key? dists adj))
                 (or (floor? adj) (unlocked-door? adj) (key? adj)))
        (push-end! q (cons adj (add1 dist))))))
  (push-end! q (cons start 0))
  (let loop ([dists (hash)]
             [key-dists (hash)])
    (if (deque-empty? q)
        key-dists
        (match-let* ([(cons pos dist) (pop-front! q)]
                     [sq (square pos)])
          (add-neighbors! pos dist dists)
          (loop (hash-set dists pos dist)
                (if (key? pos)
                    (hash-set key-dists (cons pos (set-add keys sq)) dist)
                    key-dists))))))





(define test-area (read-area test #:ignore '(#\#)))
;(min-dists test-area)
;(distances test-area (cons 15 1) '())

(define (shortest-path mapstr)
  (define area (read-area mapstr #:ignore '(#\#)))
  (define start (for/first ([(p t) (in-hash area)]
                          #:when (equal? t #\@))
                  p))
  (define keys (map (compose car string->list)
                    (regexp-match* "[a-z]" mapstr)))
  (let loop ([i (sub1 (length keys))]
             [squares (distances area (cons start (set)))])
    (if (= i 0)
        (apply min (hash-values squares))
        (let ()
          (define dists (for*/fold ([dists (hash)])
                                   ([(sq dist) (in-hash squares)]
                                    [(adj delta) (in-hash (distances area sq))])
                          (define cached (hash-ref dists adj #f))
                          (if (and cached (< cached (+ dist delta)))
                              dists
                              (hash-set dists adj (+ dist delta)))))
          (loop (sub1 i) dists)))))


;(distances test-area (cons 15 1) '())

(define test2 "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################")

;(min-dists test-area)
;(shortest-path test3)
;(shortest-path test3)
;(shortest-path test3)
(define test2-area (read-area test2 #:ignore '(#\#)))
;(distances test2-area (cons 6 3) '())
;(distances test2-area (cons 16 1) '(#\b))
;(distances test2-area (cons 8 3) '(#\a #\b))



(define test3 "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################")

;(shortest-path test3)

(define input (file->string "input18.txt"))
(shortest-path input)

;(shortest-path test3)
(define test3-area (read-area test3 #:ignore '(#\#)))
;test3-area
;(distances test3-area (cons 8 4) '())
;(distances test3-area (cons 1 3) '(#\g #\h #\a #\b #\j))

;(find-keys test3)
;Solution! 148 (m k n i c p e o l j b f d h a g)
;; (distances test3-area (cons 1 1) '(#\i #\c #\p #\e #\o #\l #\j #\b #\f #\d
;;                                     #\h #\a #\g))


(define test4 "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################")

;(find-keys test4)
