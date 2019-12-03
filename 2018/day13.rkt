#lang racket
(require rackunit
         racket/hash)

(struct cart (loc dir switch) #:transparent)

(define (read-track fname)
  (define lines (file->lines fname))
  (for/fold ([track (hash)]
             [carts '()])
            ([y (in-naturals)]
             [l (in-list lines)])
    (define-values (line-track line-carts)
      (for/fold ([track (hash)]
                 [carts '()])
                ([x (in-naturals)]
                 [ch (in-string l)])
        (define loc (cons x y))
        (case ch
          [(#\- #\\ #\/ #\| #\+) (values (hash-set track loc ch) carts)]
          [(#\^) (values (hash-set track loc #\|) (cons (cart (cons x y) ch 'left) carts))]
          [(#\>) (values (hash-set track loc #\-) (cons (cart (cons x y) ch 'left) carts))]
          [(#\v) (values (hash-set track loc #\|) (cons (cart (cons x y) ch 'left) carts))]
          [(#\<) (values (hash-set track loc #\-) (cons (cart (cons x y) ch 'left) carts))]
          [else (values track carts)])))
    (values (hash-union track line-track) (append carts line-carts))))

(define-values (test-track test-carts) (read-track "day13_test.txt"))
(define-values (test-track2 test-carts2) (read-track "day13_test2.txt"))
(define-values (track carts) (read-track "day13_input.txt"))

(define (next-loc ct [dir #f])
  (define new-dir (or dir (cart-dir ct)))
  (define new-loc
    (match/values (values (cart-loc ct) new-dir)
      [((cons x y) #\^) (cons x (sub1 y))]
      [((cons x y) #\>) (cons (add1 x) y)]
      [((cons x y) #\v) (cons x (add1 y))]
      [((cons x y) #\<) (cons (sub1 x) y)]))
  (struct-copy cart ct [loc new-loc] [dir new-dir]))

(define/match (next-reldir reldir)
  [('left) 'straight]
  [('straight) 'right]
  [('right) 'left])

(define/match (right dir)
  [(#\^) #\>]
  [(#\>) #\v]
  [(#\v) #\<]
  [(#\<) #\^])

(define/match (left dir)
  [(#\^) #\<]
  [(#\<) #\v]
  [(#\v) #\>]
  [(#\>) #\^])

(define (switch-dir c)
  (define new-dir (match (cart-switch c)
                    ['straight (cart-dir c)]
                    ['left (left (cart-dir c))]
                    ['right (right (cart-dir c))]))
  (next-loc (struct-copy cart c [dir new-dir] [switch (next-reldir (cart-switch c))])))

(define/match (take-turn ct track)
  [((cart loc #\> sw) #\\) (next-loc ct #\v)]
  [((cart loc #\^ sw) #\\) (next-loc ct #\<)]
  [((cart loc #\< sw) #\\) (next-loc ct #\^)]
  [((cart loc #\v sw) #\\) (next-loc ct #\>)]
  [((cart loc #\> sw) #\/) (next-loc ct #\^)]
  [((cart loc #\^ sw) #\/) (next-loc ct #\>)]
  [((cart loc #\< sw) #\/) (next-loc ct #\v)]
  [((cart loc #\v sw) #\/) (next-loc ct #\<)])


(define (update-cart ct track)
  (match track
    [#\- (next-loc ct)]
    [#\| (next-loc ct)]
    [#\/ (take-turn ct #\/)]
    [#\\ (take-turn ct #\\)]
    [#\+ (switch-dir ct)]))

(define (sort-carts carts)
  (sort carts (lambda (c1 c2)
                (define-values (x1 y1 x2 y2) (values
                                              (car (cart-loc c1))
                                              (cdr (cart-loc c1))
                                              (car (cart-loc c2))
                                              (cdr (cart-loc c2))))
                (cond
                  [(< y1 y2) #t]
                  [(and (= y1 y2)
                        (< x1 x2)) #t]
                  [else #f]))))

;; This is wrong because crashes can occur at any time, not just at the end
;; (define (update-state tracks carts)
;;   (for/list ([ct (in-list (sort-carts carts))])
;;     (update-cart ct (hash-ref tracks (cart-loc ct)))))

(define (crash? carts)
  (let loop ([carts carts]
             [locs (set)])
    (cond
      [(empty? carts) #f]
      [(set-member? locs (cart-loc (car carts))) (cart-loc (car carts))]
      [else (loop (cdr carts) (set-add locs (cart-loc (car carts))))])))

(define (run-until-crash tracks carts)
  (let loop ([to-run (sort-carts carts)]
             [ran '()]
             [i 0])
    (if (empty? to-run)
        (loop (sort-carts ran) '() i)
        (let ([crash-loc (crash? (append to-run ran))])
          (cond
            [crash-loc (values i crash-loc)]
            [else (loop (cdr to-run)
                        (cons (update-cart (car to-run) (hash-ref tracks
                                                                  (cart-loc (car to-run))))
                              ran)
                        (add1 i))])))))

(define (run-until-last tracks carts #:show [show #f])
  (define (remove-loc loc carts)
    (remove* (list loc) carts
             (lambda (loc ct)
               (equal? loc (cart-loc ct)))))
  
  (let loop ([to-run (sort-carts carts)]
             [ran '()]
             [i 0])
    (when (and show (empty? ran))
      (display-state tracks (append to-run ran))
      (displayln ""))
    (cond
      [(and (empty? to-run) (empty? ran)) (values i #f)]
      [(and (empty? to-run) (= 1 (length ran))) (values i (cart-loc (car ran)))]
      [(empty? to-run) (loop (sort-carts ran) '() i)]
      [else (let* ([all-carts (sort-carts (append to-run ran))]
                   [crash-loc (crash? all-carts)])
              (cond
                [crash-loc (loop (remove-loc crash-loc to-run)
                                 (remove-loc crash-loc ran)
                                 i)]
                [else (loop (cdr to-run)
                            (cons (update-cart (car to-run) (hash-ref tracks
                                                                      (cart-loc (car to-run))))
                                  ran)
                            (add1 i))]))])))

(define (display-state tracks carts)
  (define cart-locs (for/fold ([locs (hash)])
                              ([c (in-list carts)])
                      (hash-set locs (cart-loc c) (cart-dir c))))
  (define combined (hash-union tracks cart-locs #:combine (lambda (_ l) l)))
  (define rows (cdr (argmax cdr (hash-keys tracks))))
  (define cols (car (argmax car (hash-keys tracks))))
  (for ([y (add1 rows)])
    (displayln (list->string 
                (for/list ([x (add1 cols)])
                  (hash-ref combined (cons x y) #\space))))))


