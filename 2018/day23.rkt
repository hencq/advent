#lang racket
(require data/heap)

(struct bot (x y z r) #:transparent)

(define (parse lines)
  (define regex (pregexp "pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)"))
  (for/list ([l (in-list lines)])
    (apply bot (map string->number (cdr (regexp-match regex l))))))

(define test-bots (parse (list
                     "pos=<0,0,0>, r=4"
                     "pos=<1,0,0>, r=1"
                     "pos=<4,0,0>, r=3"
                     "pos=<0,2,0>, r=1"
                     "pos=<0,5,0>, r=3"
                     "pos=<0,0,3>, r=1"
                     "pos=<1,1,1>, r=1"
                     "pos=<1,1,2>, r=1"
                     "pos=<1,3,1>, r=1")))

(define (distance b1 b2)
  (+ (abs (- (bot-x b1) (bot-x b2)))
     (abs (- (bot-y b1) (bot-y b2)))
     (abs (- (bot-z b1) (bot-z b2)))))

(define (in-range bots)
  (define sorted (sort bots > #:key bot-r))
  (filter (lambda (b)
            (<= (distance (car sorted) b) (bot-r (car sorted))))
          sorted))

(define bots (parse (file->lines "day23_input.txt")))

(define (dimensions bots)
  (for*/fold ([xmin +inf.0]
              [xmax -inf.0]
              [ymin +inf.0]
              [ymax -inf.0]
              [zmin +inf.0]
              [zmax -inf.0])
             ([b (in-list bots)]
              [xmin* (in-value (- (bot-x b) (bot-r b)))]
              [xmax* (in-value (+ (bot-x b) (bot-r b)))]
              [ymin* (in-value (- (bot-y b) (bot-r b)))]
              [ymax* (in-value (+ (bot-y b) (bot-r b)))]
              [zmin* (in-value (- (bot-z b) (bot-r b)))]
              [zmax* (in-value (+ (bot-z b) (bot-r b)))])
    (values (min xmin xmin*)
            (max xmax xmax*)
            (min ymin ymin*)
            (max ymax ymax*)
            (min zmin zmin*)
            (max zmax zmax*))))

(define test-bots2 (parse (list
                           "pos=<10,12,12>, r=2"
                           "pos=<12,14,12>, r=2"
                           "pos=<16,12,12>, r=4"
                           "pos=<14,14,14>, r=6"
                           "pos=<50,50,50>, r=200"
                           "pos=<10,10,10>, r=5")))

(define (children cube)
  (define d (/ (bot-r cube) 2))
  (define segments (list (add1 (- 0 d)) d))
  (for*/list ([dx (in-list segments)]
              [dy (in-list segments)]
              [dz (in-list segments)])
    (bot (+ (bot-x cube) dx)
         (+ (bot-y cube) dy)
         (+ (bot-z cube) dz)
         d)))

(define (closest-axis-point point origin distance)
  (if (<= point origin)
      (max (add1 (- origin distance)) point)
      (min (sub1 (+ origin distance)) point)))

(define (in-cube? cube b)
  (define closest (bot (closest-axis-point (bot-x b) (bot-x cube) (bot-r cube))
                       (closest-axis-point (bot-y b) (bot-y cube) (bot-r cube))
                       (closest-axis-point (bot-z b) (bot-z cube) (bot-r cube))
                       0))
  (<= (distance closest b) (bot-r b)))

(define (root-cube bots)
  (define (min-axis axis) (axis (argmin axis bots)))
  (define (max-axis axis) (axis (argmax axis bots)))
  (define (center l r) (+ l (floor (/ (- r l) 2))))
  (define minx (min-axis bot-x))
  (define miny (min-axis bot-y))
  (define minz (min-axis bot-z))
  (define maxx (max-axis bot-x))
  (define maxy (max-axis bot-y))
  (define maxz (max-axis bot-z))
  (define d (expt 2 (truncate (log (max (- maxx minx)
                                        (- maxy miny)
                                        (- maxz minz))
                                   2))))
  (bot (center minx maxx)
       (center miny maxy)
       (center minz maxz)
       d))

(define (bfs bots)
  (define root (root-cube bots))
  (define q (make-heap (lambda (a b)
                         (> (length (cdr a)) (length (cdr b))))))
  (heap-add! q (cons root bots))
  (let loop ()
    (define node (heap-min q))
    (define cube (car node))
    (define bots (cdr node))
    (heap-remove-min! q)
    (if (= (bot-r cube) 1)
        (values cube
                (+ (bot-x cube)
                   (bot-y cube)
                   (bot-z cube))
                (length bots))
        (let ()
          (for ([child (in-list (children cube))])
            (define child-bots (filter (curry in-cube? child) bots))
            (heap-add! q (cons child child-bots)))
          (loop)))))



