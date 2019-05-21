#lang racket
(require rackunit
         threading)

(define (read-data fname)
  (define initrx (regexp "initial state: (.*)"))
  (define transrx (pregexp "(.{5}) => (.)"))
  (define plant? (curry eq? #\#))
  (with-input-from-file fname
    (lambda ()
      (define init (map plant?
                        (string->list (cadr (regexp-match initrx (read-line))))))
      (read-line)
      (define transitions (for/list ([l (in-lines)])
                            (match (regexp-match transrx l)
                              [(list _ mask result) (cons
                                                     (map plant? (string->list mask))
                                                     (string=? result "#"))])))
      (values (state-vector init) transitions))))

(struct pots (vec offset start stop) #:transparent)

(define (state-vector state-list)
  (define vec (list->vector state-list))
  (pots vec 0 0 (vector-length vec)))

(define-values (test-state test-rules) (read-data "day12_test.txt"))
(define-values (puzzle-state puzzle-rules) (read-data "day12_input.txt"))

(define (next-state slice rules)
  (define (match? mask)
    (andmap (compose not xor) mask slice))
   (let loop ([todo rules])
    (cond
      [(empty? todo) #f]
      [(match? (caar todo)) (cdar todo)]
      [else (loop (cdr todo))])))

(define (step state rules [steps 1])  
  (define (window vec i)
    (for/list ([j (in-range (- i 2) (+ i 3))])
      (if (or (< j 0) (>= j (vector-length vec)))
          #f
          (vector-ref vec j))))
  
  (define (step-1 state)
    (define oldvec (pots-vec state))
    (define newlen (+ 5 (- (pots-stop state) (pots-start state))))
    (define newvec (make-vector newlen #f))
    (for/fold ([first-plant #f]
               [last-plant #f]
               [sum 0]
               #:result (values (pots newvec
                                      (+ (pots-offset state) first-plant -2)
                                      first-plant
                                      last-plant)
                                sum))
              ([newi newlen]
               [oldi (in-range (- (pots-start state) 2) +inf.0)]
               [pot (in-range (- (pots-offset state) 2) +inf.0)])
      (define plant? (next-state (window oldvec oldi) rules))
      (if plant?
          (begin
            (vector-set! newvec newi plant?)
            (values (or first-plant newi) newi (+ sum pot)))
          (values first-plant last-plant sum))))
  
  (for/fold ([state state]
             [sum 0])
            ([i steps])
    (define-values (new-state newsum) (step-1 state))
    (printf "~a: ~a [~a] - ~a~%" (add1 i) (state->string new-state) newsum (- newsum sum))
    (values new-state newsum)))


(define (state->string state)
  (string-join
   (for/list ([pot (in-vector (pots-vec state))])
     (if pot "#" "."))
   ""))

(define (display-state-steps state rules [steps 20])
  (for/fold ([state test-state]
             [_ 0]
             [plants 0])
            ([i (add1 steps)])
    (printf "~a: ~a [~a]~%" i (state->string state) plants)
    (define-values (new-state len sum) (step state rules))
    (printf "Len: ~a~%" len)
    (values new-state len sum) )
  (void))

;; After a while it just keeps increasing the total sum by 98
;; 100 iterations = 11593

(define (puzzle-b steps)
  (define-values (state sum) (step puzzle-state puzzle-rules 100))
  (+ sum (* 98 (- steps 100))))
