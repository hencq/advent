#lang racket
(require rackunit
         threading)

(module+ test
  (define steps
    (list
     "Step C must be finished before step A can begin."
     "Step C must be finished before step F can begin."
     "Step A must be finished before step B can begin."
     "Step A must be finished before step D can begin."
     "Step B must be finished before step E can begin."
     "Step D must be finished before step E can begin."
     "Step F must be finished before step E can begin."))
  (check-equal? (string-join (traverse (build-graph steps)) "") "CABDFE"))

(define (build-graph steps)
  (define matcher "Step (.) must be finished before step (.)")
  (for/fold ([graph (hash)])
            ([s (in-list steps)])
    (match (regexp-match matcher s)
      [(list _ req step) (~> graph
                             (hash-update step (curry cons req) '())
                             (hash-update req values '()))])))

(define (ready? graph completed step)
  (define reqs (hash-ref graph step))
  (or (empty? reqs)
      (andmap (curry set-member? completed) reqs)))

(define (candidates graph completed)
  (sort (for/list ([(s _) (in-hash graph)]
                   #:when (and (not (set-member? completed s))
                               (ready? graph completed s)))
          s)
        string<?))

(define (candidates/wip graph completed wip)
  (define wip-set (map car wip))
  (sort (for/list ([(s _) (in-hash graph)]
                   #:when (and (not (set-member? completed s))
                               (not (set-member? wip-set s))
                               (ready? graph completed s)))
          s)
        string<?))

(define (traverse graph)
  (let loop ([completed '()])
    (define cands (candidates graph completed))
    (if (empty? cands)
        (reverse completed)
        (loop (cons (car cands) completed)))))

(define (get-order steps)
  (~> steps
      (build-graph)
      (traverse)
      (string-join "")))

(define (time-for-step step)
  (+ 60
     (- (char->integer (string-ref step 0)) 64)))

(define (multi-traverse graph workers)
  (let loop ([time 0]
             [completed '()]
             [available workers]
             [wip '()])
    (define cands (candidates/wip graph completed wip))
    (cond
      ;; If there are no more candidates and nothing underway, we're done
      [(and (empty? cands)
            (empty? wip)) time]
      ;; If we're out of candidates or we don't have available workers, advance time to the
      ;; next item that's done and free the worker
      [(or (empty? cands)
           (= available 0)) (let ([item (car wip)])
                              (loop (cdr item)
                                    (cons (car item) completed)
                                    (add1 available)
                                    (cdr wip)))]
      ;; In other situations we have available workers and candidate tasks to assign to them
      ;; Assign the task for completion in the future by adding it to the wip list
      [else  (loop time
                   completed
                   (sub1 available)
                   (sort (cons (cons (car cands) (+ time (time-for-step (car cands))))
                               wip)
                         < #:key cdr))])))

(define steps
  (list
"Step X must be finished before step C can begin."
"Step C must be finished before step G can begin."
"Step F must be finished before step G can begin."
"Step U must be finished before step Y can begin."
"Step O must be finished before step S can begin."
"Step D must be finished before step N can begin."
"Step M must be finished before step H can begin."
"Step J must be finished before step Q can begin."
"Step G must be finished before step R can begin."
"Step I must be finished before step N can begin."
"Step R must be finished before step K can begin."
"Step A must be finished before step Z can begin."
"Step Y must be finished before step L can begin."
"Step H must be finished before step P can begin."
"Step K must be finished before step S can begin."
"Step Z must be finished before step P can begin."
"Step T must be finished before step S can begin."
"Step N must be finished before step P can begin."
"Step E must be finished before step S can begin."
"Step S must be finished before step W can begin."
"Step W must be finished before step V can begin."
"Step L must be finished before step V can begin."
"Step P must be finished before step B can begin."
"Step Q must be finished before step V can begin."
"Step B must be finished before step V can begin."
"Step P must be finished before step Q can begin."
"Step S must be finished before step V can begin."
"Step C must be finished before step Q can begin."
"Step I must be finished before step H can begin."
"Step A must be finished before step E can begin."
"Step H must be finished before step Q can begin."
"Step G must be finished before step V can begin."
"Step N must be finished before step L can begin."
"Step R must be finished before step Q can begin."
"Step W must be finished before step L can begin."
"Step X must be finished before step L can begin."
"Step X must be finished before step J can begin."
"Step W must be finished before step P can begin."
"Step U must be finished before step B can begin."
"Step P must be finished before step V can begin."
"Step O must be finished before step P can begin."
"Step W must be finished before step Q can begin."
"Step S must be finished before step Q can begin."
"Step U must be finished before step Z can begin."
"Step Z must be finished before step T can begin."
"Step M must be finished before step T can begin."
"Step A must be finished before step P can begin."
"Step Z must be finished before step B can begin."
"Step N must be finished before step S can begin."
"Step H must be finished before step N can begin."
"Step J must be finished before step E can begin."
"Step M must be finished before step J can begin."
"Step R must be finished before step A can begin."
"Step A must be finished before step Y can begin."
"Step F must be finished before step V can begin."
"Step L must be finished before step P can begin."
"Step K must be finished before step L can begin."
"Step F must be finished before step P can begin."
"Step G must be finished before step L can begin."
"Step I must be finished before step Q can begin."
"Step C must be finished before step L can begin."
"Step I must be finished before step Y can begin."
"Step G must be finished before step B can begin."
"Step H must be finished before step L can begin."
"Step X must be finished before step U can begin."
"Step I must be finished before step K can begin."
"Step R must be finished before step N can begin."
"Step I must be finished before step L can begin."
"Step M must be finished before step I can begin."
"Step K must be finished before step V can begin."
"Step G must be finished before step E can begin."
"Step F must be finished before step B can begin."
"Step O must be finished before step Y can begin."
"Step Y must be finished before step Q can begin."
"Step F must be finished before step K can begin."
"Step N must be finished before step W can begin."
"Step O must be finished before step R can begin."
"Step N must be finished before step E can begin."
"Step M must be finished before step V can begin."
"Step H must be finished before step T can begin."
"Step Y must be finished before step T can begin."
"Step F must be finished before step J can begin."
"Step F must be finished before step O can begin."
"Step W must be finished before step B can begin."
"Step T must be finished before step E can begin."
"Step T must be finished before step P can begin."
"Step F must be finished before step M can begin."
"Step U must be finished before step I can begin."
"Step H must be finished before step S can begin."
"Step S must be finished before step P can begin."
"Step T must be finished before step W can begin."
"Step A must be finished before step N can begin."
"Step O must be finished before step N can begin."
"Step L must be finished before step B can begin."
"Step U must be finished before step K can begin."
"Step Z must be finished before step W can begin."
"Step X must be finished before step D can begin."
"Step Z must be finished before step L can begin."
"Step I must be finished before step T can begin."
"Step O must be finished before step W can begin."
"Step I must be finished before step B can begin."))
