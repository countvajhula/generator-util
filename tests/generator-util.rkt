#lang racket

(require rackunit
         rackunit/text-ui
         racket/stream
         (only-in racket/function
                  thunk)
         (except-in data/collection
                    foldl
                    foldl/steps
                    append)
         relation
         generator-util)

(define generator->list (.. ->list (curryr in-producer (void))))

(define tests
  (test-suite
   "Tests for generator utilities"

   (test-suite
    "generator-cons"
    (check-equal? (generator->list
                   (generator-cons 4 (->generator (list 1 2 3))))
                  '(4 1 2 3))
    (let ([g (generator-cons 4 (->generator (list 1)))])
      (g)
      (g)
      (g)
      (check-equal? (generator-state g) 'done))
    (let ([g (generator-cons 4 (generator-null))])
      (g)
      (g)
      (check-equal? (generator-state g) 'done)
      (check-equal? (g) (void)))
    (let ([g (generator-cons 4 (generator-null 5))])
      (g)
      (g)
      (check-equal? (generator-state g) 'done)
      (check-equal? (g) 5)))

   (test-suite
    "make-generator"
    (check-equal? (generator->list
                   (make-generator 1 2 3))
                  '(1 2 3))
    (check-equal? (generator->list
                   (make-generator))
                  '())
    (let ([g (make-generator)])
      (check-equal? (generator-state g) 'fresh)
      (check-true (void? (g)))
      (check-equal? (generator-state g) 'done)))

   (test-suite
    "generator and yield-from"
    (check-equal? (generator->list
                   (generator ()
                     (yield-from (->generator (list 1 2 3)))))
                  '(1 2 3))
    (check-equal? (generator->list
                   (generator ()
                     (yield-from (make-generator))))
                  '())
    (check-equal? (generator->list
                   (generator ()
                     (yield-from (make-generator 1))))
                  '(1))
    (check-equal? (call-with-values
                   (generator ()
                     (yield-from
                      (generator ()
                        (let loop ([i 3])
                          (when (> i 0)
                            (yield i 0)
                            (loop (sub1 i)))))))
                   list)
                  '(3 0)
                  "yield-from with multiple values"))

   (test-suite
    "generator-append"
    (check-equal? (generator->list
                   (generator-append (->generator (list 1 2 3))
                                     (->generator (list 4 5 6))))
                  '(1 2 3 4 5 6))
    (check-equal? (generator->list
                   (generator-append (->generator (list 1))
                                     (->generator (list 4))))
                  '(1 4)))

   (test-suite
    "generator-cycle"
    (check-equal? (->list
                   (take 5 (in-producer
                            (generator-cycle
                             (make-generator 1 2 3)))))
                  '(1 2 3 1 2))
    (check-equal? (->list
                   (take 5 (in-producer
                            (generator-cycle
                             (make-generator 1)))))
                  '(1 1 1 1 1))
    (check-equal? (call-with-values
                   (generator-cycle
                    (generator ()
                      (let loop ([i 3])
                        (when (> i 0)
                          (yield i 0)
                          (loop (sub1 i))))))
                   list)
                  '(3 0)
                  "cycle multi-valued generator"))

   (test-suite
    "generator-repeat"
    (check-equal? (->list
                   (take 3 (in-producer
                            (generator-repeat 5))))
                  '(5 5 5))
    (check-equal? (->list
                   (take 3 (in-producer
                            (generator-repeat (void)))))
                  (list (void) (void) (void)))
    (check-equal? (call-with-values
                   (generator-repeat 1 2 3)
                   list)
                  (list 1 2 3)))

   (test-suite
    "generate"
    (check-equal? (generator->list
                   (generate (list 1 2 3)))
                  (list 1 2 3))
    (check-equal? (generator->list
                   (generate (stream 1 2 3)))
                  (list 1 2 3))
    (check-equal? (let ([g (generate (list 1) 0)])
                    (g)
                    (g))
                  0)
    (check-equal? (generator->list (generate 3))
                  (list 0 1 2)
                  "built-in (not data/collection) sequence")
    (check-equal? (let ([g (generate 1 "bye")])
                    (g)
                    (g))
                  "bye"))

   (test-suite
    "generator-zip"
    (check-equal? (generator->list
                   (generator-zip (->generator (list 1))
                                  (->generator (list 4))))
                  '((1 4)))
    (check-equal? (generator->list
                   (generator-zip (->generator (list 1 2 3))
                                  (->generator (list 'a 'b 'c))))
                  '((1 a) (2 b) (3 c)))
    (check-equal? (generator->list
                   (generator-zip (->generator (list))
                                  (->generator (list))))
                  '())
    (check-equal? (generator->list
                   (generator-zip (->generator (list 1 2 3))
                                  (->generator (list 'a))))
                  '((1 a)))
    (check-equal? (generator->list
                   (generator-zip (->generator (list 1 2 3))
                                  (->generator (list 'a 'b))
                                  (->generator (list 'A 'B 'C))))
                  '((1 a A) (2 b B))))

   (test-suite
    "generator-zip-with"
    (check-equal? (generator->list
                   (generator-zip-with +
                                       (->generator (list 1 2 3))
                                       (->generator (list 1 2 3))))
                  '(2 4 6))
    (check-equal? (generator->list
                   (generator-zip-with +
                                       (->generator (list 1 2 3))
                                       (->generator (list 1 2))))
                  '(2 4))
    (check-equal? (generator->list
                   (generator-zip-with +
                                       (->generator (list))
                                       (->generator (list))))
                  '()))

   (test-suite
    "generator-interleave"
    (check-equal? (generator->list
                   (generator-interleave (make-generator 1 2 3)
                                         (make-generator 4 5 6)))
                  '(1 4 2 5 3 6))
    (check-equal? (generator->list
                   (generator-interleave (make-generator 1)
                                         (make-generator 2)))
                  '(1 2))
    (check-equal? (generator->list
                   (generator-interleave (make-generator 1 2)
                                         (make-generator 3)))
                  '(1 3 2))
    (check-equal? (generator->list
                   (generator-interleave (make-generator 1)
                                         (make-generator 2 3)))
                  '(1 2))
    (check-equal? (generator->list
                   (generator-interleave (make-generator 1)
                                         (make-generator)))
                  '(1))
    (check-equal? (generator->list
                   (generator-interleave (make-generator)
                                         (make-generator 1)))
                  '())
    (check-equal? (generator->list
                   (generator-interleave (make-generator)
                                         (make-generator)))
                  '())
    (let ([g (generator-interleave (generator ()
                                     (let loop ([i 3])
                                       (when (> i 0)
                                         (yield i 1)
                                         (loop (sub1 i)))))
                                   (make-generator 3 2 1))])
      (check-equal? (call-with-values g list)
                    '(3 1))
      (check-equal? (call-with-values g list)
                    '(3))))

   (test-suite
    "generator-juxtapose"
    (check-equal? (call-with-values
                   (generator-juxtapose (make-generator 1 2 3)
                                        (make-generator 4 5 6))
                   list)
                  '(1 4))
    (check-equal? (call-with-values
                   (generator-juxtapose (make-generator 1 2)
                                        (make-generator #:return 0))
                   list)
                  '(1 0))
    (check-equal? (call-with-values
                   (generator-juxtapose (make-generator #:return 0)
                                        (make-generator 1 2))
                   list)
                  '(0 1))
    (let ([g (generator-juxtapose (generator ()
                                    (let loop ([i 3])
                                      (when (> i 0)
                                        (yield i 1)
                                        (loop (sub1 i)))))
                                  (make-generator 3 2 1))])
      (check-equal? (call-with-values g list)
                    '(3 1 3))
      (check-equal? (call-with-values g list)
                    '(2 1 2))))
   (test-suite
    "in-producer"
    (check-equal? (->list
                   (in-producer
                    (->generator (list 1 2 3 4 (void) 5 6))
                    (void)))
                  '(1 2 3 4))
    (check-equal? (->list
                   (take 7 (in-producer
                            (->generator
                             (list 1 2 3 4 (void) 5 6)))))
                  (list 1 2 3 4 (void) 5 6)))

   (test-suite
    "generator-map"
    (check-equal? (generator->list
                   (generator-map add1
                                  (->generator
                                   (list 1 2 3))))
                  '(2 3 4))
    (check-equal? (generator->list
                   (generator-map +
                                  (generator ()
                                    (let loop ([i 3])
                                      (when (> i 0)
                                        (yield i 1)
                                        (loop (sub1 i)))))))
                  '(4 3 2)
                  "map over multi-valued generators")
    (check-equal? (call-with-values
                   (generator-map values
                                  (generator ()
                                    (let loop ([i 3])
                                      (when (> i 0)
                                        (yield i 1)
                                        (loop (sub1 i))))))
                   list)
                  '(3 1)
                  "multi-valued map over multi-valued generators"))

   (test-suite
    "generator-filter"
    (check-equal? (generator->list
                   (generator-filter odd?
                                     (->generator
                                      (list 1 2 3 4 5 6))))
                  '(1 3 5))
    (check-equal? (generator->list
                   (generator-filter even?
                                     (->generator
                                      (list 1 2 3 4 5 6))))
                  '(2 4 6))
    (check-equal? (call-with-values
                   (generator-filter (compose odd? +)
                                     (generator ()
                                       (let loop ([i 3])
                                         (when (> i 0)
                                           (yield i 1)
                                           (loop (sub1 i))))))
                   list)
                  '(2 1)
                  "filter over multi-valued generators"))

   (test-suite
    "generator-join"
    (check-equal? (generator->list
                   (generator-join
                    (->generator
                     (list (list 1 2)
                           (list 3 4)
                           (list 5)
                           (list 6)))))
                  '(1 2 3 4 5 6))
    (check-equal? (generator->list
                   (generator-join
                    (->generator
                     (list (list 1 2 3 4 5 6)))))
                  '(1 2 3 4 5 6))
    (check-equal? (generator->list
                   (generator-join (make-generator)))
                  '()))

   (test-suite
    "generator-flatten"
    (check-equal? (generator->list
                   (generator-flatten
                    (->generator
                     (list (list 1 2)
                           (list 3 4)
                           (list 5)
                           (list 6)))))
                  '(1 2 3 4 5 6))
    (check-equal? (generator->list
                   (generator-flatten
                    (->generator
                     (list (list 1 2 3 4 5 6)))))
                  '(1 2 3 4 5 6))
    (check-equal? (generator->list
                   (generator-flatten
                    (->generator
                     (list (list (list 1)
                                 (list 2))
                           (list (list 3)
                                 (list 4))
                           (list (list 5)
                                 (list 6))))))
                  '(1 2 3 4 5 6)))

   (test-suite
    "generator-fold"
    (check-equal? (generator->list
                   (generator-fold +
                                   (->generator
                                    (list 1 2 3 4))))
                  '(1 3 6 10))
    (check-equal? (generator->list
                   (generator-fold +
                                   (->generator
                                    (list 1 2 3 4))
                                   12))
                  '(13 15 18 22))
    (check-equal? (generator->list
                   (generator-fold *
                                   (->generator
                                    (list 1 2 3 4))))
                  '(1 2 6 24))
    (check-equal? (generator->list
                   (generator-fold ..
                                   (->generator
                                    (list "aa" "bb" "cc"))))
                  (list "aa" "bbaa" "ccbbaa"))
    (check-equal? (generator->list
                   (generator-fold ..
                                   (->generator
                                    (list "aa" "bb" "cc"))
                                   #:order 'bab))
                  (list "aa" "aabb" "aabbcc")))

   (test-suite
    "generator-peek"
    (let-values ([(head g) (generator-peek
                            (->generator
                             (list 1 2 3)))])
      (check-equal? head 1)
      (check-equal? (generator->list g) '(1 2 3)))
    (check-exn exn:fail? (thunk
                          (generator-peek
                           (generator () (void))))))

   (test-suite
    "generator-empty?"
    (let-values ([(is-empty? g) (generator-empty? (make-generator))])
      (check-true is-empty?))
    (let-values ([(is-empty? g) (generator-empty? (make-generator 1))])
      (check-false is-empty?)))

   (test-suite
    "generator-done?"
    (let ([g (make-generator)])
      (g)
      (check-true (generator-done? g))
      (let-values ([(is-empty? g) (generator-empty? g)])
        (check-true (generator-done? g)))))))

(module+ test
  (void (run-tests tests)))
