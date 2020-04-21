#lang racket/base

(module+ test
  (require rackunit
           racket/stream
           racket/generator
           (only-in racket/function
                    thunk)
           (except-in data/collection
                      foldl
                      foldl/steps
                      append)
           relation))

;; Code here

(require "data/generator.rkt")

(provide (all-from-out "data/generator.rkt"))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (->list (generator-cons 4 (->generator (list 1 2 3)))) '(4 1 2 3))
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
    (check-equal? (g) 5))
  (check-equal? (->list (make-generator 1 2 3)) '(1 2 3))
  (check-equal? (->list (make-generator)) '())
  (let ([g (make-generator)])
    (check-equal? (generator-state g) 'fresh)
    (check-true (void? (g)))
    (check-equal? (generator-state g) 'done))
  (check-equal? (->list (generator-append (->generator (list 1 2 3)) (->generator (list 4 5 6)))) '(1 2 3 4 5 6))
  (check-equal? (->list (generator-append (->generator (list 1)) (->generator (list 4)))) '(1 4))
  (check-equal? (->list (in-producer (->generator (list 1 2 3 4 (void) 5 6))
                                     (void)))
                '(1 2 3 4))
  (check-equal? (->list (take 7 (in-producer (->generator (list 1 2 3 4 (void) 5 6)))))
                (list 1 2 3 4 (void) 5 6))
  (check-equal? (->list (generator-map add1 (->generator (list 1 2 3))))
                '(2 3 4))
  (check-equal? (->list (generator-filter odd? (->generator (list 1 2 3 4 5 6))))
                '(1 3 5))
  (check-equal? (->list (generator-filter even? (->generator (list 1 2 3 4 5 6))))
                '(2 4 6))
  (check-equal? (->list (generator-join (->generator (list (list 1 2) (list 3 4) (list 5) (list 6)))))
                '(1 2 3 4 5 6))
  (check-equal? (->list (generator-join (->generator (list (list 1 2 3 4 5 6)))))
                '(1 2 3 4 5 6))
  (check-equal? (->list (generator-flatten (->generator (list (list 1 2) (list 3 4) (list 5) (list 6)))))
                '(1 2 3 4 5 6))
  (check-equal? (->list (generator-flatten (->generator (list (list 1 2 3 4 5 6)))))
                '(1 2 3 4 5 6))
  (check-equal? (->list (generator-flatten (->generator (list (list (list 1) (list 2)) (list (list 3) (list 4)) (list (list 5) (list 6))))))
                '(1 2 3 4 5 6))
  (check-equal? (->list (generator-fold + (->generator (list 1 2 3 4))))
                '(1 3 6 10))
  (check-equal? (->list (generator-fold +
                                        (->generator (list 1 2 3 4))
                                        12))
                '(13 15 18 22))
  (check-equal? (->list (generator-fold * (->generator (list 1 2 3 4))))
                '(1 2 6 24))
  (check-equal? (->list (generator-fold .. (->generator (list "aa" "bb" "cc"))))
                (list "aa" "bbaa" "ccbbaa"))
  (check-equal? (->list (generator-fold ..
                                        (->generator (list "aa" "bb" "cc"))
                                        #:order 'bab))
                (list "aa" "aabb" "aabbcc"))
  (let-values ([(head gen) (generator-peek (->generator (list 1 2 3)))])
    (check-equal? head 1)
    (check-equal? (->list gen) '(1 2 3)))
  (check-exn exn:fail? (thunk (generator-peek (generator () (void)))))
  (let-values ([(is-empty? gen) (generator-empty? (make-generator))])
    (check-true is-empty?))
  (let-values ([(is-empty? gen) (generator-empty? (make-generator 1))])
    (check-false is-empty?))
  (let ([gen (make-generator)])
    (gen)
    (check-true (generator-done? gen))
    (let-values ([(is-empty? gen) (generator-empty? gen)])
      (check-true (generator-done? gen)))))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who))))
