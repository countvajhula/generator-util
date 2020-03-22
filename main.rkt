#lang racket/base

(module+ test
  (require rackunit)
  (require racket/stream)
  (require racket/function)
  (require (except-in data/collection
                      foldl
                      foldl/steps
                      append))
  (require relation))

;; Code here

(require "utils.rkt")

(provide (all-from-out "utils.rkt"))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (->list (generator-cons 4 (->generator (list 1 2 3)))) '(4 1 2 3))
  (check-equal? (->list (generator-append (->generator (list 1 2 3)) (->generator (list 4 5 6)))) '(1 2 3 4 5 6))
  (check-equal? (->list (generator-append (->generator (list 1)) (->generator (list 4)))) '(1 4))
  (check-equal? (let-values ([(a b)
                              (generator-splitf-at (->generator (list 1 3 5 2 4)) odd?)])
                  (->list (map ->list (list a b))))
                (list '(1 3 5) '(2 4)))
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
                '(2 4 6)))

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
