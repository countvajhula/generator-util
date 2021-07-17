#lang racket/base

(require (prefix-in b: racket/base)
         (except-in racket/contract
                    predicate/c)
         racket/stream
         racket/match
         (prefix-in b: racket/generator)
         (only-in racket/generator
                  yield
                  sequence->generator
                  sequence->repeated-generator)
         (only-in racket/function
                  const
                  thunk*)
         racket/generic
         racket/undefined
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of)
         contract/social
         relation)

(require "private/util.rkt")

(provide gen:generator
         generator/c
         generator
         yield  ; reprovided from racket/generator
         (contract-out
          [struct gen ((primitive generator?))]
          [generator? predicate/c]
          [generator-state (function/c generator? symbol?)]
          [generate (->* ((or/c sequence? b:sequence?))
                         (any/c)
                         generator?)]
          [in-producer (->* (generator?)
                            (any/c)
                            #:rest (listof any/c)
                            sequence?)]
          [generator-null (->* ()
                               (any/c)
                               generator?)]
          [generator-cons (binary-constructor/c any/c
                                                generator?)]
          [make-generator (->* ()
                               #:rest (listof any/c)
                               generator?)]
          [generator-empty? (function/c generator?
                                        (values boolean? generator?))]
          [generator-done? (predicate/c generator?)]
          [generator-peek (function/c generator?
                                      (values any/c generator?))]
          [generator-map (binary-function/c function/c
                                            generator?
                                            generator?)]
          [generator-filter (binary-function/c predicate/c
                                               generator?
                                               generator?)]
          [generator-fold (->* (binary-function/c generator?)
                               (any/c #:order (one-of/c 'abb 'bab))
                               generator?)]
          [yield-from (function/c generator? any)]
          [generator-append (binary-composition/c generator?)]
          [generator-cycle (->* (generator?)
                                (any/c)
                                generator?)]
          [generator-repeat (encoder/c generator?)]
          [generator-zip-with (variadic-function/c procedure?
                                                   generator?
                                                   generator?)]
          [generator-zip (variadic-composition/c generator?)]
          [generator-interleave (variadic-composition/c generator?)]
          [generator-join (self-map/c generator?)]
          [generator-flatten (self-map/c generator?)]))

(module+ test
  (require rackunit
           rackunit/text-ui
           racket/stream
           (except-in racket/generator
                      generator?
                      generator-state)
           (only-in racket/function
                    thunk)
           (except-in data/collection
                      foldl
                      foldl/steps
                      append)
           relation))

(define-syntax-rule (generator formals body ...)
  (gen (b:generator formals body ...)))

(define-generics generator
  (generator-state generator)
  #:fast-defaults ([b:generator?
                    (define generator-state b:generator-state)]))

(struct gen (primitive)
  #:transparent

  #:methods gen:collection
  [(define (conj this v)
     (let ([g (gen-primitive this)])
       (generator-cons v g)))]

  #:methods gen:sequence
  [(define empty? (thunk* (error "Not implemented")))
   (define first (thunk* (error "Not implemented")))
   (define rest (thunk* (error "Not implemented")))]

  #:methods gen:generator
  [(define/generic -generator-state generator-state)
   (define (generator-state this)
     (let ([g (gen-primitive this)])
       (-generator-state g)))]

  #:property prop:procedure
  (λ (this . args)
    (let ([g (gen-primitive this)])
      (apply g args))))

(define (generator-null [return (void)])
  (generator ()
    return))

(define (generator-cons v g)
  (generator ()
    (yield v)
    (let ([cur (g)])
      (if (generator-done? g)
          cur
          (let loop ([cur cur]
                     [next (g)])
            (if (generator-done? g)
                (begin (yield cur)
                       (g))
                (begin (yield cur)
                       (loop next (g)))))))))

(define (make-generator #:return [return (void)] . vals)
  (match vals
    ['() (generator-null return)]
    [(cons v vs) (generator-cons v (apply make-generator #:return return vs))]))

(define (generate seq [return (void)])
  (generator ()
    (if (sequence? seq)
        (for-each (λ (v)
                    (yield v)
                    (void))
                  seq)
        ;; the contract ensures it's either a data/collection sequence
        ;; or a built-in sequence
        (yield-from (sequence->generator seq)))
    return))

(define (in-producer g [stop undefined] . args)
  (let ([pred (if (undefined? stop)
                  (const #t)
                  (!! (curry = stop)))])
    (take-while pred
                (build-sequence (apply unthunk g args)))))

(define (generator-empty? g)
  (if (generator-done? g)
      (values #t g)
      (let ([val (g)])
        (if (generator-done? g)
            (values #t
                    (generator-null val))
            (values #f
                    (generator-cons val g))))))

(define (generator-done? g)
  (= (generator-state g)
     'done))

(define (generator-peek g)
  (if (generator-done? g)
      (raise-argument-error 'generator-peek
                            "Non-empty generator in a non-terminal state."
                            g)
      (let ([val (g)])
        (if (generator-done? g)
            (raise-argument-error 'generator-peek
                            "Non-empty generator in a non-terminal state."
                            g)
            (values val
                    (generator-cons val g))))))

(define (generator-map f g)
  (generator ()
    (let loop ([cur (g)]
               [next (g)])
      (if (generator-done? g)
          (begin (yield (f cur))
                 (let ([result (g)])
                   (unless (void? result)
                     (f result))))
          (begin (yield (f cur))
                 (loop next (g)))))))

(define (generator-filter pred g)
  (generator ()
    (let loop ([cur (g)]
               [next (g)])
      (if (generator-done? g)
          (begin (when (pred cur)
                   (yield cur))
                 (let ([result (g)])
                   (unless (void? result)
                     (when (pred result)
                       result))))
          (begin (when (pred cur)
                   (yield cur))
                 (loop next (g)))))))

(define (generator-fold f g [base undefined] #:order [order 'abb])
  (generator ()
    (let-values ([(is-empty? g) (generator-empty? g)])
      (if is-empty?
          base
          (let-values ([(head g) (generator-peek g)])
            (let ([base (if (undefined? base)
                            ((id f) head)
                            base)]
                  [f (if (= order 'abb)
                         f
                         (flip f))])
              (let loop ([acc base]
                         [cur (g)]
                         [next (g)])
                (let ([acc (f cur acc)])
                  (if (generator-done? g)
                      (begin (yield acc)
                             (let ([result (g)])
                               (unless (void? result)
                                 (yield (f result acc)))))
                      (begin (yield acc)
                             (loop acc next (g))))))))))))

(define (yield-from g)
  (if (generator-done? g)
      (raise-argument-error 'yield-from
                            "Generator in a non-terminal state"
                            g)
      (let ([v (g)])
        (if (generator-done? g)
            v
            (begin (yield v)
                   (yield-from g))))))

(define (generator-append a b)
  (generator ()
    (yield-from a)
    (yield-from b)))

(define (generator-cycle g [stop (void)])
  (sequence->repeated-generator (in-producer g stop)))

(define (generator-repeat v)
  (sequence->repeated-generator (list v)))

(define (generator-zip-with f . gs)
  (generator ()
    (let loop ([curs (b:map (curryr apply null) gs)])
      (unless (any? (b:map generator-done? gs))
        (yield (apply f curs))
        (loop (b:map (curryr apply null) gs))))))

(define (generator-zip . gs)
  (apply generator-zip-with list gs))

(define (generator-interleave . gs)
  (generator ()
    (unless (empty? gs)
      (let loop ([remaining-gs gs])
        (if (empty? remaining-gs)
            (loop gs)
            (let ([first-g (first remaining-gs)])
              (let ([cur (first-g)])
                (if (generator-done? first-g)
                    cur
                    (begin (yield cur)
                           (loop (rest remaining-gs)))))))))))

(define (flatten-one-level vs)
  (if (sequence? vs)
      (for-each (λ (v)
                  (yield v)
                  (void))
                vs)
      (raise-argument-error 'flatten-one-level
                            "sequence?"
                            vs)))

(define (generator-join g)
  (generator ()
    (let ([cur (g)])
      (if (generator-done? g)
          cur
          (let loop ([cur cur]
                     [next (g)])
            (if (generator-done? g)
                (begin (flatten-one-level cur)
                       (let ([result (g)])
                         (unless (void? result)
                           (flatten-one-level result))))
                (begin (flatten-one-level cur)
                       (loop next (g)))))))))

(define (generator-flatten g)
  (let-values ([(is-empty? g) (generator-empty? g)])
    (if is-empty?
        g
        (let-values ([(v g) (generator-peek g)])
          (if (sequence? v)
              (generator-flatten (generator-join g))
              g)))))

(module+ test

  (define generator->list (.. ->list (curryr in-producer (void))))
  (define tests
    (test-suite
     "Tests for generator utilities"
     (check-equal? (generator->list (generator-cons 4 (->generator (list 1 2 3)))) '(4 1 2 3))
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
     (check-equal? (generator->list (make-generator 1 2 3)) '(1 2 3))
     (check-equal? (generator->list (make-generator)) '())
     (let ([g (make-generator)])
       (check-equal? (generator-state g) 'fresh)
       (check-true (void? (g)))
       (check-equal? (generator-state g) 'done))
     (check-equal? (generator->list (generator () (yield-from (->generator (list 1 2 3))))) '(1 2 3))
     (check-equal? (generator->list (generator () (yield-from (make-generator)))) '())
     (check-equal? (generator->list (generator () (yield-from (make-generator 1)))) '(1))
     (check-equal? (generator->list (generator-append (->generator (list 1 2 3)) (->generator (list 4 5 6)))) '(1 2 3 4 5 6))
     (check-equal? (generator->list (generator-append (->generator (list 1)) (->generator (list 4)))) '(1 4))
     (check-equal? (->list (take 5 (in-producer (generator-cycle (make-generator 1 2 3))))) '(1 2 3 1 2))
     (check-equal? (->list (take 5 (in-producer (generator-cycle (make-generator 1))))) '(1 1 1 1 1))
     (check-equal? (->list (take 3 (in-producer (generator-repeat 5)))) '(5 5 5))
     (check-equal? (->list (take 3 (in-producer (generator-repeat (void))))) (list (void) (void) (void)))
     (check-equal? (generator->list (generate (list 1 2 3))) (list 1 2 3))
     (check-equal? (generator->list (generate (stream 1 2 3))) (list 1 2 3))
     (check-equal? (let ([g (generate (list 1) 0)])
                     (g)
                     (g))
                   0)
     (check-equal? (generator->list (generate 3)) (list 0 1 2) "built-in (not data/collection) sequence")
     (check-equal? (let ([g (generate 1 "bye")])
                     (g)
                     (g))
                   "bye")
     (check-equal? (generator->list (generator-zip (->generator (list 1)) (->generator (list 4)))) '((1 4)))
     (check-equal? (generator->list (generator-zip (->generator (list 1 2 3)) (->generator (list 'a 'b 'c)))) '((1 a) (2 b) (3 c)))
     (check-equal? (generator->list (generator-zip (->generator (list)) (->generator (list)))) '())
     (check-equal? (generator->list (generator-zip (->generator (list 1 2 3)) (->generator (list 'a)))) '((1 a)))
     (check-equal? (generator->list (generator-zip (->generator (list 1 2 3)) (->generator (list 'a 'b)) (->generator (list 'A 'B 'C)))) '((1 a A) (2 b B)))
     (check-equal? (generator->list (generator-zip-with + (->generator (list 1 2 3)) (->generator (list 1 2 3)))) '(2 4 6))
     (check-equal? (generator->list (generator-zip-with + (->generator (list 1 2 3)) (->generator (list 1 2)))) '(2 4))
     (check-equal? (generator->list (generator-zip-with + (->generator (list)) (->generator (list)))) '())
     (check-equal? (generator->list (generator-interleave (make-generator 1 2 3) (make-generator 4 5 6))) '(1 4 2 5 3 6))
     (check-equal? (generator->list (generator-interleave (make-generator 1) (make-generator 2))) '(1 2))
     (check-equal? (generator->list (generator-interleave (make-generator 1 2) (make-generator 3))) '(1 3 2))
     (check-equal? (generator->list (generator-interleave (make-generator 1) (make-generator 2 3))) '(1 2))
     (check-equal? (generator->list (generator-interleave (make-generator 1) (make-generator))) '(1))
     (check-equal? (generator->list (generator-interleave (make-generator) (make-generator 1))) '())
     (check-equal? (generator->list (generator-interleave (make-generator) (make-generator))) '())
     (check-equal? (->list (in-producer (->generator (list 1 2 3 4 (void) 5 6))
                                        (void)))
                   '(1 2 3 4))
     (check-equal? (->list (take 7 (in-producer (->generator (list 1 2 3 4 (void) 5 6)))))
                   (list 1 2 3 4 (void) 5 6))
     (check-equal? (generator->list (generator-map add1 (->generator (list 1 2 3))))
                   '(2 3 4))
     (check-equal? (generator->list (generator-filter odd? (->generator (list 1 2 3 4 5 6))))
                   '(1 3 5))
     (check-equal? (generator->list (generator-filter even? (->generator (list 1 2 3 4 5 6))))
                   '(2 4 6))
     (check-equal? (generator->list (generator-join (->generator (list (list 1 2) (list 3 4) (list 5) (list 6)))))
                   '(1 2 3 4 5 6))
     (check-equal? (generator->list (generator-join (->generator (list (list 1 2 3 4 5 6)))))
                   '(1 2 3 4 5 6))
     (check-equal? (generator->list (generator-join (make-generator)))
                   '())
     (check-equal? (generator->list (generator-flatten (->generator (list (list 1 2) (list 3 4) (list 5) (list 6)))))
                   '(1 2 3 4 5 6))
     (check-equal? (generator->list (generator-flatten (->generator (list (list 1 2 3 4 5 6)))))
                   '(1 2 3 4 5 6))
     (check-equal? (generator->list (generator-flatten (->generator (list (list (list 1) (list 2)) (list (list 3) (list 4)) (list (list 5) (list 6))))))
                   '(1 2 3 4 5 6))
     (check-equal? (generator->list (generator-fold + (->generator (list 1 2 3 4))))
                   '(1 3 6 10))
     (check-equal? (generator->list (generator-fold +
                                                    (->generator (list 1 2 3 4))
                                                    12))
                   '(13 15 18 22))
     (check-equal? (generator->list (generator-fold * (->generator (list 1 2 3 4))))
                   '(1 2 6 24))
     (check-equal? (generator->list (generator-fold .. (->generator (list "aa" "bb" "cc"))))
                   (list "aa" "bbaa" "ccbbaa"))
     (check-equal? (generator->list (generator-fold ..
                                                    (->generator (list "aa" "bb" "cc"))
                                                    #:order 'bab))
                   (list "aa" "aabb" "aabbcc"))
     (let-values ([(head g) (generator-peek (->generator (list 1 2 3)))])
       (check-equal? head 1)
       (check-equal? (generator->list g) '(1 2 3)))
     (check-exn exn:fail? (thunk (generator-peek (generator () (void)))))
     (let-values ([(is-empty? g) (generator-empty? (make-generator))])
       (check-true is-empty?))
     (let-values ([(is-empty? g) (generator-empty? (make-generator 1))])
       (check-false is-empty?))
     (let ([g (make-generator)])
       (g)
       (check-true (generator-done? g))
       (let-values ([(is-empty? g) (generator-empty? g)])
         (check-true (generator-done? g)))))))

(module+ test
  (run-tests tests))
