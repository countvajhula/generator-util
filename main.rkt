#lang racket/base

(require (prefix-in b: racket/base)
         racket/contract
         racket/stream
         racket/match
         (prefix-in b: racket/generator)
         (only-in racket/generator
                  generator
                  yield)
         (only-in racket/function
                  const
                  curry
                  curryr
                  negate)
         racket/generic
         racket/undefined
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of)
         relation)

(require "private/util.rkt")

(provide gen:generator
         generator/c
         (contract-out
          [generator? (-> any/c boolean?)]
          [generator-state (-> generator? symbol?)]
          [in-producer (->* (generator?)
                            (any/c)
                            #:rest (listof any/c)
                            sequence?)]
          [generator-null (->* ()
                               (any/c)
                               generator?)]
          [generator-cons (-> any/c
                              generator?
                              generator?)]
          [make-generator (->* ()
                               #:rest (listof any/c)
                               generator?)]
          [generator-empty? (-> generator?
                                (values boolean? generator?))]
          [generator-done? (-> generator?
                               boolean?)]
          [generator-peek (-> generator?
                              (values any/c generator?))]
          [generator-map (-> (-> any/c any/c)
                             generator?
                             generator?)]
          [generator-filter (-> (-> any/c boolean?)
                                generator?
                                generator?)]
          [generator-fold (->* ((-> any/c any/c any/c) generator?)
                               (any/c #:order (one-of/c 'abb 'bab))
                               generator?)]
          [yield-from (-> generator? any)]
          [generator-append (-> generator? generator? generator?)]
          [generator-join (-> generator? generator?)]
          [generator-flatten (-> generator? generator?)]))

(module+ test
  (require rackunit
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

(define-generics generator
  (generator-state generator)
  #:fast-defaults ([b:generator?
                    (define generator-state b:generator-state)]))

(define (in-producer gen [stop undefined] . args)
  (let ([pred (if (undefined? stop)
                  (const #t)
                  (!! (curry = stop)))])
    (take-while pred
                (build-sequence (apply unthunk gen args)))))

(define (generator-null [return (void)])
  (generator ()
    return))

(define (generator-cons v gen)
  (generator ()
    (yield v)
    (let ([cur (gen)])
      (if (generator-done? gen)
          cur
          (let loop ([cur cur]
                     [next (gen)])
            (if (generator-done? gen)
                (begin (yield cur)
                       (gen))
                (begin (yield cur)
                       (loop next (gen)))))))))

(define (make-generator #:return [return (void)] . vals)
  (match vals
    ['() (generator-null return)]
    [(cons v vs) (generator-cons v (apply make-generator #:return return vs))]))

(define (generator-empty? gen)
  (if (generator-done? gen)
      (values #t gen)
      (let ([val (gen)])
        (if (generator-done? gen)
            (values #t
                    (generator () val))
            (values #f
                    (generator-cons val gen))))))

(define (generator-done? gen)
  (= (generator-state gen)
     'done))

(define (generator-peek gen)
  (if (generator-done? gen)
      (raise-argument-error 'generator-peek
                            "Non-empty generator in a non-terminal state."
                            gen)
      (let ([val (gen)])
        (if (generator-done? gen)
            (raise-argument-error 'generator-peek
                            "Non-empty generator in a non-terminal state."
                            gen)
            (values val
                    (generator-cons val gen))))))

(define (generator-map f gen)
  (generator ()
    (let loop ([cur (gen)]
               [next (gen)])
      (if (generator-done? gen)
          (begin (yield (f cur))
                 (let ([result (gen)])
                   (unless (void? result)
                     (f result))))
          (begin (yield (f cur))
                 (loop next (gen)))))))

(define (generator-filter pred gen)
  (generator ()
    (let loop ([cur (gen)]
               [next (gen)])
      (if (generator-done? gen)
          (begin (when (pred cur)
                   (yield cur))
                 (let ([result (gen)])
                   (unless (void? result)
                     (when (pred result)
                       result))))
          (begin (when (pred cur)
                   (yield cur))
                 (loop next (gen)))))))

(define (generator-fold f gen [base undefined] #:order [order 'abb])
  (generator ()
    (let-values ([(is-empty? gen) (generator-empty? gen)])
      (if is-empty?
          base
          (let-values ([(head gen) (generator-peek gen)])
            (let ([base (if (undefined? base)
                            ((id f) head)
                            base)]
                  [f (if (= order 'abb)
                         f
                         (flip f))])
              (let loop ([acc base]
                         [cur (gen)]
                         [next (gen)])
                (let ([acc (f cur acc)])
                  (if (generator-done? gen)
                      (begin (yield acc)
                             (let ([result (gen)])
                               (unless (void? result)
                                 (yield (f result acc)))))
                      (begin (yield acc)
                             (loop acc next (gen))))))))))))

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

(define (flatten-one-level vs)
  (if (sequence? vs)
      (for-each (λ (v)
                  (yield v)
                  (void))
                vs)
      (raise-argument-error 'flatten-one-level
                            "sequence?"
                            vs)))

(define (generator-join gen)
  (generator ()
    (let loop ([cur (gen)]
               [next (gen)])
      (if (generator-done? gen)
          (begin (flatten-one-level cur)
                 (let ([result (gen)])
                   (unless (void? result)
                     (flatten-one-level result))))
          (begin (flatten-one-level cur)
                 (loop next (gen)))))))

(define (generator-flatten gen)
  (let-values ([(is-empty? gen) (generator-empty? gen)])
    (if is-empty?
        gen
        (let-values ([(v gen) (generator-peek gen)])
          (if (sequence? v)
              (generator-flatten (generator-join gen))
              gen)))))

(module+ test
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
  (check-equal? (->list (generator () (yield-from (->generator (list 1 2 3))))) '(1 2 3))
  (check-equal? (->list (generator () (yield-from (make-generator)))) '())
  (check-equal? (->list (generator () (yield-from (make-generator 1)))) '(1))
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
