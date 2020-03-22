#lang racket/base

(require racket/contract
         racket/stream
         racket/match
         racket/generator
         racket/function
         racket/generic
         racket/undefined
         (except-in data/collection
                    foldl
                    foldl/steps
                    append)
         (only-in algebraic/prelude
                  &&
                  ||)
         functional-utils
         core-utils
         collection-utils
         relation)

(provide generator-collection
         generator-collection-gen
         generator-collection?
         gen:producer
         producer-state
         producer?
         generator-cons
         generator-append
         generator-splitf-at
         generator-map
         generator-filter
         generator-flatten
         in-producer)

(struct generator-collection (gen)
  #:transparent
  #:methods gen:collection
  [(define (conj st v)
     (let ([gen (generator-collection-gen st)])
       (generator-collection (generator-cons v gen))))]
  #:property prop:procedure
  (λ (self . args)
    (let ([gen (generator-collection-gen self)])
      (apply gen args))))

(define-generics producer
  (producer-state producer)
  #:fast-defaults ([generator?
                    (define producer-state generator-state)]))

(define (generator-cons v gen)
  (generator ()
    (yield v)
    (let loop ([cur (gen)]
               [next (gen)])
      (if (= (generator-state gen)
             'done)
          (begin (yield cur)
                 (gen))
          (begin (yield cur)
                 (loop next (gen)))))))

(define (generator-append a b)
  (generator ()
    (let loop ([cur (a)]
               [next (a)])
      (if (= (generator-state a)
             'done)
          (begin (yield cur)
                 (a))
          (begin (yield cur)
                 (loop next (a)))))
    (let loop ([cur (b)]
               [next (b)])
      (if (= (generator-state b)
             'done)
          (begin (yield cur)
                 (b))
          (begin (yield cur)
                 (loop next (b)))))))

(define (in-producer gen [stop undefined] . args)
  (let ([pred (if (undefined? stop)
                  (const #t)
                  (!! (curry = stop)))])
    (takef (build-sequence (apply unthunk gen args))
           pred)))

(define (generator-splitf-at gen pred)
  (splitf-at (in-producer gen (void))
             pred))

(define (generator-map pred gen)
  (generator ()
    (let loop ([cur (gen)]
               [next (gen)])
      (if (= (generator-state gen)
             'done)
          (begin (yield (pred cur))
                 (let ([result (gen)])
                   (unless (void? result)
                     (pred result))))
          (begin (yield (pred cur))
                 (loop next (gen)))))))

(define (generator-filter pred gen)
  (generator ()
    (let loop ([cur (gen)]
               [next (gen)])
      (if (= (generator-state gen)
             'done)
          (begin (when (pred cur)
                   (yield cur))
                 (let ([result (gen)])
                   (unless (void? result)
                     (when (pred result)
                       result))))
          (begin (when (pred cur)
                   (yield cur))
                 (loop next (gen)))))))

;; generator-flatten
(define (flatten-one-level vs)
  (for-each (λ (v)
              (yield v)
              (void))
            vs))

(define (generator-flatten gen)
  (generator ()
    (let loop ([cur (gen)]
               [next (gen)])
      (if (= (generator-state gen)
             'done)
          (begin (flatten-one-level cur)
                 (let ([result (gen)])
                   (unless (void? result)
                     (flatten-one-level result))))
          (begin (flatten-one-level cur)
                 (loop next (gen)))))))
