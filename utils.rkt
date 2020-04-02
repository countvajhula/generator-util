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
                  ||
                  flip)
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
         in-producer
         empty-generator
         generator-cons
         generator-peek
         generator-map
         generator-filter
         generator-fold
         generator-append
         generator-split-where
         generator-flatten)

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

(define (in-producer gen [stop undefined] . args)
  (let ([pred (if (undefined? stop)
                  (const #t)
                  (!! (curry = stop)))])
    (take-while pred
                (build-sequence (apply unthunk gen args)))))

(define (empty-generator)
  (generator ()
    (void)))

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

(define (generator-peek gen)
  (let ([val (gen)])
    (values val
            (generator-cons val gen))))

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

(define (generator-fold f gen [base undefined] #:order [order 'abb])
  (generator ()
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
            (if (= (generator-state gen)
                   'done)
                (begin (yield acc)
                       (let ([result (gen)])
                         (unless (void? result)
                           (yield (f result acc)))))
                (begin (yield acc)
                       (loop acc next (gen))))))))))

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

(define (generator-split-where pred gen)
  (split-where pred
               (in-producer gen (void))))

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
