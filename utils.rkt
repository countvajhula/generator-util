#lang racket/base

(require racket/contract
         racket/stream
         racket/match
         racket/generator
         (only-in racket/function
                  const)
         racket/generic
         racket/undefined
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of)
         core-utils
         collection-utils
         relation)

(provide gen:producer
         producer/c
         (contract-out
          [generator-collection? (-> any/c boolean?)]
          [generator-collection (-> generator? collection?)]
          [generator-collection-gen (-> generator-collection? generator?)]
          [producer? (-> any/c boolean?)]
          [producer-state (-> producer? symbol?)]
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
          [generator-append (-> generator? generator? generator?)]
          [generator-flatten (-> generator? generator?)]))

(define-generics producer
  (producer-state producer)
  #:fast-defaults ([generator?
                    (define producer-state generator-state)]))

(struct generator-collection (gen)
  #:transparent
  #:methods gen:collection
  [(define (conj st v)
     (let ([gen (generator-collection-gen st)])
       (generator-collection (generator-cons v gen))))]
  #:methods gen:producer
  [(define (producer-state st)
     (let ([gen (generator-collection-gen st)])
       (generator-state gen)))]
  #:property prop:procedure
  (λ (self . args)
    (let ([gen (generator-collection-gen self)])
      (apply gen args))))

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
      (if (= (generator-state gen)
             'done)
          cur
          (let loop ([cur cur]
                     [next (gen)])
            (if (= (generator-state gen)
                   'done)
                (begin (yield cur)
                       (gen))
                (begin (yield cur)
                       (loop next (gen)))))))))

(define (make-generator . vals)
  (match vals
    ['() (generator-null)]
    [(cons v vs) (generator-cons v (apply make-generator vs))]))

(define (generator-peek gen)
  (let ([val (gen)])
    (values val
            (generator-cons val gen))))

(define (generator-map f gen)
  (generator ()
    (let loop ([cur (gen)]
               [next (gen)])
      (if (= (generator-state gen)
             'done)
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
