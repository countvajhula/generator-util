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
          [generator-map (self-map/c generator? (head any/c))]
          [generator-filter (self-map/c generator? (head any/c))]
          [generator-fold (->* (binary-function/c generator?)
                               (any/c #:order (one-of/c 'abb 'bab))
                               generator?)]
          [yield-from (function/c generator? any)]
          [generator-append (binary-composition/c generator?)]
          [generator-cycle (->* (generator?)
                                (any/c)
                                generator?)]
          [generator-repeat (-> any/c ... generator?)]
          [generator-zip-with (variadic-composition/c generator?
                                                      (head procedure?))]
          [generator-zip (variadic-composition/c generator?)]
          [generator-interleave (variadic-composition/c generator?)]
          [generator-join (self-map/c generator?)]
          [generator-flatten (self-map/c generator?)]))

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
    (let loop ([cur (call-with-values g list)]
               [next (call-with-values g list)])
      (let ([mapped-cur (call-with-values
                         (λ () (apply f cur))
                         list)])
        (if (generator-done? g)
            (begin (apply yield mapped-cur)
                   (let ([result (call-with-values g list)])
                     (unless (and (null? (cdr result))
                                  (void? (car result)))
                       (apply f result))))
            (begin (apply yield mapped-cur)
                   (loop next (call-with-values g list))))))))

(define (generator-filter pred g)
  (generator ()
    (let loop ([cur (call-with-values g list)]
               [next (call-with-values g list)])
      (if (generator-done? g)
          (begin (when (apply pred cur)
                   (apply yield cur))
                 (let ([result (call-with-values g list)])
                   (unless (and (null? (cdr result))
                                (void? (car result)))
                     (when (apply pred result)
                       (apply values result)))))
          (begin (when (apply pred cur)
                   (apply yield cur))
                 (loop next (call-with-values g list)))))))

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
      (call-with-values g
                        (λ vs
                          (if (generator-done? g)
                              (apply values vs)
                              (begin (apply yield vs)
                                     (yield-from g)))))))

(define (generator-append a b)
  (generator ()
    (yield-from a)
    (yield-from b)))

(define (generator-cycle g [stop (void)])
  (generator ()
    (let loop ([acc null])
      (let ([cur (call-with-values g list)])
        (unless (generator-done? g)
          (apply yield cur)
          (loop (cons cur acc))))
      (let ([acc (reverse acc)])
        (for ([cur (in-cycle acc)])
          (apply yield cur))))))

(define (generator-repeat . vs)
  (generator ()
    (let loop ()
      (apply yield vs)
      (loop))))

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
              (let ([cur (call-with-values first-g list)])
                (if (generator-done? first-g)
                    (apply values cur)
                    (begin (apply yield cur)
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
